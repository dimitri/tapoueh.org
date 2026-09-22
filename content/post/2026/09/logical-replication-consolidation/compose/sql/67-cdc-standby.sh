# @nosync
# (no automatic wait: the standby steps call sync_all where needed)
# Step 4h (v): decode from a physical STANDBY of the warehouse so the CDC load stays off the primary.
SBC=("${DC[@]}")
sb()  { "${SBC[@]}" exec -T warehouse_standby psql -X -U postgres -d warehouse "$@" </dev/null 2>&1; }
sbx() { "${SBC[@]}" exec -T warehouse_standby sh -c "$1" </dev/null 2>&1; }
PGDATA_SB=/var/lib/postgresql/standby
COMMON='-c max_worker_processes=24 -c max_wal_senders=10 -c max_replication_slots=20 -c logical_decoding_work_mem=64kB'

echo '### 1. pg_basebackup (with -R: recovery settings; -C -S: physical slot on the primary)'
"${SBC[@]}" up -d --wait warehouse_standby 2>&1 | grep -v Container
sbx "PGPASSWORD=demo pg_basebackup -h warehouse -U postgres -D $PGDATA_SB -X stream -R -C -S standby1 -v"
sbx "grep primary_slot_name $PGDATA_SB/postgresql.auto.conf"

echo '### 2. first start attempt with default settings'
sbx "pg_ctl -D $PGDATA_SB -l /tmp/standby.log -w -t 20 start >/dev/null 2>&1; grep -E 'FATAL|DETAIL' /tmp/standby.log"

echo '### 3. start with the primary'"'"'s sizing settings (max_worker_processes etc.), wal_level left at the default'
sbx "rm -f /tmp/standby.log; pg_ctl -D $PGDATA_SB -l /tmp/standby.log -w -t 30 -o '$COMMON' start >/dev/null 2>&1"
sb -Atc "select 'in recovery: ' || pg_is_in_recovery()" -c "show wal_level" -c "show hot_standby_feedback"
echo '--- create the slot on the standby with pg_recvlogical:'
sbx "pg_recvlogical -U postgres -d warehouse --slot cdc_sb --create-slot -P test_decoding; echo exit=\$?"

echo '### 4. wal_level = logical is needed on the STANDBY too (it does not inherit it from the primary)'
sbx "pg_ctl -D $PGDATA_SB -l /tmp/standby.log -w -t 30 -o '$COMMON -c wal_level=logical' restart >/dev/null 2>&1"
sb -Atc "show wal_level"
echo '--- creating the slot now BLOCKS on an idle primary (it waits for a running-xacts record):'
"${SBC[@]}" exec -d warehouse_standby sh -c 'pg_recvlogical -U postgres -d warehouse --slot cdc_sb --create-slot -P test_decoding > /tmp/create.out 2>&1; echo "exit=$?" >> /tmp/create.out' </dev/null
wait_until warehouse_standby warehouse "select count(*) = 1 from pg_stat_activity where backend_type = 'walsender' and query like 'CREATE_REPLICATION_SLOT%'" 30
sb -Atc "select 'waiting for the snapshot, slots on standby: ' || count(*) from pg_replication_slots"
echo '--- unblock it from the PRIMARY: select pg_log_standby_snapshot()'
qt warehouse warehouse "select pg_log_standby_snapshot() is not null" >/dev/null
wait_file warehouse_standby /tmp/create.out 'exit=' 60
sbx "cat /tmp/create.out"
sb -c "select slot_name, plugin, active, conflicting from pg_replication_slots"

echo '### 5. hot_standby_feedback = off: the slot is invalidated when the primary vacuums catalog rows'
# (the primary'"'"'s own CDC slots hold back catalog cleanup and would mask this: drop them first)
sq warehouse warehouse -qAt -c "select count(pg_drop_replication_slot(slot_name)) from pg_replication_slots where slot_name in ('cdc_td','cdc_pg')" </dev/null >/dev/null
churn() {
  sq warehouse warehouse -qc "create table churn_a (i int)" -c "drop table churn_a" -c "create table churn_b (i int)" -c "drop table churn_b" -c "vacuum pg_catalog.pg_class" -c "vacuum pg_catalog.pg_attribute" </dev/null
  wait_until warehouse_standby warehouse "select pg_last_wal_replay_lsn() >= '$(qt warehouse warehouse 'select pg_current_wal_lsn()')'" 30
}
# psql on the standby service (wait_until uses qt, which uses the service name given)
churn
sb -c "select slot_name, wal_status, conflicting, invalidation_reason from pg_replication_slots"
sb -c "select count(*) from pg_logical_slot_peek_changes('cdc_sb', null, null)"

echo '### 6. hot_standby_feedback = on, fresh slot: the primary now keeps the catalog rows the standby slot needs'
sb -qc "alter system set hot_standby_feedback = on" -c "select pg_reload_conf()" >/dev/null
sb -qc "select pg_drop_replication_slot('cdc_sb')" >/dev/null
"${SBC[@]}" exec -d warehouse_standby sh -c 'pg_recvlogical -U postgres -d warehouse --slot cdc_sb --create-slot -P test_decoding > /tmp/create.out 2>&1; echo "exit=$?" >> /tmp/create.out' </dev/null
wait_until warehouse_standby warehouse "select count(*) = 1 from pg_stat_activity where backend_type = 'walsender' and query like 'CREATE_REPLICATION_SLOT%'" 30
qt warehouse warehouse "select pg_log_standby_snapshot() is not null" >/dev/null
wait_file warehouse_standby /tmp/create.out 'exit=' 60
sb -Atc "show hot_standby_feedback"
wait_until warehouse warehouse "select catalog_xmin is not null from pg_replication_slots where slot_name = 'standby1'" 60
sq warehouse warehouse -c "select slot_name, catalog_xmin is not null as primary_holds_catalog_xmin_for_standby from pg_replication_slots where slot_name = 'standby1'" </dev/null
churn
sb -c "select slot_name, wal_status, conflicting, invalidation_reason from pg_replication_slots"

echo '### 7. CDC from the standby: a change on shop travels shop -> warehouse primary -> standby -> slot'
sq shop shop -qc "insert into shop.orders (customer_id, amount, status, tenant) values (1, 77.00, 'via-standby', 'eu')" </dev/null
sync_all
end=$(qt warehouse warehouse "select pg_current_wal_lsn()")
i=0; until [ "$(sb -Atc "select pg_last_wal_replay_lsn() >= '$end'")" = t ]; do i=$((i+1)); [ $i -gt 150 ] && break; sleep 0.2; done
sbx "pg_recvlogical -U postgres -d warehouse --slot cdc_sb --start --endpos '$end' -f - -o include-xids=0 -o skip-empty-xacts=1 | grep -E 'via-standby|BEGIN|COMMIT' | tail -3"
echo '--- the primary has no logical slot for this consumer, the standby does:'
sq warehouse warehouse -c "select count(*) as logical_slots_on_primary from pg_replication_slots where slot_type = 'logical'" </dev/null
sb -c "select count(*) as logical_slots_on_standby from pg_replication_slots where slot_type = 'logical'"

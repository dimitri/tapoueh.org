# @nosync
# Step 4h (vi), beta territory: Postgres 19's effective_wal_level. A separate
# primary/standby pair (pg19_a/pg19_b), both left at every default, including
# wal_level = replica. Does a logical slot still work with nothing configured?
PC=("${DC[@]}")
pc() { "${PC[@]}" "$@"; }
a()  { sq pg19_a postgres "$@"; }
ax() { "${PC[@]}" exec -T pg19_a sh -c "$1" </dev/null 2>&1; }
b()  { sq pg19_b postgres "$@"; }
bx() { "${PC[@]}" exec -T pg19_b sh -c "$1" </dev/null 2>&1; }
PGDATA_B=/var/lib/postgresql/standby

echo '### 0. both servers, wal_level untouched (the default is replica)'
pc up -d --wait pg19_a pg19_b 2>&1 | grep -v Container
a -c "show wal_level" </dev/null
a -qc "create table public.t (id int)" </dev/null

echo '### 1. base backup pg19_b from pg19_a, start it as a standby, hot_standby_feedback on'
bx "PGPASSWORD=demo pg_basebackup -h pg19_a -U postgres -D $PGDATA_B -X stream -R -C -S standby1 -v" | grep -v '^pg_basebackup: w'
bx "pg_ctl -D $PGDATA_B -l /tmp/standby.log -w -t 30 -o '-c hot_standby_feedback=on' start"
b -c "select 'in recovery: ' || pg_is_in_recovery()" -c "show wal_level" </dev/null

echo '### 2. a logical slot directly on the standby, before the primary has any: refused'
bx "pg_recvlogical -U postgres -d postgres --slot probe_b --create-slot -P test_decoding; echo exit=\$?"

echo '### 3. one logical slot on the primary, nothing else changed'
a -c "select pg_create_logical_replication_slot('probe_a', 'test_decoding')" </dev/null
a -c "select name, setting from pg_settings where name in ('effective_wal_level','wal_level') order by name" </dev/null

echo '### 4. the same slot on the standby now succeeds, and its own effective_wal_level is logical too'
bx "pg_recvlogical -U postgres -d postgres --slot probe_b --create-slot -P test_decoding; echo exit=\$?"
b -c "select name, setting from pg_settings where name in ('effective_wal_level','wal_level') order by name" </dev/null

echo '### 5. a row on the primary, decoded from the standby'"'"'s own slot, once it has replayed'
a -qc "insert into public.t values (1)" </dev/null
end=$(qt pg19_a postgres "select pg_current_wal_lsn()")
i=0; until [ "$(qt pg19_b postgres "select pg_last_wal_replay_lsn() >= '$end'")" = t ]; do i=$((i+1)); [ "$i" -gt 150 ] && break; sleep 0.2; done
bx "pg_recvlogical -U postgres -d postgres --slot probe_b --start --endpos '$end' -f - -o include-xids=0 -o skip-empty-xacts=1"

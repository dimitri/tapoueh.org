# @service: upg
# Step 31 (part B): pg_upgrade --link of the SUBSCRIBER, 17 -> 18, while the
# publisher stays up and keeps taking writes.
. /work/lib.sh
cd /pgdata
echo "### 1. initdb of the new cluster with the 18 defaults, then pg_upgrade --check"
$B18/initdb -D /pgdata/s18 --auth=trust >/dev/null
$B17/pg_ctl -D /pgdata/s17 -m fast -w stop >/dev/null
$B18/pg_upgrade -b $B17 -B $B18 -d /pgdata/s17 -D /pgdata/s18 -p 5443 -P 5444 --link --check 2>&1 | tail -12
echo "[pg_upgrade --check exit status: ${PIPESTATUS[0]}]"

echo; echo "### 2. PG 18 turns data checksums on by default, 17 did not: re-create the target without"
rm -rf /pgdata/s18
$B18/initdb -D /pgdata/s18 --auth=trust --no-data-checksums >/dev/null
cat >> /pgdata/s18/postgresql.conf <<CONF
port = $S18
listen_addresses = 'localhost'
unix_socket_directories = '/tmp'
wal_level = logical
max_replication_slots = 10
max_wal_senders = 10
CONF

echo; echo "### 3. writes on the publisher while the subscriber is down"
pq $P17 app -qc "INSERT INTO items (v) SELECT 'while subscriber down ' || g FROM generate_series(1, 500) g"
qq $P17 "SELECT slot_name, active, confirmed_flush_lsn IS NOT NULL AS flushed FROM pg_replication_slots"

echo; echo "### 4. pg_upgrade --link (subscriber)"
$B18/pg_upgrade -b $B17 -B $B18 -d /pgdata/s17 -D /pgdata/s18 -p 5443 -P 5444 --link 2>&1 | grep -iE 'subscription|slot|origin|Upgrade Complete|Performing Upgrade|error|fatal|statistics' | head -20
echo "[pg_upgrade exit status: ${PIPESTATUS[0]}]"
echo "[start the new cluster with logical replication workers disabled, to look before anything runs]"
$B18/pg_ctl -D /pgdata/s18 -l /pgdata/s18.log -o "-c max_logical_replication_workers=0" -w start >/dev/null
echo; echo "### 5. what survived on the new subscriber (workers held back)"
qq $S18 "SELECT version()"
qq $S18 "SELECT subname, subenabled, subslotname, suborigin FROM pg_subscription"
qq $S18 "SELECT srrelid::regclass, srsubstate FROM pg_subscription_rel ORDER BY srrelid::regclass::text"
qq $S18 "SELECT external_id, remote_lsn IS NOT NULL AS has_remote_lsn FROM pg_replication_origin_status ORDER BY 1"
qq $S18 "SELECT count(*) AS rows_before_restart FROM items"

echo; echo "### 6. restart normally: nothing to enable, the subscription resumes from its saved position"
$B18/pg_ctl -D /pgdata/s18 -m fast -w stop >/dev/null   # (restart would keep the -o option)
$B18/pg_ctl -D /pgdata/s18 -l /pgdata/s18.log -w start >/dev/null
sleep 4
qq $S18 "SELECT count(*) AS rows_after_restart FROM items"
qq $P17 "SELECT slot_name, active FROM pg_replication_slots"
pq $P17 app -qc "INSERT INTO items (v) VALUES ('after subscriber upgrade')"; sleep 2
qq $S18 "SELECT count(*) AS rows_final, max(v) FILTER (WHERE v LIKE 'after%') AS last_row FROM items"
:

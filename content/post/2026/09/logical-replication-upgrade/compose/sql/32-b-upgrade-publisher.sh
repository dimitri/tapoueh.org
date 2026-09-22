# @service: upg
# Step 32 (part B): pg_upgrade --link of the PUBLISHER, 17 -> 18. The (already
# upgraded, 18) subscriber stays configured and running.
. /work/lib.sh
cd /pgdata
echo "### 1. pitfall: the publisher is shut down while its slot still has unconsumed changes"
$B18/pg_ctl -D /pgdata/s18 -m fast -w stop >/dev/null      # the subscriber is away...
pq $P17 app -qc "INSERT INTO items (v) VALUES ('not yet consumed')"   # ...so this stays in the slot
$B17/pg_ctl -D /pgdata/p17 -m fast -w stop >/dev/null
$B18/initdb -D /pgdata/p18 --auth=trust --no-data-checksums >/dev/null
cat >> /pgdata/p18/postgresql.conf <<CONF
port = $P18
listen_addresses = 'localhost'
unix_socket_directories = '/tmp'
max_wal_senders = 10
CONF
$B18/pg_upgrade -b $B17 -B $B18 -d /pgdata/p17 -D /pgdata/p18 -p 5443 -P 5444 --link --check 2>&1 | grep -vE ' ok$' | tail -8
echo "[pg_upgrade --check exit status: ${PIPESTATUS[0]}]"
echo "invalid_logical_slots.txt says:"; cat /pgdata/p18/pg_upgrade_output.d/*/invalid_logical_slots.txt
rm -rf /pgdata/p18/pg_upgrade_output.d

echo; echo "### 2. bring both back so the slot consumes everything, then shut the publisher down again"
$B17/pg_ctl -D /pgdata/p17 -l /pgdata/p17.log -w start >/dev/null
$B18/pg_ctl -D /pgdata/s18 -l /pgdata/s18.log -w start >/dev/null
sleep 6
qq $S18 "SELECT count(*) AS rows_on_subscriber FROM items"
qq $P17 "SELECT slot_name, active FROM pg_replication_slots"
$B17/pg_ctl -D /pgdata/p17 -m fast -w stop >/dev/null
echo "new cluster still WITHOUT wal_level = logical: pg_upgrade --check"
$B18/pg_upgrade -b $B17 -B $B18 -d /pgdata/p17 -D /pgdata/p18 -p 5443 -P 5444 --link --check 2>&1 | grep -vE ' ok$' | tail -8
echo "[pg_upgrade --check exit status: ${PIPESTATUS[0]}]"

echo; echo "### 3. set wal_level = logical and max_replication_slots on the new cluster, then upgrade"
cat >> /pgdata/p18/postgresql.conf <<CONF
wal_level = logical
max_replication_slots = 10
CONF
$B18/pg_upgrade -b $B17 -B $B18 -d /pgdata/p17 -D /pgdata/p18 -p 5443 -P 5444 --link 2>&1 | grep -iE 'slot|Upgrade Complete|Performing Upgrade|error|fatal|statistics' | head -20
echo "[pg_upgrade exit status: ${PIPESTATUS[0]}]"
$B18/pg_ctl -D /pgdata/p18 -l /pgdata/p18.log -w start >/dev/null

echo; echo "### 4. what survived on the new publisher"
qq $P18 "SELECT version()"
qq $P18 "SELECT slot_name, plugin, slot_type, active, confirmed_flush_lsn IS NOT NULL AS flushed, failover FROM pg_replication_slots"
qq $P18 "SELECT pubname, puballtables FROM pg_publication"
qq $P18 "SELECT last_value FROM pg_sequences WHERE sequencename = 'standalone_seq'"
echo "the subscriber reconnects by itself (retry interval 5 s)"
pq $P18 app -qc "INSERT INTO items (v) VALUES ('after publisher upgrade')"
sleep 8
qq $P18 "SELECT slot_name, active FROM pg_replication_slots"
qq $S18 "SELECT count(*) AS rows_on_subscriber, max(v) FILTER (WHERE v LIKE 'after publisher%') AS last_row FROM items"
qq $P18 "SELECT count(*) AS rows_on_publisher FROM items"
qq $S18 "SELECT pid IS NOT NULL AS apply_running FROM pg_stat_subscription WHERE subname = 'sub'"
echo; echo "### 5. left to do by hand"
qq $P18 "SELECT relname, n_live_tup, last_analyze IS NOT NULL AS analyzed FROM pg_stat_user_tables ORDER BY 1"
$B18/pg_ctl -D /pgdata/p18 -m fast -w stop >/dev/null; $B18/pg_ctl -D /pgdata/s18 -m fast -w stop >/dev/null
:

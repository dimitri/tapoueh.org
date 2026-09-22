# @service: upg
# Step 30 (part B): two PostgreSQL 17 clusters inside the helper container:
# a PUBLISHER (port 5441) and a SUBSCRIBER (port 5442) with a live subscription.
. /work/lib.sh
set -e
rm -rf /pgdata/* /tmp/.s.PGSQL.* 2>/dev/null || true
$B17/postgres --version; $B18/postgres --version
for n in p17 s17; do
  $B17/initdb -D /pgdata/$n --auth=trust >/dev/null
  port=$([ $n = p17 ] && echo $P17 || echo $S17)
  cat >> /pgdata/$n/postgresql.conf <<CONF
port = $port
listen_addresses = 'localhost'
unix_socket_directories = '/tmp'
wal_level = logical
max_replication_slots = 10
max_wal_senders = 10
CONF
  $B17/pg_ctl -D /pgdata/$n -l /pgdata/$n.log -w start >/dev/null
done
echo "data checksums (17, initdb default): $(pq $P17 postgres -qAtc 'SHOW data_checksums')"

pq $P17 postgres -qc "CREATE DATABASE app"; pq $S17 postgres -qc "CREATE DATABASE app"
DDL="CREATE TABLE items (id bigint GENERATED ALWAYS AS IDENTITY PRIMARY KEY, v text);
     CREATE SEQUENCE standalone_seq START 500;
     CREATE TABLE gap (id int PRIMARY KEY)"
pq $P17 app -qc "$DDL"; pq $S17 app -qc "$DDL"
pq $P17 app -qc "INSERT INTO items (v) SELECT 'row ' || g FROM generate_series(1, 1000) g; SELECT nextval('standalone_seq') FROM generate_series(1, 7); CREATE PUBLICATION pub FOR ALL TABLES" >/dev/null
qq $S17 "CREATE SUBSCRIPTION sub CONNECTION 'host=localhost port=$P17 dbname=app' PUBLICATION pub"
sleep 3
echo; echo "### 17 before the upgrade"
qq $S17 "SELECT count(*) FROM items"
qq $S17 "SELECT subname, subenabled, subslotname FROM pg_subscription"
qq $S17 "SELECT srrelid::regclass, srsubstate FROM pg_subscription_rel ORDER BY srrelid::regclass::text"
qq $S17 "SELECT external_id, remote_lsn IS NOT NULL AS has_remote_lsn FROM pg_replication_origin_status ORDER BY 1"
qq $P17 "SELECT slot_name, plugin, active, confirmed_flush_lsn IS NOT NULL AS flushed FROM pg_replication_slots"
:

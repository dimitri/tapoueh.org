# @service: pg19a
# Step 44 (part C, 19 BETA): effective_wal_level. pg19c runs with the DEFAULT
# wal_level = replica and has never been restarted since.
. /work/lib.sh
C() { q pg19c "$1" "$2"; }; B() { q pg19b "$1" "$2"; }
C postgres "SHOW wal_level"
C postgres "SHOW effective_wal_level"
C postgres "SELECT name, context FROM pg_settings WHERE name IN ('wal_level', 'effective_wal_level') ORDER BY 1"
qa pg19c postgres "CREATE DATABASE app" >/dev/null
qa pg19c app "CREATE TABLE t (id int PRIMARY KEY); INSERT INTO t VALUES (1), (2)" >/dev/null
echo; echo "### a publication needs nothing special"
C app "CREATE PUBLICATION p FOR ALL TABLES"
C app "SHOW effective_wal_level"
echo; echo "### creating the first logical slot switches logical decoding on, no restart"
C app "SELECT slot_name FROM pg_create_logical_replication_slot('s1', 'pgoutput')"
C app "SHOW effective_wal_level"
C app "SHOW wal_level"
C app "SELECT slot_name, slot_type FROM pg_replication_slots"
echo; echo "### dropping the last logical slot switches it off again"
C app "SELECT pg_drop_replication_slot('s1')"
sleep 3
C app "SHOW effective_wal_level"
echo; echo "### the same through CREATE SUBSCRIPTION from another server"
qa pg19b postgres "CREATE DATABASE walsub" >/dev/null
qa pg19b walsub "CREATE TABLE t (id int PRIMARY KEY)" >/dev/null
B walsub "CREATE SUBSCRIPTION s_wal CONNECTION 'host=pg19c dbname=app user=postgres password=postgres' PUBLICATION p"
for i in $(seq 1 60); do [ "$(qa pg19b walsub "SELECT count(*) FROM pg_subscription_rel WHERE srsubstate <> 'r'")" = 0 ] && break; sleep 0.5; done
B walsub "SELECT count(*) AS rows_copied FROM t"
C app "SHOW effective_wal_level"
B walsub "DROP SUBSCRIPTION s_wal"
sleep 3
C app "SHOW effective_wal_level"
:

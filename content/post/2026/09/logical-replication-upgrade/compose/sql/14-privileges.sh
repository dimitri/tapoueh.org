# @service: traffic
# Step 14: does CREATE SUBSCRIPTION (with copy_data) need a superuser on the new
# side? Tested in a scratch database so the real migration is not disturbed.
. /work/lib.sh
CONN='host=old dbname=scratch user=repl password=repl'
qa old postgres "CREATE DATABASE scratch" >/dev/null
qa new postgres "CREATE DATABASE scratch" >/dev/null
qa old scratch "CREATE TABLE t (id int PRIMARY KEY, v text); INSERT INTO t SELECT g, 'row' || g FROM generate_series(1,3) g; CREATE PUBLICATION p_scratch FOR TABLE t" >/dev/null
qa new scratch "CREATE TABLE t (id int PRIMARY KEY, v text)" >/dev/null
qa new postgres "CREATE ROLE migrator LOGIN PASSWORD 'migrator'" >/dev/null
qa new postgres "GRANT pg_create_subscription TO migrator" >/dev/null
mig() { echo "migrator@new> $1"; PGUSER=migrator PGPASSWORD=migrator psql -X -h new -d scratch -c "$1" 2>&1; }

echo "### 1. pg_create_subscription alone"
mig "CREATE SUBSCRIPTION s_scratch CONNECTION '$CONN' PUBLICATION p_scratch"
echo; echo "### 2. + CREATE on the database"
qa new postgres "GRANT CREATE ON DATABASE scratch TO migrator"
mig "CREATE SUBSCRIPTION s_scratch CONNECTION 'host=old dbname=scratch user=repl' PUBLICATION p_scratch"
mig "CREATE SUBSCRIPTION s_scratch CONNECTION '$CONN' PUBLICATION p_scratch"
sleep 6
echo; echo "### state after 6s: tablesync cannot write into a table migrator does not own"
q new scratch "SELECT srrelid::regclass, srsubstate FROM pg_subscription_rel"
q new scratch "SELECT subname, subowner::regrole, subrunasowner, subpasswordrequired FROM pg_subscription"
q new scratch "SELECT count(*) FROM t"
docker_log() { :; }
echo; echo "### 3. give migrator the table (ownership) -> the sync completes"
qa new scratch "ALTER TABLE t OWNER TO migrator"
sleep 8
q new scratch "SELECT srrelid::regclass, srsubstate FROM pg_subscription_rel"
q new scratch "SELECT * FROM t ORDER BY 1"
echo; echo "### cleanup of the scratch subscription (drops the slot on old)"
mig "DROP SUBSCRIPTION s_scratch"
q old postgres "SELECT slot_name FROM pg_replication_slots"
:

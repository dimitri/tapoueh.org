# @service: traffic
# Step 21: ROLLBACK PATH, prepared BEFORE the cutover: a subscription on the OLD
# server that pulls from the NEW one, with origin = none and copy_data = false.
. /work/lib.sh
echo "### 0. the schema dump of step 13 already created a FOR ALL TABLES publication on new"
q new app "SELECT pubname, puballtables FROM pg_publication"

echo; echo "### 1. the forward subscription must stop forwarding what it did not originate"
q new app "SELECT subname, suborigin FROM pg_subscription"
q new app "ALTER SUBSCRIPTION app_sub SET (origin = none)"
q new app "SELECT subname, suborigin FROM pg_subscription"

echo; echo "### 2. reverse subscription on old: no initial copy, only changes made locally on new"
q old app "CREATE SUBSCRIPTION app_rev CONNECTION 'host=new dbname=app user=repl password=repl' PUBLICATION app_pub WITH (copy_data = false, origin = none)"
sleep 3
q old app "SELECT subname, suborigin, subenabled FROM pg_subscription"
q old app "SELECT srrelid::regclass AS tbl, srsubstate FROM pg_subscription_rel ORDER BY srrelid::regclass::text"

echo; echo "### 3. probe both directions, and prove nothing bounces back"
q new app "INSERT INTO audit_log (event, payload) VALUES ('probe-from-new', 'x')"
q old app "INSERT INTO audit_log (event, payload) VALUES ('probe-from-old', 'x')"
sleep 3
for h in old new; do q $h app "SELECT event, count(*) FROM audit_log WHERE event LIKE 'probe-%' GROUP BY 1 ORDER BY 1"; done
q new app "DELETE FROM audit_log WHERE event LIKE 'probe-%'"
sleep 3
for h in old new; do q $h app "SELECT count(*) AS probes_left FROM audit_log WHERE event LIKE 'probe-%'"; done
q old app "SELECT slot_name, active FROM pg_replication_slots ORDER BY 1"
q new app "SELECT slot_name, active FROM pg_replication_slots ORDER BY 1"

echo; echo "### 4. pitfall: reverse subscription WITH copy_data = true (scratch database)"
qa new scratch "CREATE PUBLICATION p_new_scratch FOR TABLE t" >/dev/null
q old scratch "CREATE SUBSCRIPTION s_rev_bad CONNECTION 'host=new dbname=scratch user=repl password=repl' PUBLICATION p_new_scratch WITH (copy_data = true, origin = none)"
sleep 6
q old scratch "SELECT srrelid::regclass, srsubstate FROM pg_subscription_rel"
q old scratch "DROP SUBSCRIPTION s_rev_bad"
:

# @service: pg19a
# Step 43 (part C, 19 BETA): subscription option retain_dead_tuples and the
# update_deleted conflict. pg19b (subscriber) runs with track_commit_timestamp = on.
# Scenario: the publisher UPDATEs a row, the subscriber DELETEs it locally
# LATER, then the update is applied (the subscription is disabled in between so
# the order is deterministic).
. /work/lib.sh
A() { q pg19a "$1" "$2"; }; B() { q pg19b "$1" "$2"; }
qa pg19a postgres "CREATE DATABASE rdt" >/dev/null; qa pg19b postgres "CREATE DATABASE rdt" >/dev/null
DDL="CREATE TABLE acct (id int PRIMARY KEY, bal int); CREATE TABLE acct_plain (id int PRIMARY KEY, bal int)"
qa pg19a rdt "$DDL" >/dev/null; qa pg19b rdt "$DDL" >/dev/null
qa pg19a rdt "INSERT INTO acct VALUES (1, 10), (2, 20); INSERT INTO acct_plain VALUES (1, 10), (2, 20);
               CREATE PUBLICATION p_rdt FOR TABLE acct; CREATE PUBLICATION p_plain FOR TABLE acct_plain" >/dev/null
B rdt "SHOW track_commit_timestamp"
echo "### without track_commit_timestamp the option is accepted with warnings (pg19a has it off)"
A rdt "CREATE SUBSCRIPTION s_refused CONNECTION 'host=pg19a dbname=rdt user=postgres password=postgres' PUBLICATION p_rdt WITH (retain_dead_tuples = true, connect = false)"
A rdt "ALTER SUBSCRIPTION s_refused SET (slot_name = NONE)"; A rdt "DROP SUBSCRIPTION s_refused"
echo; echo "### subscriber with retain_dead_tuples (and a 60 s cap on how long it may hold back cleanup on the publisher)"
B rdt "CREATE SUBSCRIPTION s_rdt CONNECTION 'host=pg19a dbname=rdt user=postgres password=postgres' PUBLICATION p_rdt WITH (retain_dead_tuples = true, max_retention_duration = 60000)"
B rdt "CREATE SUBSCRIPTION s_plain CONNECTION 'host=pg19a dbname=rdt user=postgres password=postgres' PUBLICATION p_plain"
for i in $(seq 1 60); do [ "$(qa pg19b rdt "SELECT count(*) FROM pg_subscription_rel WHERE srsubstate <> 'r'")" = 0 ] && break; sleep 0.5; done
B rdt "SELECT subname, subretaindeadtuples, submaxretention, subretentionactive FROM pg_subscription ORDER BY 1"
B rdt "SELECT slot_name, slot_type, xmin IS NOT NULL AS holds_xmin, active FROM pg_replication_slots"
echo; echo "### provoke: update on the publisher, delete on the subscriber, then apply"
B rdt "ALTER SUBSCRIPTION s_rdt DISABLE"
B rdt "ALTER SUBSCRIPTION s_plain DISABLE"
sleep 1
A rdt "UPDATE acct SET bal = 11 WHERE id = 1"
A rdt "UPDATE acct_plain SET bal = 11 WHERE id = 1"
sleep 1
B rdt "DELETE FROM acct WHERE id = 1"
B rdt "DELETE FROM acct_plain WHERE id = 1"
B rdt "ALTER SUBSCRIPTION s_rdt ENABLE"
B rdt "ALTER SUBSCRIPTION s_plain ENABLE"
sleep 6
B rdt "SELECT subname, confl_update_deleted, confl_update_missing FROM pg_stat_subscription_stats ORDER BY 1"
B rdt "SELECT * FROM acct ORDER BY 1"
:

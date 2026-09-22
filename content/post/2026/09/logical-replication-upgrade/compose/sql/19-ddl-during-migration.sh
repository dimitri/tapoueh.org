# @service: traffic
# Step 19: what happens when the application team ships DDL on the old server
# while the migration is running? (publication is FOR ALL TABLES)
. /work/lib.sh
errs() { qa new app "SELECT apply_error_count FROM pg_stat_subscription_stats WHERE subname = 'app_sub'"; }
wait_err() { for i in $(seq 1 60); do [ "$(errs)" -gt "$1" ] && return; sleep 0.5; done; echo "no apply error seen"; }
lag_bytes() { # bytes the subscriber is behind the old primary
  local t; t=$(qa old app "SELECT pg_current_wal_lsn()")
  qa new app "SELECT pg_wal_lsn_diff('$t', remote_lsn) FROM pg_replication_origin_status WHERE external_id = 'pg_' || (SELECT oid FROM pg_subscription WHERE subname = 'app_sub')"; }
wait_caught_up() { for i in $(seq 1 60); do [ "$(lag_bytes | cut -d. -f1)" -lt 200000 ] && return; sleep 0.5; done; }

echo "### (a) ALTER TABLE ... ADD COLUMN on old only"
q old app "ALTER TABLE customers ADD COLUMN note text"
e0=$(errs); wait_err "$e0"
echo "[poll] apply errors so far: $(errs)"
q new app "SELECT subname, apply_error_count FROM pg_stat_subscription_stats"
echo "the same DDL on new repairs it; the apply worker retries by itself"
q new app "ALTER TABLE customers ADD COLUMN note text"
wait_caught_up
echo "[poll] lag now: $(lag_bytes) bytes"
echo "caught up: $([ "$(lag_bytes | cut -d. -f1)" -lt 200000 ] && echo yes || echo NO)"

echo; echo "### (b) CREATE TABLE on old only: FOR ALL TABLES publishes it immediately"
q old app "CREATE TABLE late_table (id int PRIMARY KEY, v text)"
q old app "INSERT INTO late_table VALUES (1, 'created during the migration')"
q old app "SELECT tablename FROM pg_publication_tables WHERE tablename = 'late_table'"
e0=$(errs); wait_err "$e0"
echo "[poll] apply errors so far: $(errs)"
q new app "SELECT subname, apply_error_count FROM pg_stat_subscription_stats"
echo "create the table on new: does the row show up by itself?"
q new app "CREATE TABLE late_table (id int PRIMARY KEY, v text)"
sleep 8
q new app "SELECT count(*) AS rows_on_new FROM late_table"
q new app "SELECT srrelid::regclass, srsubstate FROM pg_subscription_rel WHERE srrelid = 'late_table'::regclass"
echo "no: the table is not in pg_subscription_rel yet. REFRESH PUBLICATION registers it and copies its rows."
q new app "ALTER SUBSCRIPTION app_sub REFRESH PUBLICATION"
sleep 6
q new app "SELECT srrelid::regclass, srsubstate FROM pg_subscription_rel WHERE srrelid = 'late_table'::regclass"
q new app "SELECT * FROM late_table"
wait_caught_up
echo "[poll] lag now: $(lag_bytes) bytes"
:

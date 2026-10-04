-- @service: pghub
-- Step 7f (optional: ./run.sh pglogical-crash). What happens if the hub table that pglogical writes
-- into is the PARTITIONED PARENT (as with core logical replication in step 2b).
-- Expected on pglogical 2.4.8 / PostgreSQL 14.24: the initial copy works, then the apply worker
-- crashes with SIGSEGV on the first replicated INSERT, the postmaster restarts every backend, and the
-- subscription crash-loops until dropped. This session is killed by the crash: that is the result.
CREATE TABLE ue_crash (worker_id int NOT NULL, event_id bigint NOT NULL, qty int NOT NULL, PRIMARY KEY (worker_id, event_id))
  PARTITION BY LIST (worker_id);
CREATE TABLE ue_crash_w1 PARTITION OF ue_crash FOR VALUES IN (1);
SELECT lr.on('pgw1', 'CREATE TABLE ue_crash (worker_id int NOT NULL DEFAULT 1, event_id bigint NOT NULL, qty int NOT NULL, PRIMARY KEY (worker_id, event_id))');
SELECT lr.on('pgw1', 'INSERT INTO ue_crash (event_id, qty) VALUES (1, 1)');
SELECT * FROM dblink(lr.conn('pgw1'), $$SELECT pglogical.create_replication_set('crash')$$) t(x oid);
SELECT * FROM dblink(lr.conn('pgw1'), $$SELECT pglogical.replication_set_add_table('crash', 'ue_crash')$$) t(x bool);
SELECT pglogical.create_subscription(subscription_name := 'sub_crash',
  provider_dsn := 'host=pgw1 dbname=app user=postgres', replication_sets := ARRAY['crash'],
  synchronize_data := true, forward_origins := '{}') IS NOT NULL AS created;
SELECT pglogical.wait_for_subscription_sync_complete('sub_crash');
SELECT count(*) AS rows_copied_by_initial_sync FROM ue_crash;
SELECT lr.on('pgw1', 'INSERT INTO ue_crash (event_id, qty) VALUES (2, 2)');
SELECT lr.wait_for($$SELECT count(*) = 2 FROM ue_crash$$, 30);

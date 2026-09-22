-- @service: hub
-- Step 4g. Replication lag, subscriber side (the hub): pg_stat_subscription, then a lag we cause
-- on purpose by disabling sub_usage_w3 while worker3 keeps writing.
SELECT subname, worker_type, pid IS NOT NULL AS running, received_lsn = latest_end_lsn AS received_eq_latest_end
FROM pg_stat_subscription WHERE subname LIKE 'sub_usage%' AND worker_type = 'apply' ORDER BY 1;

ALTER SUBSCRIPTION sub_usage_w3 DISABLE;
SELECT lr.wait_for($$SELECT NOT active FROM dblink(lr.conn('worker3'), $q$SELECT active FROM pg_replication_slots
  WHERE slot_name = 'sub_usage_w3'$q$) t(active bool)$$);
SELECT lr.on('worker3', $$INSERT INTO usage_events (customer_id, meter, qty)
  SELECT 7, 'lag-test', g FROM generate_series(1, 5000) g$$);
-- from the hub: pid is gone; from worker3: slot inactive, WAL retained for it
SELECT subname, pid IS NOT NULL AS running FROM pg_stat_subscription WHERE subname = 'sub_usage_w3';
SELECT slot_name, active, retained_bytes > 100000 AS retains_wal, wal_status
FROM dblink(lr.conn('worker3'), $q$SELECT slot_name, active,
  pg_wal_lsn_diff(pg_current_wal_lsn(), restart_lsn)::bigint, wal_status FROM pg_replication_slots
  WHERE slot_name = 'sub_usage_w3'$q$) t(slot_name text, active bool, retained_bytes bigint, wal_status text);
SELECT count(*) AS lag_test_rows_on_hub FROM usage_events WHERE meter = 'lag-test';

ALTER SUBSCRIPTION sub_usage_w3 ENABLE;
SELECT lr.wait_for($$SELECT count(*) = 5000 FROM usage_events WHERE meter = 'lag-test'$$);
SELECT count(*) AS lag_test_rows_on_hub FROM usage_events WHERE meter = 'lag-test';

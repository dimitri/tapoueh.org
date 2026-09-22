-- @service: worker1 worker2 worker3
-- Step 4f. Replication lag, publisher side: on a worker, pg_stat_replication has one row per
-- hub subscription (application_name = subscription name), pg_replication_slots the retained WAL.
SELECT lr.wait_caught_up();
SELECT application_name, state, sync_state,
       pg_wal_lsn_diff(pg_current_wal_lsn(), replay_lsn) < 1024 * 1024 AS replay_within_1MB
FROM pg_stat_replication ORDER BY 1;
SELECT slot_name, slot_type, active, wal_status,
       pg_wal_lsn_diff(pg_current_wal_lsn(), confirmed_flush_lsn) < 1024 * 1024 AS confirmed_within_1MB
FROM pg_replication_slots ORDER BY 1;

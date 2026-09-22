-- @service: hub
-- Step 4e. Conflict counters (PG18) in pg_stat_subscription_stats. Exact column names:
SELECT column_name FROM information_schema.columns
WHERE table_schema = 'pg_catalog' AND table_name = 'pg_stat_subscription_stats' ORDER BY ordinal_position;

-- Provoke non-fatal conflicts on the hub table fed by sub_usage_w1. Three fresh events on
-- worker1 (meter 'conflict-test'), replicated to the hub first:
SELECT lr.on('worker1', $$INSERT INTO usage_events (customer_id, meter, qty)
  SELECT 1, 'conflict-test', g FROM generate_series(1, 3) g$$);
SELECT lr.wait_for($$SELECT count(*) = 3 FROM usage_events WHERE meter = 'conflict-test'$$);

-- (1) update_missing: the hub deleted the row locally, then worker1 updates it.
DELETE FROM usage_events WHERE meter = 'conflict-test' AND qty = 1;
SELECT lr.on('worker1', $$UPDATE usage_events SET qty = 100 WHERE meter = 'conflict-test' AND qty = 1$$);
-- (2) delete_missing: same with a DELETE
DELETE FROM usage_events WHERE meter = 'conflict-test' AND qty = 2;
SELECT lr.on('worker1', $$DELETE FROM usage_events WHERE meter = 'conflict-test' AND qty = 2$$);
-- (3) update_origin_differs: the hub modifies the row itself, then worker1 updates it too
UPDATE usage_events SET qty = 300 WHERE meter = 'conflict-test' AND qty = 3;
SELECT lr.on('worker1', $$UPDATE usage_events SET qty = 301 WHERE meter = 'conflict-test' AND qty = 3$$);

SELECT lr.wait_for($$SELECT confl_update_missing >= 1 AND confl_delete_missing >= 1 AND confl_update_origin_differs >= 1
                    FROM pg_stat_subscription_stats WHERE subname = 'sub_usage_w1'$$);
SELECT subname, apply_error_count, confl_insert_exists, confl_update_origin_differs, confl_update_exists,
       confl_update_missing, confl_delete_origin_differs, confl_delete_missing, confl_multiple_unique_conflicts
FROM pg_stat_subscription_stats WHERE subname = 'sub_usage_w1';
-- and the stalled sub_naive_w2 of step 2c (skipped in 4d): its errors were all insert_exists
-- (the exact number depends on how long the stall lasted, so only compare)
SELECT subname, apply_error_count >= 2 AS errored_repeatedly, apply_error_count = confl_insert_exists AS all_errors_were_insert_exists
FROM pg_stat_subscription_stats WHERE subname = 'sub_naive_w2';

-- None of these stopped the apply worker (only insert_exists / update_exists /
-- multiple_unique_conflicts raise an ERROR). What was decided:
SELECT worker_id, meter, qty FROM usage_events WHERE meter = 'conflict-test' ORDER BY qty;  -- update_origin_differs: applied

WITH x AS (
  SELECT lr.norm(l) AS line, row_number() OVER () AS n
  FROM lr.log_lines('conflict=(update_missing|delete_missing|update_origin_differs)|Could not find|Updating the row|Deleting the row') l)
SELECT line FROM (SELECT line, min(n) AS mn FROM x GROUP BY line) s ORDER BY mn;

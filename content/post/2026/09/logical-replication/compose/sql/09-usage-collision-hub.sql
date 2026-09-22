-- @service: hub
-- Step 2c. Why worker_id must be part of the key. The hub gets a second table whose key
-- is the per-node identity alone. The workers' inserts are issued through dblink so that
-- the sequence of events stays in one file: lr.on(node, sql) runs sql on that node.
CREATE TABLE usage_naive (
  event_id    bigint PRIMARY KEY,
  customer_id int NOT NULL,
  qty         int NOT NULL
);
SELECT format($f$CREATE SUBSCRIPTION sub_naive_w%1$s
  CONNECTION 'host=worker%1$s dbname=app user=postgres' PUBLICATION pub_naive$f$, n)
FROM generate_series(1, 3) n \gexec
SELECT lr.wait_for($$SELECT count(*) = 3 FROM pg_subscription_rel r JOIN pg_subscription s ON s.oid = r.srsubid
                    WHERE s.subname LIKE 'sub_naive%' AND r.srsubstate = 'r'$$);

-- worker1 writes 3 events: event_id 1, 2, 3
SELECT lr.on('worker1', 'INSERT INTO usage_naive (customer_id, qty) SELECT 1, g FROM generate_series(1, 3) g');
SELECT lr.wait_for($$SELECT count(*) = 3 FROM usage_naive$$);

-- worker2 writes 3 events too: its identity also gives event_id 1, 2, 3
SELECT lr.on('worker2', 'INSERT INTO usage_naive (customer_id, qty) SELECT 4, g FROM generate_series(1, 3) g');

-- The apply worker of sub_naive_w2 hits the duplicate key, raises an ERROR, exits,
-- and is restarted (every wal_retrieve_retry_interval, 1s here) to fail again.
SELECT lr.wait_for($$SELECT apply_error_count >= 2 FROM pg_stat_subscription_stats WHERE subname = 'sub_naive_w2'$$);
SELECT subname, apply_error_count >= 2 AS retried_at_least_twice, sync_error_count
FROM pg_stat_subscription_stats WHERE subname LIKE 'sub_naive%' ORDER BY 1;
SELECT subname, pid IS NOT NULL AS worker_running FROM pg_stat_subscription
WHERE subname LIKE 'sub_naive%' ORDER BY 1;
-- Only w2 is stuck: the other subscriptions and sub_usage_* keep flowing.
SELECT count(*) AS rows_on_hub, min(customer_id) AS only_from_customer FROM usage_naive;

-- What the server log says (deduplicated: one copy of the message per failed attempt;
-- LSNs, xids and timestamps normalised by lr.norm):
WITH x AS (
  SELECT lr.norm(l) AS line, row_number() OVER () AS n
  FROM lr.log_lines('conflict detected|^[[:space:]]+Key|DETAIL.*usage_naive|CONTEXT.*usage_naive') l)
SELECT line FROM (SELECT line, min(n) AS mn FROM x GROUP BY line) s ORDER BY mn;

-- @service: hub
-- Step 4c. Hub side, part 2: a new partition, then the subscription.
-- First note the running apply workers, to prove later they were not disturbed.
SELECT string_agg(pid::text, ',' ORDER BY subname) AS pids_before
FROM pg_stat_subscription WHERE subname LIKE 'sub_usage_w%' AND worker_type = 'apply' \gset

-- Which lock does the parent table take? Check in a transaction we roll back.
CREATE TABLE usage_events_w4 (LIKE usage_events INCLUDING ALL);
ALTER TABLE usage_events_w4 ADD CONSTRAINT w4_only CHECK (worker_id = 4);
BEGIN;
ALTER TABLE usage_events ATTACH PARTITION usage_events_w4 FOR VALUES IN (4);
SELECT mode FROM pg_locks WHERE relation = 'usage_events'::regclass AND pid = pg_backend_pid() AND locktype = 'relation' ORDER BY 1;
ROLLBACK;
BEGIN;
CREATE TABLE usage_events_wx PARTITION OF usage_events FOR VALUES IN (99);
SELECT mode FROM pg_locks WHERE relation = 'usage_events'::regclass AND pid = pg_backend_pid() AND locktype = 'relation' ORDER BY 1;
ROLLBACK;

ALTER TABLE usage_events ATTACH PARTITION usage_events_w4 FOR VALUES IN (4);
CREATE SUBSCRIPTION sub_usage_w4
  CONNECTION 'host=worker4 dbname=app user=postgres' PUBLICATION pub_usage;
SELECT lr.wait_for($$SELECT count(*) = 4 FROM usage_events WHERE worker_id = 4$$);

-- The three existing subscriptions were not touched: same apply worker processes.
SELECT string_agg(pid::text, ',' ORDER BY subname) = :'pids_before' AS same_apply_workers_as_before
FROM pg_stat_subscription WHERE subname IN ('sub_usage_w1', 'sub_usage_w2', 'sub_usage_w3') AND worker_type = 'apply';

-- and events keep flowing from old and new workers
SELECT lr.on('worker1', $$INSERT INTO usage_events (customer_id, meter, qty) VALUES (1, 'api_calls', 1)$$);
SELECT lr.on('worker4', $$INSERT INTO usage_events (customer_id, meter, qty) VALUES (9, 'api_calls', 1)$$);
SELECT lr.wait_for($$SELECT count(*) = 2 FROM usage_events WHERE qty = 1$$);
SELECT worker_id, count(*) AS events FROM usage_events GROUP BY 1 ORDER BY 1;

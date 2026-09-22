-- @service: pghub
-- Step 7c. Hub: usage_events partitioned by worker_id, fed by one subscription per worker.
-- pglogical 2.4.8 writes into the partitions directly (usage_events_w1, usage_events_w2 are
-- the names of the tables on the workers too), not into the partitioned parent: see README.
CREATE TABLE usage_events (
  worker_id int NOT NULL, event_id bigint NOT NULL, customer_id int NOT NULL, meter text NOT NULL, qty int NOT NULL,
  PRIMARY KEY (worker_id, event_id)
) PARTITION BY LIST (worker_id);
CREATE TABLE usage_events_w1 PARTITION OF usage_events FOR VALUES IN (1);
CREATE TABLE usage_events_w2 PARTITION OF usage_events FOR VALUES IN (2);

SELECT pglogical.create_subscription(
  subscription_name := 'sub_usage_w' || n,
  provider_dsn := 'host=pgw' || n || ' dbname=app user=postgres',
  replication_sets := ARRAY['usage'],
  synchronize_data := true,
  forward_origins := '{}') IS NOT NULL AS created
FROM generate_series(1, 2) n;
SELECT pglogical.wait_for_subscription_sync_complete('sub_usage_w' || n) FROM generate_series(1, 2) n;
SELECT subscription_name, status FROM pglogical.show_subscription_status() ORDER BY 1;
SELECT lr.wait_for($$SELECT count(*) = 12 FROM usage_events$$);
SELECT tableoid::regclass AS partition, count(*) FROM usage_events GROUP BY 1 ORDER BY 1;

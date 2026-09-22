-- @service: hub
-- Step 2b. The hub receives every worker's events into ONE table partitioned by worker_id
-- (PG13+: a subscription can target a partitioned table; rows are routed to partitions).
CREATE TABLE usage_events (
  worker_id   int    NOT NULL,
  event_id    bigint NOT NULL,
  customer_id int    NOT NULL,
  meter       text   NOT NULL,
  qty         int    NOT NULL,
  PRIMARY KEY (worker_id, event_id)          -- must include the partition key
) PARTITION BY LIST (worker_id);
CREATE TABLE usage_events_w1 PARTITION OF usage_events FOR VALUES IN (1);
CREATE TABLE usage_events_w2 PARTITION OF usage_events FOR VALUES IN (2);
CREATE TABLE usage_events_w3 PARTITION OF usage_events FOR VALUES IN (3);

SELECT format($f$CREATE SUBSCRIPTION sub_usage_w%1$s
  CONNECTION 'host=worker%1$s dbname=app user=postgres' PUBLICATION pub_usage$f$, n)
FROM generate_series(1, 3) n \gexec

-- initial copy: 6 + 10 + 8 events from the three workers (worker3 has 4 customers, w2 has 5, w1 has 3)
SELECT lr.wait_for($$SELECT count(*) = 24 FROM usage_events$$);

-- rows were routed to partitions
SELECT tableoid::regclass AS partition, count(*) FROM usage_events GROUP BY 1 ORDER BY 1;
-- same event_id on different workers, no collision
SELECT worker_id, event_id, customer_id, meter FROM usage_events WHERE event_id = 1 ORDER BY 1;

-- The invoicing query: usage from all workers x reference data owned by the hub.
SELECT c.name, c.worker_id AS home_worker, u.meter, sum(u.qty) AS qty,
       sum(u.qty * p.unit_price) AS amount
FROM usage_events u
JOIN customers c USING (customer_id)
JOIN prices p ON p.plan_id = c.plan_id AND p.meter = u.meter
GROUP BY c.customer_id, c.name, c.worker_id, u.meter
ORDER BY c.customer_id, u.meter;

SELECT worker_id, count(*) AS events, sum(qty) AS total_qty FROM usage_events GROUP BY 1 ORDER BY 1;

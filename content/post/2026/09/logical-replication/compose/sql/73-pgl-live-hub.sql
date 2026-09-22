-- @service: pghub
-- Step 7d. Live changes. Reference data down (filtered), usage events up (into partitions).
INSERT INTO customers (name, worker_id, plan_id, billing_notes) VALUES ('new-for-w1', 1, 1, 'note'), ('new-for-w2', 2, 2, 'note');
UPDATE customers SET billing_notes = 'changed' WHERE customer_id = 1;
-- move customer 3 from worker 1 to worker 2 (new row does not match ref_w1's filter, matches ref_w2's)
UPDATE customers SET worker_id = 2 WHERE customer_id = 3;
SELECT lr.wait_for($$SELECT count(*) = 1 FROM dblink(lr.conn('pgw1'), 'SELECT 1 FROM customers WHERE name = ''new-for-w1''') t(x int)$$);
SELECT lr.wait_for($$SELECT count(*) = 1 FROM dblink(lr.conn('pgw2'), 'SELECT 1 FROM customers WHERE name = ''new-for-w2''') t(x int)$$);
-- Changes reach a worker in commit order: once the markers arrive, the customer-3 UPDATE has
-- been processed (or ignored) everywhere.
INSERT INTO customers (name, worker_id, plan_id) VALUES ('marker-w1', 1, 1), ('marker-w2', 2, 1);
SELECT lr.wait_for($$SELECT count(*) = 1 FROM dblink(lr.conn('pgw1'), 'SELECT 1 FROM customers WHERE name = ''marker-w1''') t(x int)$$);
SELECT lr.wait_for($$SELECT count(*) = 1 FROM dblink(lr.conn('pgw2'), 'SELECT 1 FROM customers WHERE name = ''marker-w2''') t(x int)$$);
SELECT 'worker1' AS node, customer_id, name, worker_id FROM dblink(lr.conn('pgw1'), 'SELECT customer_id, name, worker_id FROM customers')
  t(customer_id int, name text, worker_id int)
UNION ALL
SELECT 'worker2', customer_id, name, worker_id FROM dblink(lr.conn('pgw2'), 'SELECT customer_id, name, worker_id FROM customers')
  t(customer_id int, name text, worker_id int)
ORDER BY 1, 2;

-- Usage events written on the workers reach the partitions of the hub table
SELECT lr.on('pgw1', $$INSERT INTO usage_events_w1 (customer_id, meter, qty) VALUES (1, 'api_calls', 7)$$);
SELECT lr.on('pgw2', $$INSERT INTO usage_events_w2 (customer_id, meter, qty) VALUES (4, 'api_calls', 7)$$);
SELECT lr.wait_for($$SELECT count(*) = 14 FROM usage_events$$);
SELECT tableoid::regclass AS partition, count(*) FROM usage_events GROUP BY 1 ORDER BY 1;
SELECT c.name, u.meter, sum(u.qty * p.unit_price) AS amount
FROM usage_events u JOIN customers c USING (customer_id) JOIN prices p ON p.plan_id = c.plan_id AND p.meter = u.meter
WHERE u.qty = 7 GROUP BY c.name, u.meter ORDER BY 1;

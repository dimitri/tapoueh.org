-- @service: hub
-- Step 4a. Adding worker 4 LIVE, hub side, part 1: reference publication for worker 4.
CREATE PUBLICATION ref_w4 FOR TABLE plans, prices,
  customers (customer_id, name, worker_id, plan_id) WHERE (worker_id = 4);

-- rebalance: customers 9 and 12 will be served by worker 4 (worker 3 receives the DELETEs)
UPDATE customers SET worker_id = 4 WHERE customer_id IN (9, 12);

SELECT lr.wait_caught_up();

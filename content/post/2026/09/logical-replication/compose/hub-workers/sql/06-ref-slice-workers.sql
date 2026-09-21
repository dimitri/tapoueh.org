-- @service: worker1 worker2 worker3
-- Step 1f. What each worker holds after the changes of step 1e.
SELECT customer_id, name, worker_id, plan_id FROM customers ORDER BY 1;
SELECT unit_price FROM prices WHERE plan_id = 1 AND meter = 'api_calls';
-- customer 3 (moved w1 -> w2): DELETE arrived on worker1, INSERT arrived on worker2
SELECT count(*) AS has_customer_3 FROM customers WHERE customer_id = 3;

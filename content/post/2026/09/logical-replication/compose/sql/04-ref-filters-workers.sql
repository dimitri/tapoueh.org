-- @service: worker1 worker2 worker3
-- Step 1d. Switch each subscription to its own filtered publication.
SELECT format('ALTER SUBSCRIPTION sub_ref_w%1$s SET PUBLICATION ref_w%1$s WITH (refresh = false)', :worker_id) \gexec
SELECT subname, subpublications FROM pg_subscription;

-- Changing the publication is not retroactive: what was already copied stays.
SELECT count(*) AS customers_still_there, count(billing_notes) AS notes_still_there FROM customers;

-- Cleanup is our job (local writes to a subscribed table are allowed; the column is
-- not in the column list any more, so the apply worker will never write it).
DELETE FROM customers WHERE worker_id <> :worker_id;
ALTER TABLE customers DROP COLUMN billing_notes;
SELECT customer_id, name, worker_id, plan_id FROM customers ORDER BY 1;

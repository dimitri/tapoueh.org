-- @service: warehouse
-- Step 2f: the subscriber after all that.
select * from shop.customers order by id;
select id, customer_id, tenant, status from shop.orders order by id;

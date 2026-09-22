-- @service: warehouse
-- Step 2d: what the subscriber holds.
select column_name from information_schema.columns where table_schema = 'shop' and table_name = 'customers' order by ordinal_position;
select * from shop.customers order by id;
-- gotcha 2: the row filter only applies to changes and to new tables. The US order copied
-- BEFORE the filter existed is still here (and stays until someone deletes it by hand).
select id, tenant, status from shop.orders order by id;

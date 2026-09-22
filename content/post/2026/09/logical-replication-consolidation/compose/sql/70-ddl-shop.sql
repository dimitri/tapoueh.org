-- @service: shop
-- @nosync
-- Step 5a: DDL is not replicated. WRONG order for an additive change: publisher first.
alter table shop.orders add column note text;
insert into shop.orders (customer_id, amount, status, tenant, note) values (1, 5.00, 'new', 'eu', 'first with note');
insert into shop.orders (customer_id, amount, status, tenant) values (2, 6.00, 'new', 'eu');

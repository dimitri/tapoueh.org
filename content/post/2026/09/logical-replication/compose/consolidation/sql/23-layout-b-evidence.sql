-- @service: warehouse
-- @db: wh_shop
-- Layout (b) evidence: data is there, but nothing can join across databases.
select count(*) as shop_orders from shop.orders;
select count(*) from wh_crm.crm.accounts;
select * from crm.accounts;
select a.name, o.amount from shop.orders o join crm.accounts a on a.id = o.customer_id;
-- what each database costs: one apply worker per subscription, one slot per subscription on the source
select d.datname, count(*) as subscriptions from pg_subscription s join pg_database d on d.oid = s.subdbid group by 1 order by 1;

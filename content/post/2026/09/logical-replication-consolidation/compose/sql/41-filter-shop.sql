-- @service: shop
-- Step 2b: filter at the source. Column list drops email and phone; row filter keeps
-- only the EU tenant (this warehouse is the EU warehouse).
alter publication pub_shop set table
  shop.orders where (tenant = 'eu'),
  shop.customers (id, account_id, name, country, tenant) where (tenant = 'eu');

select schemaname, tablename, attnames, rowfilter from pg_publication_tables where pubname = 'pub_shop' order by tablename;

-- gotcha 1: the row filter column must be part of the replica identity, or UPDATE/DELETE fail at the source
update shop.orders set status = 'paid' where id = 4;
update shop.customers set name = 'Daniel' where id = 4;

-- fix: a unique index that contains the filter column, used as replica identity
create unique index orders_id_tenant on shop.orders (id, tenant);
alter table shop.orders replica identity using index orders_id_tenant;
create unique index customers_id_tenant on shop.customers (id, tenant);
alter table shop.customers replica identity using index customers_id_tenant;
update shop.orders set status = 'paid' where id = 4;
update shop.customers set name = 'Dan' where id = 4;

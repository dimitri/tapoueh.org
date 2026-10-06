-- @service: shop
-- @nosync
-- Step 1i: the way to get a schema per application on the subscriber is a schema per application on
-- the publisher. Move the tables, and give the application role a search_path so that the
-- unqualified table names in its SQL keep resolving.
create schema shopapp;
alter table public.orders set schema shopapp;
create role app_shop login;
alter role app_shop set search_path = shopapp;
grant usage on schema shopapp to app_shop;
grant select on all tables in schema shopapp to app_shop;
-- the publication followed the table: it is tracked by OID, not by name
select pubname, schemaname, tablename from pg_publication_tables where pubname = 'pub_rename';
-- the failed CREATE SUBSCRIPTION left nothing behind on the publisher
select count(*) as leftover_slots from pg_replication_slots where slot_name = 'sub_rename';

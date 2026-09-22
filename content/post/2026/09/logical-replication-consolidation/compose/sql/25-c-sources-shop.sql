-- @service: shop
-- Layout (c): a shared table fed by several sources. Both apps have a table
-- public.customers with the same shape; the ids overlap on purpose.
create table public.customers (id int primary key, name text, city text);
insert into public.customers values (1, 'Alice (shop)', 'Paris'), (2, 'Bob (shop)', 'Lyon');
create publication pub_c for table public.customers;

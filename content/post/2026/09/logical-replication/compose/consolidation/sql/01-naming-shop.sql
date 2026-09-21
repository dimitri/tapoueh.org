-- @service: shop
-- Step 1a: the naming constraint. Two apps each own a table called
-- public.customers (same shape, different rows) and public.contacts
-- (different shape). Both are about to be subscribed into ONE target.
create role repl replication login password 'repl';
grant pg_read_all_data to repl;

create table public.customers (id int primary key, name text, city text);
insert into public.customers values (1, 'Alice (shop)', 'Paris'), (2, 'Bob (shop)', 'Lyon');
create table public.contacts (id int primary key, name text);
insert into public.contacts values (1, 'Contact 1 (shop)');
create publication pub_naming for table public.customers, public.contacts;

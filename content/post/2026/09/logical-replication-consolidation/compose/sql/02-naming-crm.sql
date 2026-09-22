-- @service: crm
-- Step 1b: the crm app has the same table names. customers has the same
-- columns and overlapping ids; contacts has one extra column.
create role repl replication login password 'repl';
grant pg_read_all_data to repl;

create table public.customers (id int primary key, name text, city text);
insert into public.customers values (1, 'Carol (crm)', 'Nantes'), (2, 'Dave (crm)', 'Lille');
create table public.contacts (id int primary key, name text, company text);
insert into public.contacts values (1, 'Contact 1 (crm)', 'ACME');
create publication pub_naming for table public.customers, public.contacts;

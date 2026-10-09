-- @service: crm
-- @nosync
create table public.customers (id int primary key check (id > 100), name text, city text);
insert into public.customers values (101, 'Carol (crm)', 'Nantes'), (102, 'Dave (crm)', 'Lille');
create publication pub_c for table public.customers;

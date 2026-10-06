-- @service: crm
create table public.customers (id int primary key, name text, city text);
insert into public.customers values (1, 'Carol (crm)', 'Nantes'), (2, 'Dave (crm)', 'Lille');
create publication pub_c for table public.customers;

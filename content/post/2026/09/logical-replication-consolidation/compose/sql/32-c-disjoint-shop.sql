-- @service: shop
-- @nosync
-- Layout (c), second try: the key spaces are disjoint (shop ids 1-99, crm ids 101-199).
create table public.customers (id int primary key check (id < 100), name text, city text);
insert into public.customers values (1, 'Alice (shop)', 'Paris'), (2, 'Bob (shop)', 'Lyon');
create publication pub_c for table public.customers;

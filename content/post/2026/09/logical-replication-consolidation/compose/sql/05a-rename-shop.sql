-- @service: shop
-- @nosync
-- Step 1g: the usual application: its tables live in schema public. The warehouse wants them in
-- a schema of their own ("shopapp"). Can the subscription rename public.orders to shopapp.orders?
create table public.orders (id int primary key, customer_id int not null, amount numeric(10,2) not null);
insert into public.orders values (1, 1, 100.00), (2, 2, 50.00);
create publication pub_rename for table public.orders;

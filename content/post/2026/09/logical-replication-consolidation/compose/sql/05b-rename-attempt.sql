-- @service: warehouse
-- @nosync
-- Step 1h: the warehouse has the schema and the table it wants. The subscription can only look for
-- the publisher's own names, public.orders, and fails.
create schema shopapp;
create table shopapp.orders (id int primary key, customer_id int not null, amount numeric(10,2) not null);
create subscription sub_rename
  connection 'host=shop dbname=shop user=repl password=repl'
  publication pub_rename;
select count(*) as subscriptions_created from pg_subscription where subname = 'sub_rename';

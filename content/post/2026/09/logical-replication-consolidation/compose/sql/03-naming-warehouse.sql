-- @service: warehouse
-- @nosync
-- Step 1c: one target table per name, two subscriptions feeding it.
-- The first subscription works; the second one cannot copy its rows.
create table public.customers (id int primary key, name text, city text);
create table public.contacts (id int primary key, name text);

create subscription sub_naming_shop
  connection 'host=shop dbname=shop user=repl password=repl'
  publication pub_naming;

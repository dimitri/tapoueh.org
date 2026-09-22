-- @service: shop
-- Step 2e: what does the publisher actually send? Read the pgoutput stream of a scratch
-- slot after each statement. Message types: B begin, R relation, I insert, U update,
-- D delete, C commit. "pii_bytes" is true if any message contains an email or phone value.
create function peek(slot text default 'peek', pub text default 'pub_shop') returns table (messages text, pii_bytes boolean) language sql as $$
  select coalesce(string_agg(chr(get_byte(data, 0)), '' order by lsn), '(nothing)'),
         coalesce(bool_or(encode(data, 'escape') ~ '(@example\.com|\+33|\+49|\+1 555)'), false)
  from pg_logical_slot_get_binary_changes(slot, null, null, 'proto_version', '1', 'publication_names', pub) $$;
select count(*) as slot_created from pg_create_logical_replication_slot('peek', 'pgoutput');
select * from peek();   -- drains what earlier steps produced

\echo -- UPDATE of a filtered-out column only (phone)
update shop.customers set phone = '+33 9 99 99 99 99' where id = 1;
select * from peek();
\echo -- UPDATE of a published column (name)
update shop.customers set name = 'Alicia' where id = 1;
select * from peek();
\echo -- UPDATE of the filter column, eu -> us (row leaves the filter)
update shop.customers set tenant = 'us' where id = 4;
select * from peek();
\echo -- UPDATE of the filter column, us -> eu (row enters the filter)
update shop.customers set tenant = 'eu' where id = 3;
select * from peek();
\echo -- INSERT of a row outside the filter (us order)
insert into shop.orders (customer_id, amount, status, tenant) values (3, 10.00, 'new', 'us');
select * from peek();
\echo -- UPDATE of a published column on a row outside the filter
update shop.orders set status = 'shipped' where id = 3;
select * from peek();
\echo -- INSERT inside the filter
insert into shop.orders (customer_id, amount, status, tenant) values (1, 12.00, 'new', 'eu');
select * from peek();
\echo -- positive control for the PII check: a publication WITHOUT a column list does leak the phone
select count(*) as control_slot from pg_create_logical_replication_slot('peek_all', 'pgoutput');
create publication pub_control for table shop.customers;
update shop.customers set phone = '+33 9 99 99 99 88' where id = 1;
select * from peek('peek_all', 'pub_control');
select pg_drop_replication_slot('peek'), pg_drop_replication_slot('peek_all');
drop publication pub_control;
drop function peek(text, text);

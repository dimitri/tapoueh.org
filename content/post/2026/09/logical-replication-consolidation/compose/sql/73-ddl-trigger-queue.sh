# @nosync
# Step 5c: build pglogical's own trick with core only. A queue table, in the
# publication, captures DDL via an event trigger; a subscriber-side trigger,
# created ENABLE ALWAYS so it fires in the apply worker, executes it.

echo '### on shop: the queue table and the event trigger that fills it'
sq shop shop -f - <<'SQL'
create table shop.ddl_log (
  id      bigint generated always as identity primary key,
  tag     text        not null,
  command text        not null,
  logged_at timestamptz not null default now()
);

create function shop.log_ddl() returns event_trigger
  language plpgsql as $f$
begin
  insert into shop.ddl_log (tag, command) values (tg_tag, current_query());
end;
$f$;

create event trigger shop_log_ddl on ddl_command_end
  when tag in ('CREATE TABLE', 'ALTER TABLE')
  execute function shop.log_ddl();
SQL

echo '### on warehouse: a matching table, and the replay trigger (ENABLE ALWAYS)'
sq warehouse warehouse -f - <<'SQL'
create table shop.ddl_log (
  id      bigint primary key,
  tag     text        not null,
  command text        not null,
  logged_at timestamptz not null default now()
);

create function shop.apply_ddl() returns trigger
  language plpgsql as $f$
begin
  execute new.command;
  return new;
end;
$f$;

create trigger apply_ddl before insert on shop.ddl_log
  for each row execute function shop.apply_ddl();
alter table shop.ddl_log enable always trigger apply_ddl;
-- sub_shop's tablesync worker runs as the subscription owner: same rule as
-- every other table in this architecture, see "A schema per application".
alter table shop.ddl_log owner to sub_shop;
SQL

echo '### wire the queue table into the existing publication and subscription'
sq shop shop -qc "alter publication pub_shop add table shop.ddl_log" </dev/null
sq warehouse warehouse -qc "alter subscription sub_shop refresh publication" </dev/null
wait_until warehouse warehouse \
  "select srsubstate = 'r' from pg_subscription_rel r join pg_subscription s on s.oid = r.srsubid where s.subname = 'sub_shop' and r.srrelid = 'shop.ddl_log'::regclass" 30

echo '### a real DDL statement on shop, one statement, so current_query() stays clean'
sq shop shop -qc "alter table shop.orders add column urgent boolean default false" </dev/null
sync_all

echo '### on warehouse: the column exists, with no manual ALTER TABLE there at all'
sq warehouse warehouse -c \
  "select column_name, column_default from information_schema.columns where table_schema = 'shop' and table_name = 'orders' and column_name = 'urgent'" </dev/null
echo '### and the queue table doubles as an audit log'
sq warehouse warehouse -c "select tag, command from shop.ddl_log order by id" </dev/null

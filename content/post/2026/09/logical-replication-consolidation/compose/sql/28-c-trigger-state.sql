-- @service: warehouse
-- What happened during the initial copy with default (ENABLE) triggers?
select source, id, name from public.customers order by id;
select count(*) as probe_rows_after_copy from public.probe;
select tgname, tgenabled from pg_trigger where tgrelid = 'public.customers'::regclass and not tgisinternal order by 1;
-- ENABLE = 'O' fires only when session_replication_role is origin/local. Apply workers run as 'replica'.
alter table public.customers enable always trigger a_stamp;
alter table public.customers enable always trigger b_probe;
select tgname, tgenabled from pg_trigger where tgrelid = 'public.customers'::regclass and not tgisinternal order by 1;

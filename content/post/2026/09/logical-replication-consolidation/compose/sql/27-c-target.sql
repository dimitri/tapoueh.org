-- @service: warehouse
-- @nosync
-- Layout (c) target: public.customers has an EXTRA column "source" that the
-- publishers do not have. A probe trigger records what an apply worker can see.
create role wh_owner nologin;
grant wh_owner to sub_shop, sub_crm;          -- needed: apply switches to the table owner
grant create on schema public to wh_owner;
alter role sub_shop set lr.source = 'shop';    -- a per-role GUC, another way to carry a label
alter role sub_crm  set lr.source = 'crm';

create table public.probe (
  n serial primary key, op text, cur_user text, sess_user text,
  origin_is_setup boolean, sub_from_pid text, origin_from_pid text,
  origin_by_progress text, application_name text, role_guc text, backend_type text);
alter table public.probe owner to wh_owner;
alter sequence public.probe_n_seq owner to wh_owner;

create table public.customers (source text not null default 'unknown', id int primary key, name text, city text);
alter table public.customers owner to wh_owner;

-- privileged functions are revoked from PUBLIC: without these grants the apply worker fails
grant execute on function pg_replication_origin_session_is_setup() to wh_owner;
grant execute on function pg_replication_origin_session_progress(boolean) to wh_owner;
grant select on pg_replication_origin_status to wh_owner;
grant execute on function pg_show_replication_origin_status() to wh_owner;

create function public.probe_identity() returns trigger language plpgsql as $$
begin
  insert into public.probe (op, cur_user, sess_user, origin_is_setup, sub_from_pid, origin_from_pid,
                            origin_by_progress, application_name, role_guc, backend_type)
  values (tg_op, current_user, session_user, pg_replication_origin_session_is_setup(),
    (select subname from pg_stat_subscription where pid = pg_backend_pid()),
    (select 'pg_' || subid || coalesce('_' || relid, '') from pg_stat_subscription where pid = pg_backend_pid()),
    (select external_id from pg_replication_origin_status
      where remote_lsn = pg_replication_origin_session_progress(false) limit 1),
    current_setting('application_name'), current_setting('lr.source', true),
    (select backend_type from pg_stat_activity where pid = pg_backend_pid()));
  return new;
end $$;

-- stamping trigger: label from the subscription-owner role name (sub_<source>)
create function public.stamp_source() returns trigger language plpgsql as $$
begin
  if pg_replication_origin_session_is_setup() then
    new.source := regexp_replace(session_user, '^sub_', '');
  else
    new.source := 'local';
  end if;
  return new;
end $$;
create trigger a_stamp before insert on public.customers for each row execute function public.stamp_source();
create trigger b_probe before insert on public.customers for each row execute function public.probe_identity();
-- note: both triggers are still in the default state (ENABLE = fire on origin sessions only)

set role sub_shop;
create subscription sub_c_shop connection 'host=shop dbname=shop user=repl password=repl' publication pub_c;
reset role;

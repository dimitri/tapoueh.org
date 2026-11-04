-- uuidv7() in plain SQL, for servers older than Postgres 18.
-- gen_random_uuid() is built in from Postgres 13; before that, pgcrypto has it.
create or replace function public.uuidv7() returns uuid
    language sql volatile parallel safe
as $$
    select encode(
        set_bit(
            set_bit(
                overlay(uuid_send(gen_random_uuid())
                        placing substring(int8send((extract(epoch from clock_timestamp()) * 1000)::bigint) from 3)
                        from 1 for 6),
                52, 1),
            53, 1),
        'hex')::uuid
$$;

\echo == version nibble, variant nibble, and the clock it carries
select substr(u::text, 15, 1)                                   as version,
       substr(u::text, 20, 1) in ('8', '9', 'a', 'b')           as rfc_variant,
       abs(extract(epoch from clock_timestamp()
           - to_timestamp(('x' || substr(u::text, 1, 8) || substr(u::text, 10, 4))::bit(48)::bigint / 1000.0))) < 1
                                                                as clock_within_1s
  from (select public.uuidv7() as u) as t;

\echo == which uuidv7() is resolved
select p.oid::regprocedure, n.nspname
  from pg_proc p join pg_namespace n on n.oid = p.pronamespace
 where p.proname = 'uuidv7' order by n.nspname;
select (uuidv7() = public.uuidv7()) is not null as both_callable;

\echo == one statement, 1,000,000 ids: how many are smaller than the one before
create temp table gen as
    select row_number() over () as n, public.uuidv7() as u
      from generate_series(1, 1000000);
select count(*) filter (where u < prev) as sql_inversions
  from (select u, lag(u) over (order by n) as prev from gen) as t;
\timing on
select count(public.uuidv7()) from generate_series(1, 1000000);
\timing off

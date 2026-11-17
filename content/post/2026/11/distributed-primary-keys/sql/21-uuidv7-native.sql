\echo == native uuidv7() and the SQL one defined above, on the same server
select version();
select p.oid::regprocedure, n.nspname
  from pg_proc p join pg_namespace n on n.oid = p.pronamespace
 where p.proname = 'uuidv7' order by n.nspname;
select pg_typeof(uuidv7()), uuid_extract_version(uuidv7()) as native_version;
\echo == which one an unqualified call runs
explain (verbose, costs off) select uuidv7();
\echo == native, one statement, 1,000,000 ids: inversions
create temp table gen_native as
    select row_number() over () as n, pg_catalog.uuidv7() as u
      from generate_series(1, 1000000);
select count(*) filter (where u < prev) as native_inversions
  from (select u, lag(u) over (order by n) as prev from gen_native) as t;
\timing on
select count(pg_catalog.uuidv7()) from generate_series(1, 1000000);
\timing off

+++
title     = "Getting Ready for PostgreSQL 19"
date      = "2026-09-01T09:00:00+0200"
tags      = ["PostgreSQL", "SQL"]
categories = ["PostgreSQL", "SQL"]
icon      = "🐘"
+++

PostgreSQL 19 Beta 3 shipped on August 13, 2026, [the release notes are
already
frozen](https://www.postgresql.org/docs/19/release-19.html) even though the
exact GA date isn't announced yet — following the project's usual September/October
cadence, general availability should land within the next few weeks. That
makes now the right time to read through what's changing, the same way I did
for [PostgreSQL 11 through 18 a few weeks
ago](/blog/2026/07/pg-since-11/).

This is not a changelog dump. It's the subset of PG 19 I think is worth
knowing about before you upgrade: a handful of compatibility breaks that
will bite people who don't read release notes, and the SQL-level additions
I found genuinely useful once I started poking at them. Every query below
ran against a real PostgreSQL 19 Beta 3 instance — no hand-waving about
syntax that might work.

{{< lab >}}
Every query in this article ran against [the Lab](https://theartofpostgresql.com/lab/), the same free dataset bundle used in the rest of this blog (F1 data, geopolitical data, music data, and more), pinned to a real PostgreSQL 19 Beta 3 instance. PG 19 support isn't the Lab's default yet — pass `POSTGRES_VERSION=19beta3 PG_MAJOR=19` to `docker compose up` to reproduce these results yourself; plain `docker compose up` still runs PG 16 until 19 reaches general availability.
{{< /lab >}}

<!--more-->

<!--toc-->

---

## Before you upgrade: compatibility breaks

PostgreSQL 19 carries more breaking changes than a typical release. None of
them are exotic, but each one can silently change behavior if you don't
know to look for it.

- **JIT is now disabled by default.** The optimizer's JIT costing model was
  found to be unreliable, so PG 19 turns JIT off out of the box. If your
  workload includes large analytical queries and you were relying on JIT
  kicking in automatically, you now need to enable it by hand
  (`jit = on`).
- **`standard_conforming_strings` is now always on**, with no way to turn it
  off server-side. Dumps taken with old versions of `pg_dump`/`pg_dumpall`
  while the *source* server had `standard_conforming_strings = off` will not
  load cleanly into PostgreSQL 19. Re-dump with a current `pg_dump` before
  migrating old data.
- **RADIUS authentication is removed.** It only ever supported RADIUS over
  UDP, which the project now considers unfixably insecure. If you
  authenticate via RADIUS, you need a different method before upgrading.
- **MD5 password authentication now issues a warning** on every successful
  login (`md5_password_warnings` controls it). MD5 was deprecated in
  PostgreSQL 18; this is the next step toward removing it. Migrate to `scram-sha-256`.
- **The default opclass for `inet`/`cidr` GiST indexing changes.** The old
  default excluded rows that should have matched. `pg_upgrade` will refuse
  to upgrade a cluster with indexes built on the broken opclass — you'll
  need to `REINDEX` them first.
- **CR/LF characters are disallowed in database, role, and tablespace
  names**, for security reasons. `pg_upgrade` also refuses clusters that use
  such names.
- **`max_locks_per_transaction` default doubles**, from 64 to 128 — lock
  size accounting changed internally, so an explicit setting you tuned
  under PG ≤18 now covers half the locks it used to. Double it if you set
  it explicitly.

None of these are difficult to handle. All of them are easy to miss if you
only skim the highlights section.

---

## Temporal updates: FOR PORTION OF

Modeling a fact that's true "from X to Y" and then having that fact change
partway through the interval has always meant hand-rolling the split:
`UPDATE` the row's end date, then `INSERT` a new row for the remainder. PG
19 adds the standard SQL `FOR PORTION OF` clause, which does the split for
you in one statement.

```sql
create table demo_driver_contract (
  driverid     bigint not null,
  team         text   not null,
  valid_period daterange not null,
  exclude using gist (driverid with =, valid_period with &&)
);

insert into demo_driver_contract (driverid, team, valid_period)
values (1, 'McLaren', daterange('2007-01-01', '2013-01-01'));

select * from demo_driver_contract;
```

```results
 driverid |  team   |      valid_period       
----------+---------+-------------------------
        1 | McLaren | [2007-01-01,2013-01-01)
```

Now split the middle of that period out with a different value, in a single
`UPDATE`:

```sql
update demo_driver_contract
for portion of valid_period from '2010-01-01' to '2013-01-01'
set team = 'McLaren (final years)'
where driverid = 1;

select * from demo_driver_contract order by valid_period;
```

```results
 driverid |         team          |      valid_period       
----------+-----------------------+-------------------------
        1 | McLaren               | [2007-01-01,2010-01-01)
        1 | McLaren (final years) | [2010-01-01,2013-01-01)
```

One `UPDATE` produced two rows: the original row shrank to the untouched
portion, and a new row was inserted for the updated sub-range — the
exclusion constraint stayed satisfied throughout. `FOR PORTION OF` also
applies to `DELETE`, removing just the requested slice of the range and
leaving the rest of the row(s) intact.

---

## INSERT ... ON CONFLICT DO SELECT ... RETURNING

`ON CONFLICT DO NOTHING` and `DO UPDATE` have existed since PG 9.5, but
neither one gives you back the *existing* row when a conflict happens — you
had to follow up with a separate `SELECT`. PG 19 adds a third branch,
`DO SELECT`, that returns the conflicting row directly, optionally locked
with `FOR UPDATE`/`FOR SHARE`.

```sql
create table demo_driver_seen (
  driverid      bigint primary key,
  surname       text not null,
  first_seen_at timestamptz not null default now()
);

insert into demo_driver_seen (driverid, surname)
values (1, 'Hamilton')
on conflict (driverid) do select
returning driverid, surname, first_seen_at;
```

```results
 driverid | surname  |         first_seen_at         
----------+----------+-------------------------------
        1 | Hamilton | 2026-09-01 14:44:45.661884+00
```

Running the exact same statement again, a second later, hits the conflict
and returns the *existing* row instead of erroring or inserting a
duplicate:

```sql
insert into demo_driver_seen (driverid, surname)
values (1, 'Hamilton')
on conflict (driverid) do select
returning driverid, surname, first_seen_at;
```

```results
 driverid | surname  |         first_seen_at         
----------+----------+-------------------------------
        1 | Hamilton | 2026-09-01 14:44:45.661884+00
```

Same `first_seen_at` both times — no new row, no round trip to fetch the
conflicting row separately. This is the missing piece for idempotent
"upsert-and-fetch" patterns: registration flows, dedup-on-write ingestion,
anywhere you need the row back regardless of whether this call created it.

---

## Window functions: IGNORE NULLS

`lead()`, `lag()`, `first_value()`, `last_value()`, and `nth_value()` now
accept `IGNORE NULLS` (or the explicit default, `RESPECT NULLS`), placed
right after the function call and before `OVER`. Without it, a `NULL` in
the window simply propagates; with it, the function skips `NULL`s and finds
the nearest actual value.

Kimi Räikkönen's 2017 season is a clean example: a DNF at the Spanish Grand
Prix leaves a `NULL` finishing position for that round.

```sql
select races.round, races.name, results.position as finish,
       lag(results.position) over (order by races.round) as prev_plain,
       lag(results.position) ignore nulls over (order by races.round) as prev_ignore_nulls
from f1db.results
join f1db.races using(raceid)
join f1db.drivers using(driverid)
where drivers.surname = 'Räikkönen'
  and extract(year from races.date) = 2017
order by races.round
limit 8;
```

```results
 round |         name          | finish | prev_plain | prev_ignore_nulls 
-------+-----------------------+--------+------------+-------------------
     1 | Australian Grand Prix |      4 |            |                  
     2 | Chinese Grand Prix    |      5 |          4 |                 4
     3 | Bahrain Grand Prix    |      4 |          5 |                 5
     4 | Russian Grand Prix    |      3 |          4 |                 4
     5 | Spanish Grand Prix    |        |          3 |                 3
     6 | Monaco Grand Prix     |      2 |            |                 3
     7 | Canadian Grand Prix   |      7 |          2 |                 2
     8 | Azerbaijan Grand Prix |     14 |          7 |                 7
```

At round 6, `prev_plain` shows `NULL` — the DNF at round 5 propagated
straight through. `prev_ignore_nulls` shows `3`, Räikkönen's actual last
finish from round 4, correctly skipping over the gap. Before PG 19, getting
this "last known value" behavior required a window frame trick or a
subquery threading a separate counter — a common enough need that it's now
built in.

---

## CHECK constraints can be un-enforced

PostgreSQL 17 added `NOT ENFORCED` for foreign key constraints — useful for
staging a migration where you know historical data violates a constraint
you want to add. PG 19 extends the same mechanism to `CHECK` constraints.

```sql
create table demo_driver (
  driverid bigint primary key,
  points   numeric check (points >= 0)
);

insert into demo_driver values (1, 10);

alter table demo_driver alter constraint demo_driver_points_check not enforced;

-- would normally violate the check, but enforcement is off
insert into demo_driver values (2, -5);

select * from demo_driver order by driverid;
```

```results
 driverid | points 
----------+--------
        1 |     10
        2 |     -5
```

Turning enforcement back on validates existing data — and correctly refuses
if any row now violates the constraint:

```sql
alter table demo_driver alter constraint demo_driver_points_check enforced;
```

```results
ERROR:  check constraint "demo_driver_points_check" of relation "demo_driver" is violated by some row
```

This is the standard "add the constraint now, clean up the data later"
workflow — previously available only for foreign keys — generalized to any
`CHECK` constraint.

---

## REPACK replaces VACUUM FULL and CLUSTER

`VACUUM FULL` and `CLUSTER` have always done nearly the same thing —
rewrite a table to reclaim space or apply a physical ordering — under two
confusingly different names, with two different lock behaviors. PG 19
unifies them into a single `REPACK` command (the old names still work, kept
for compatibility), and adds a `CONCURRENTLY` option that avoids the
access-exclusive lock both predecessors required.

```sql
create table demo_bloat (id int primary key, val text);
insert into demo_bloat select g, repeat('x', 100) from generate_series(1,1000) g;
delete from demo_bloat where id % 2 = 0;

select pg_size_pretty(pg_total_relation_size('demo_bloat')) as before_repack;
```

```results
 before_repack 
---------------
 224 kB
```

```sql
repack demo_bloat;

select pg_size_pretty(pg_total_relation_size('demo_bloat')) as after_repack;
```

```results
 after_repack 
--------------
 112 kB
```

Same space reclamation you'd get from `VACUUM FULL`, one clearer command
name, and `REPACK CONCURRENTLY table_name` when you can't afford the lock.

---

## Merging and splitting partitions online

Restructuring a partitioned table used to mean detaching partitions,
recreating them with new bounds, and reattaching — each step a separate
operation, each one requiring you to reason about visibility in between. PG
19 adds `ALTER TABLE ... MERGE PARTITIONS` and `... SPLIT PARTITION` to do
this as a single, online operation.

```sql
create table demo_races (raceid bigint, season int, name text)
  partition by range (season);
create table demo_races_2015 partition of demo_races for values from (2015) to (2016);
create table demo_races_2016 partition of demo_races for values from (2016) to (2017);
create table demo_races_2017 partition of demo_races for values from (2017) to (2018);

insert into demo_races
select raceid, extract(year from date)::int, name
from f1db.races where extract(year from date) between 2015 and 2017;

select tableoid::regclass, count(*) from demo_races group by 1 order by 1;
```

```results
    tableoid     | count 
-----------------+-------
 demo_races_2015 |    19
 demo_races_2016 |    21
 demo_races_2017 |    20
```

```sql
alter table demo_races merge partitions
  (demo_races_2015, demo_races_2016, demo_races_2017)
  into demo_races_2015_2017;

select tableoid::regclass, count(*) from demo_races group by 1 order by 1;
```

```results
       tableoid       | count 
----------------------+-------
 demo_races_2015_2017 |    60
```

All 60 rows survived the merge, now living in one partition instead of
three. `SPLIT PARTITION` runs the same idea in reverse — useful once a
single partition has grown large enough that you want to break it apart by
a finer-grained boundary, without a full table rebuild.

---

## Dumping DDL from the catalog directly

Three new functions return the `CREATE`/`ALTER` statements needed to
recreate an object, straight from the catalog — no external tool required:
`pg_get_role_ddl()`, `pg_get_tablespace_ddl()`, and `pg_get_database_ddl()`.

```sql
select pg_get_role_ddl('taop'::regrole);
```

```results
                                   pg_get_role_ddl                                   
--------------------------------------------------------------------------------------
 CREATE ROLE taop SUPERUSER INHERIT CREATEROLE CREATEDB LOGIN REPLICATION BYPASSRLS;
 ALTER ROLE taop SET search_path TO 'f1db', 'chinook', 'public', 'scan34';
```

```sql
select pg_get_database_ddl('taop'::regdatabase);
```

```results
                                              pg_get_database_ddl                                               
------------------------------------------------------------------------------------------------------------------
 CREATE DATABASE taop WITH TEMPLATE = template0 ENCODING = 'UTF8' LOCALE_PROVIDER = libc LOCALE = 'en_US.utf8';
 ALTER DATABASE taop OWNER TO taop;
```

Each function returns a `SETOF text`, one row per statement, in the order
they need to run — the role's own `CREATE ROLE` first, then any `ALTER ROLE
... SET` session defaults. Previously this meant reaching for `pg_dumpall
--roles-only` or a third-party script; now it's a plain SQL query, scriptable
from inside any migration tool that already talks to the database.

---

## Ranges: subtracting with gaps

`range_agg()` has existed since PG 14 for building a multirange out of
several ranges. PG 19 adds the inverse operation for subtraction:
`range_minus_multi()` and `multirange_minus_multi()`, both returning a set
of the fragments left over after removing one range from another —
correctly producing *two* fragments when the subtracted range falls in the
middle.

```sql
select range_minus_multi(
  daterange('2007-01-01', '2013-01-01'),
  daterange('2010-01-01', '2011-01-01')
);
```

```results
    range_minus_multi    
--------------------------
 [2007-01-01,2010-01-01)
 [2011-01-01,2013-01-01)
```

Plain range subtraction (`-`) has always required the subtracted range to
sit at one end — subtracting from the middle raises an error, because the
result isn't representable as a single range. `range_minus_multi()` sidesteps
that restriction entirely by returning a set.

---

## Worth watching, not yet covered here

A few PG 19 additions are large enough to deserve their own treatment
later, once they've had time to settle:

- **SQL/PGQ** — SQL-standard property graph queries, processed internally
  as views over regular relational data. A genuinely new query paradigm on
  top of PostgreSQL's existing storage model, not a new storage engine.
- **`pg_plan_advice`** and **`pg_stash_advice`** — two new extensions for
  stabilizing planner decisions: the first lets you record and replay a
  known-good plan shape, the second applies stored advice automatically
  based on the incoming query. Aimed squarely at the "the plan changed
  after a statistics update and now everything is slow" problem.
- **`WAIT FOR`** — a new command that blocks until a standby has replayed
  WAL up to a given point, giving you a clean way to implement
  read-your-writes against a read replica without polling `pg_stat_replication`.
- **Parallel autovacuum** and a **priority scoring system** for which
  tables get vacuumed first — genuinely useful operationally, but outside
  this article's SQL-level focus.

---

I'm currently updating [*The Art of
PostgreSQL*](https://theartofpostgresql.com) for PostgreSQL 19 — several of
the queries above map directly onto existing chapters (temporal ranges,
partitioning, constraints, window functions) and will be folded into the
relevant sections rather than bolted on as a separate appendix. That work
isn't finished yet; it'll ship to everyone who already owns the book, at no
extra cost, once it's ready — timed for PostgreSQL 19's general
availability rather than rushed ahead of it.

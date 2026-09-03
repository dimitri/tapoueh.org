+++
title     = "Getting Ready for PostgreSQL 19"
date      = "2026-09-03T15:09:30+0200"
tags      = ["PostgreSQL", "SQL"]
categories = ["PostgreSQL", "SQL"]
icon      = "🐘"
+++

PostgreSQL 19 Beta 3 shipped on August 13, 2026, and [the release
notes](https://www.postgresql.org/docs/19/release-19.html) have been filled
in as of 2026-07-18 — still marked subject to change, and the GA date isn't
announced yet, but following the project's usual September/October cadence
general availability should land within the next few weeks. That makes now
the right time to read through what's changing, the same way I did for
[PostgreSQL 11 through 18 a few weeks
ago](/blog/2026/07/sql-improvements-in-postgresql-1118-a-personal-selection/).

This is not a changelog dump. It's the subset of PG 19 I think is worth
knowing about before you upgrade: a handful of compatibility breaks that
will bite people who don't read release notes, and the SQL-level additions
I found genuinely useful once I started poking at them. Every query below
ran against a real PostgreSQL 19 Beta 3 instance — no hand-waving about
syntax that might work.

{{< lab >}}
Every query in this article ran against [the Lab](https://theartofpostgresql.com/lab/), the same free dataset bundle used in the rest of this blog (F1 data, geopolitical data, music data, and more), pinned to a real PostgreSQL 19 Beta 3 instance. PG 19 support isn't the Lab's default yet, but there is a prebuilt beta image on the registry, for both `linux/amd64` and `linux/arm64` — so `POSTGRES_VERSION=19beta3 PG_MAJOR=19 docker compose up` pulls it rather than building anything, and every query below reproduces on it exactly as printed. (`PG_MAJOR` only matters if Compose ends up building; it costs nothing to set.) Plain `docker compose up` still pulls the PG 16 image, and stays the default until 19 reaches general availability.
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
- **The default opclass for `inet`/`cidr` GiST indexing changes**, from the
  ones the `btree_gist` extension supplies to new core GiST opclasses. The
  old ones are broken: they can exclude rows that should have been returned.
  `pg_upgrade` refuses to upgrade a cluster carrying `btree_gist`
  `inet`/`cidr` indexes, so `REINDEX` them first.
- **CR/LF characters are disallowed in database, role, and tablespace
  names**, for security reasons. `pg_upgrade` also refuses clusters that use
  such names.
- **`max_locks_per_transaction` default doubles**, from 64 to 128. This is
  not extra headroom: lock size allocation changed, so as the release notes
  put it, settings must now be doubled to match the capacity they had in
  previous releases. If you tuned this explicitly under PG ≤18, double your
  value.
- **`default_toast_compression` changes from `pglz` to `lz4`.** A silent,
  cluster-wide change to how out-of-line values are compressed — generally
  faster, but it shifts both storage and CPU behaviour with no action on
  your part.

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
create table demo_driver_contract
 (
  driverid     bigint not null,
  team         text not null,
  valid_period daterange not null,
  exclude      using gist(driverid with =, valid_period with &&)
);

insert into demo_driver_contract(driverid, team, valid_period)
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
update demo_driver_contract for portion of valid_period
  from '2010-01-01' to '2013-01-01'
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

{{< image src="fig-for-portion-of.svg" title="One contract row valid 2007–2013, before and after an UPDATE ... FOR PORTION OF covering 2010–2013. The original row shrinks to the untouched 2007–2010 portion and a second row is inserted for the updated sub-range, so the two never overlap." >}}

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
with `FOR UPDATE`, `FOR NO KEY UPDATE`, `FOR SHARE` or `FOR KEY SHARE`, and
optionally filtered with its own `WHERE`.

The spelling takes a moment to get used to, because `DO SELECT` has no
select list:

```
DO SELECT [ FOR { UPDATE | NO KEY UPDATE | SHARE | KEY SHARE } ] [ WHERE condition ]
```

`SELECT` here is an *action name*, exactly parallel to `NOTHING` in `DO
NOTHING` — not a query. There is nothing to project, because the conflicting
row is already pinned down by the conflict target; `DO UPDATE` needs its
`SET` because you have to say what changes, and `DO SELECT` needs nothing
because you don't. The projection goes in the slot `INSERT` already has,
which is why `DO SELECT` is the one conflict action for which `RETURNING` is
*mandatory*: leave it off and the statement would have nothing to give back,
which would rather defeat the point.

```sql
create table demo_driver_seen
 (
  driverid      bigint primary key,
  surname       text not null,
  first_seen_at timestamptz not null default now()
);

insert into demo_driver_seen(driverid, surname)
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
insert into demo_driver_seen(driverid, surname)
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
  select races.round,
         races.name,
         results.position as finish,
         lag(results.position) over(order by races.round) as prev_plain,
         lag(results.position) ignore nulls over(
                                                 order by races.round
                                            ) as prev_ignore_nulls
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

PostgreSQL 18 added `NOT ENFORCED` as a way to *declare* a constraint —
`CHECK` or foreign key — that the server records but never checks. PG 19
fills in the missing half: `ALTER TABLE ... ALTER CONSTRAINT ... [NOT]
ENFORCED` now works for `CHECK` constraints, so you can flip enforcement on
an existing one. Previously only foreign keys could be altered that way.

```sql
create table demo_driver
 (
  driverid bigint primary key,
  points   numeric check (points >= 0)
);

insert into demo_driver values (1, 10);

alter table demo_driver
  alter constraint demo_driver_points_check not enforced;

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

Worth being precise about what this is and isn't. If what you want is "add
the constraint now, validate the existing rows later", that's `ADD
CONSTRAINT ... NOT VALID` followed by `VALIDATE CONSTRAINT`, and it has
worked for `CHECK` constraints since PostgreSQL 9.2 — a `NOT VALID`
constraint still enforces itself against every *new* row. `NOT ENFORCED` is
the stronger, and rarer, thing: the constraint is documented in the catalog
and checked against nothing at all, new rows included. Reach for it when you
want the schema to record an invariant that some other layer is responsible
for enforcing; reach for `NOT VALID` when you just have a backlog to clean
up.

---

## REPACK replaces VACUUM FULL and CLUSTER

`VACUUM FULL` and `CLUSTER` have always done nearly the same thing —
rewrite a table to reclaim space or apply a physical ordering — under two
confusingly different names, with two different lock behaviors. PG 19
unifies them into a single `REPACK` command (the old names still work, kept
for compatibility), and adds a `CONCURRENTLY` option that avoids the
access-exclusive lock both predecessors required.

```sql
create table demo_bloat
 (
  id  int primary key,
  val text
);

insert into demo_bloat
     select g, repeat('x', 100)
       from generate_series(1, 1000) g;

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

Same space reclamation you'd get from `VACUUM FULL`, under one clearer
command name. `REPACK CONCURRENTLY table_name` is the version that avoids
holding the lock for the whole rewrite — but read its restrictions before
reaching for it. It isn't MVCC-safe; it's rejected for unlogged,
partitioned, catalog and TOAST tables, and for any table without a primary
key or replica-identity index; it needs a free slot from the new
`max_repack_replication_slots`; and it still takes a brief `ACCESS
EXCLUSIVE` at the end to swap the files in.

---

## MERGE PARTITIONS: held back for another release

This section was going to be about `ALTER TABLE ... MERGE PARTITIONS` and
`... SPLIT PARTITION`, which restructure a partitioned table in a single
statement instead of the detach/recreate/reattach dance. I wrote it, ran the
queries against Beta 3, and drew a diagram for it.

Then, on August 27, [Alexander Korotkov reverted the whole
feature](https://git.postgresql.org/gitweb/?p=postgresql.git;a=commit;h=3e8bcc864)
from the PostgreSQL 19 branch. I am leaving the section in, because how that
decision got made is more interesting than the feature would have been:

> The feature is reverted due to multiple design issues which are too late to
> address in this release cycle.

{{< image src="fig-partition-merge.svg" title="What MERGE PARTITIONS did in Beta 3: three yearly range partitions of demo_races holding 19, 21 and 20 races, folded into a single partition covering 2015-2018 with all 60 rows. The command was reverted from PostgreSQL 19 on August 27, 2026." >}}

That commit takes out 1,631 lines of `tablecmds.c`, 1,054 lines of
`partbounds.c`, both isolation test suites, the documentation, and the
`PARTITIONS` keyword. It is the second time this feature has been held back
late in a cycle — it was pulled from PostgreSQL 17 in August 2024 too, that
time over `CVE-2014-0062` repeatable-name-lookup issues.

Reverting two years of work a month before a release is not a failure of the
process. It *is* the process. Somebody found real problems, the people who
would have to live with them agreed they were real, and the feature went back
in the oven rather than into your database. That decision is available to a
project where the engineers have the final say on what ships. It is much
harder to make when a release date has been promised to a market.

There is a trap here worth knowing about. At the time of writing the
[release notes](https://www.postgresql.org/docs/19/release-19.html) still
list the feature, because the entry is *still in the release-note source on
the release branch* — that file was edited on September 1, five days after
the revert, and the entry survived. So if you check the official notes and
conclude that PG 19 merges partitions, you are reading something the binary
will not do. Beta 3, released August 13, still has the feature; anything
built after August 27 does not.

### What went wrong

The [thread that killed
it](https://www.postgresql.org/message-id/CAN4CZFNCU%3Dt09M%3D%2Br2t9hHLJuujdM4oQ8hCK_Sx-GpfiwMAicw%40mail.gmail.com)
was opened by Zsolt Parragi of Percona on July 23, listing five design
problems. Since Beta 3 still ships the feature, I went back and reproduced
them. Four of the five fall out in a handful of statements.

**A `CHECK` constraint quietly evaporates.** Attributes that belong to the
individual partitions — indexes, constraints, defaults, storage options,
comments — are simply dropped rather than carried across or refused:

```sql
create table demo_chk
 (
  id  int not null,
  val text
) partition by range (id);

create table demo_chk_lo partition of demo_chk for values from (0) to (10);

create table demo_chk_hi partition of demo_chk for values from (10) to (20);

alter table demo_chk_lo
  add constraint val_not_forbidden check (val <> 'forbidden');

insert into demo_chk values (1, 'ok');

insert into demo_chk values (2, 'forbidden');
```

```results
ERROR:  new row for relation "demo_chk_lo" violates check constraint "val_not_forbidden"
DETAIL:  Failing row contains (2, forbidden).
```

Good — that is the constraint doing its job. Now merge, and run the very
same statement again:

```sql
alter table demo_chk
  merge partitions (demo_chk_lo, demo_chk_hi) into demo_chk_all;

insert into demo_chk values (2, 'forbidden');

  select id, val
    from demo_chk
order by id;
```

```results
 id |    val    
----+-----------
  1 | ok
  2 | forbidden
```

No error, no warning, and the row your schema was explicitly rejecting a
moment ago is now sitting in the table. The index and the column default
went the same way.

**Stored generated columns are silently recomputed.** A partition can be
attached with a generation expression that differs from its parent's, which
is fine until two such partitions are merged and one expression wins:

```sql
create table demo_gen
 (
  id int not null,
  g  int generated always as (id * 100) stored
) partition by range (id);

create table demo_gen_lo partition of demo_gen for values from (0) to (10);

create table demo_gen_hi
 (
  id int not null,
  g  int generated always as (id * 2) stored
);

alter table demo_gen
  attach partition demo_gen_hi for values from (10) to (20);

insert into demo_gen values (3), (13);

  select tableoid::regclass as partition, id, g
    from demo_gen
order by id;
```

```results
  partition  | id |  g  
-------------+----+-----
 demo_gen_lo |  3 | 300
 demo_gen_hi | 13 |  26
```

```sql
alter table demo_gen
  merge partitions (demo_gen_lo, demo_gen_hi) into demo_gen_all;

  select tableoid::regclass as partition, id, g
    from demo_gen
order by id;
```

```results
  partition   | id |  g   
--------------+----+------
 demo_gen_all |  3 |  300
 demo_gen_all | 13 | 1300
```

Row 13 stored `26` before the merge and stores `1300` after it. Nothing in
the statement asked for that, and nothing reported it.

**Logical replication sees inserts that never happened.** The rows are moved
with plain heap inserts, so a decoding slot reports them as fresh `INSERT`s
into the new partition — with no matching `DELETE`s from the old ones:

```sql
create table demo_rep
 (
  id  int not null,
  val text
) partition by range (id);

create table demo_rep_lo partition of demo_rep for values from (0) to (10);

create table demo_rep_hi partition of demo_rep for values from (10) to (20);

insert into demo_rep values (1, 'one'), (11, 'eleven');

select slot_name
  from pg_create_logical_replication_slot('demo_slot', 'test_decoding');

alter table demo_rep
  merge partitions (demo_rep_lo, demo_rep_hi) into demo_rep_all;

select data from pg_logical_slot_get_changes('demo_slot', null, null);
```

```results
                                 data                                 
----------------------------------------------------------------------
 BEGIN 1280
 table public.demo_rep_all: INSERT: id[integer]:1 val[text]:'one'
 table public.demo_rep_all: INSERT: id[integer]:11 val[text]:'eleven'
 COMMIT 1280
```

A subscriber replaying that stream keeps the rows it already had in the old
partitions and adds the new copies on top.

**A publication can quietly empty itself.** If a publication named one of the
merged partitions directly, it loses it and gains nothing; `REPLICA IDENTITY
FULL` set on a partition reverts to the default too. The publication is still
there, still enabled, and now replicating nothing.

The fifth issue in the thread — losing `UPDATE`s on a subscriber when
`REFRESH PUBLICATION` runs with `copy_data = false` — needs two instances to
show, and I did not try it.

### What this costs you, and what it buys

Today, nothing changes: keep detaching, recreating and reattaching partitions
the way you already do. That path works, and it has the advantage that every
step is one you can see.

Look at what the five problems have in common, though, and the delay starts
to look like good news. Not one of them is about moving rows between files —
that part evidently works, and I ran it. They are all about what a partition
*carries*: its constraints, its generated columns, its publication membership,
its replica identity. Those are the questions that decide whether the command
is safe to run on a database you care about, and they are exactly the
questions worth taking another release to answer properly. A version of this
feature that moved rows correctly and lost your `CHECK` constraints would have
been worse than not having it, because you would have trusted it.

So `SPLIT`/`MERGE PARTITIONS` will land when the semantics are settled, and
when it does it will be the version that keeps what your partitions carry. On
the evidence above, that is worth waiting for.

One practical note while it is in flight: every other feature in this article
is in the release branch and staying there, but this one ran perfectly in Beta
3 and is gone. For anything you intend to depend on before GA, check the
branch you will actually run rather than the notes alone.

(For where partitioning stands without it, see the [partitioning section of
the PG 11–18
round-up](/blog/2026/07/sql-improvements-in-postgresql-1118-a-personal-selection/#partition-improvements-pg-1219).)

---

## Dumping DDL from the catalog directly

Three new functions return the `CREATE`/`ALTER` statements needed to
recreate an object, straight from the catalog — no external tool required:
`pg_get_role_ddl()`, `pg_get_tablespace_ddl()`, and `pg_get_database_ddl()`.

```sql
alter role taop set search_path to 'f1db', 'chinook', 'public', 'scan34';

select pg_get_role_ddl('taop'::regrole);
```

```results
                                   pg_get_role_ddl                                   
-------------------------------------------------------------------------------------
 CREATE ROLE taop SUPERUSER INHERIT CREATEROLE CREATEDB LOGIN REPLICATION BYPASSRLS;
 ALTER ROLE taop SET search_path TO 'f1db', 'chinook', 'public', 'scan34';
```

```sql
select pg_get_database_ddl('taop'::regdatabase);
```

```results
                                              pg_get_database_ddl                                               
----------------------------------------------------------------------------------------------------------------
 CREATE DATABASE taop WITH TEMPLATE = template0 ENCODING = 'UTF8' LOCALE_PROVIDER = libc LOCALE = 'en_US.utf8';
 ALTER DATABASE taop OWNER TO taop;
```

Each function returns a `setof text`, one row per statement, in the order
they need to run — the role's own `CREATE ROLE` first, then any `ALTER ROLE
... SET` session defaults. Each also takes optional flags beyond the object
itself: `pretty` on all three, plus `memberships` for roles and
`owner`/`tablespace` for databases. What comes back is a decompiled
reconstruction, not the text you originally typed. Previously this meant reaching for `pg_dumpall
--roles-only` or a third-party script; now it's a plain SQL query, scriptable
from inside any migration tool that already talks to the database.

---

## Ranges: subtracting with gaps

Subtracting a range out of the middle of another has been possible since
multiranges arrived in PG 14 — `datemultirange(a) - datemultirange(b)`
returns a multirange with a gap in it. What PG 19 adds is the convenient
form: `range_minus_multi()` and `multirange_minus_multi()` take plain
ranges and hand you back a `setof anyrange`, one row per surviving
fragment, with no multirange wrapping and unwrapping in between.

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
that by returning a set instead. (`multirange_minus_multi()` always returns
a single row, since one multirange can already hold any result.)

---

## Two small functions that pull their weight

Not everything in a release needs a section of its own, but these two are
short enough to show and useful often enough to remember.

### random() over dates and timestamps

`random()` has returned a `double precision` between 0 and 1 since forever,
and PG 17 added integer and numeric ranges. PG 19 completes the set with
`date`, `timestamp` and `timestamptz` versions, which is exactly what you
want when generating test data over a period:

```sql
select setseed(0.42);

select random('2017-03-26'::date, '2017-11-26'::date) as race_day,
       random(
         timestamp '2017-03-26 12:00', timestamp '2017-03-26 16:00'
       ) as lights_out
  from generate_series(1, 5);
```

```results
  race_day  |         lights_out         
------------+----------------------------
 2017-10-02 | 2017-03-26 13:43:47.198578
 2017-11-20 | 2017-03-26 12:18:25.009763
 2017-04-19 | 2017-03-26 13:01:14.162498
 2017-07-29 | 2017-03-26 13:51:27.212232
 2017-08-20 | 2017-03-26 15:35:52.873462
```

Bounds are inclusive, and the `setseed()` call is there so that run
repeats — drop it and you get fresh values each time. Before this you wrote
the arithmetic yourself, something like `'2017-03-26'::date + (random() *
245)::int`, which works but has to be re-derived every time and quietly gets
the endpoints wrong about half the time anybody writes it.

### error_on_null()

`error_on_null(x)` returns `x`, or raises if `x` is `NULL`. That sounds
almost too small to bother with until you think about where `NULL` comes
from in a query you did not expect it in — a scalar subquery that matched
nothing:

```sql
select (select driverid from f1db.drivers where surname = 'Raikkonen');
```

```results
 driverid 
----------
         
```

No error. One row, one `NULL`, because the surname is spelled `Räikkönen`
and a scalar subquery that matches nothing is a perfectly legal `NULL`. That
value then flows into whatever comes next — a join that quietly returns no
rows, an `IN` list that never matches, an arithmetic expression that turns
the whole column `NULL`. It is one of the great silent bugs in SQL, and the
usual advice is to notice it in review.

Now you can just say what you meant:

```sql
select error_on_null(
  (select driverid from f1db.drivers where surname = 'Raikkonen')) as driverid;
```

```results
ERROR:  null value not allowed
```

Spell it correctly and it returns the value and gets out of the way:

```sql
select error_on_null(
  (select driverid from f1db.drivers where surname = 'Räikkönen')) as driverid;
```

```results
 driverid 
----------
        8
```

It is polymorphic — `anyelement` in, the same type out — so it drops into an
expression anywhere without a cast, and it costs you a comparison. Think of
it as an assertion you can write inline, in the place where the assumption
actually lives, rather than in a comment above the query.

---

## SQL/PGQ: graph patterns over the tables you already have

PostgreSQL 19 implements [SQL/PGQ](https://www.iso.org/standard/79473.html),
Part 16 of the SQL standard, which lets you query relational tables using
graph pattern syntax. It is worth being clear about what that does and does
not mean. A property graph is *not* a new storage engine and not a copy of
your data: `CREATE PROPERTY GRAPH` behaves like `CREATE VIEW`, recording a
logical structure that is resolved at query time against the same tables you
already have. Permissions come from the base relations, not from the graph.

You declare which tables are vertices, which are edges, and how the edges
connect. The Lab's `geoname` schema has exactly this shape already: `country`
keyed by `isocode`, and `neighbour` holding pairs of bordering countries with
a foreign key at each end.

```sql
create property graph borders
  vertex tables (
    geoname.country key (isocode) label country properties (name, iso)
  )
  edge tables (
    geoname.neighbour key (isocode, neighbour)
      source key (isocode) references country(isocode)
      destination key (neighbour) references country(isocode)
      label borders
  );
```

Now the graph can be pattern-matched with `GRAPH_TABLE`, which takes a
`MATCH` pattern and a `COLUMNS` projection and returns an ordinary relation:

```sql
  select neighbour
    from graph_table (borders
               match (c is country where c.name = 'France')
                     -[is borders]->(n is country)
             columns (n.name as neighbour))
order by neighbour;
```

```results
  neighbour  
-------------
 Andorra
 Belgium
 Germany
 Italy
 Luxembourg
 Monaco
 Spain
 Switzerland
```

`(c is country)` is a vertex with a label, `-[is borders]->` is a directed
edge, and `columns` decides what comes back. That query is a join written in
a different shape, and the documentation says so itself — it gives the
equivalent `SELECT ... JOIN` right beside its own example. Which is rather
the point: the graph is a *way of asking*, over data that stays exactly where
it was, with the permissions it already had.

### What lands in a later release

The next thing you will want is a longer question. Not "who borders France"
but "how far does France reach". In the SQL/PGQ standard you say that with a
quantifier on the edge pattern — one to four hops:

```sql
  select distinct reachable
    from graph_table (borders
               match (c is country where c.name = 'France')
                     -[is borders]->{1,4}(n is country)
             columns (n.name as reachable))
order by reachable;
```

```results
ERROR:  element pattern quantifier is not supported
```

Not yet, then. Variable-length paths are the next patch, and with them the
things that surround them: nested path patterns, several path patterns in one
`GRAPH_TABLE`, subqueries inside it, aggregates and window functions in
`COLUMNS`, `ANY SHORTEST` and `ALL SHORTEST`.

Notice what the error message is, though. It is not a syntax error — the
parser understood the quantifier perfectly and told you precisely which part
of the standard has not been wired up yet. The grammar is in place, and it
knows the shape of what is coming.

You can spell a fixed number of hops out by hand, and that does work:

```sql
  select distinct two_hops
    from graph_table (borders
               match (a is country where a.name = 'France')
                     -[is borders]->(b is country)
                     -[is borders]->(c is country)
             columns (c.name as two_hops))
order by two_hops
   limit 8;
```

```results
 two_hops  
-----------
 Andorra
 Austria
 Belgium
 Czechia
 Denmark
 France
 Germany
 Gibraltar
```

That is one pattern per depth, and it returns France itself, because nothing
stops the walk revisiting a vertex — `TRAIL` and `ACYCLIC`, the standard's way
of saying "don't do that", arrive with the quantifiers. So for reachability,
shortest paths and transitive closure, keep reaching for `WITH RECURSIVE`,
which has answered those questions since PostgreSQL 8.4 and is not going
anywhere. It is what I used on this very dataset in the [PG 11–18
round-up](/blog/2026/07/sql-improvements-in-postgresql-1118-a-personal-selection/):
that map of everywhere you can drive from France in four hops is a recursive
CTE, and a good one.

### Why this is the right amount to ship

It would be easy to read "no variable-length paths" as PGQ arriving
half-finished. I would read it the other way round.

What landed is the part that is tedious, invasive and hard to change later:
five new system catalogs, a parser that understands the full pattern grammar,
name and label resolution, permission checks that defer to the base tables,
and a rewriter that turns a matched pattern into an ordinary plan. None of
that is glamorous, and all of it is load-bearing. Variable-length paths are a
genuinely hard planning problem — you are asking the optimizer to cost a
recursive walk — and they are much better attempted on top of a settled
foundation than alongside one.

That ordering is a choice, and it is the same one visible in the `MERGE
PARTITIONS` story earlier: get the semantics right, ship the part you are sure
of, leave the hard part for when it can be done properly. It is what a release
process looks like when the engineers decide what is ready, rather than a
calendar or a feature-comparison table. Oracle 23ai shipped SQL/PGQ first, and
PostgreSQL is not racing it.

What you get today is real: property graphs are declared over tables you
already have, cost nothing to maintain, and let you write a pattern instead of
a join chain — with the standard's syntax, so what you learn now is what you
will use later. The rest builds on this, one release at a time. That is how
PostgreSQL has always gotten where it is going, and it is why the pieces still
fit together twenty years on.

---

## Worth watching, not yet covered here

This article stops at the SQL layer, and PostgreSQL 19 is a large release.
Here is what I left out, with a note on why each one is worth its own
treatment rather than a paragraph.

**Planner stability**

- **`pg_plan_advice`** and **`pg_stash_advice`** — two new extensions for
  stabilizing planner decisions: the first lets you record and replay a
  known-good plan shape, the second applies stored advice automatically
  based on the incoming query. Aimed squarely at the "the plan changed
  after a statistics update and now everything is slow" problem. This is
  the release's most consequential feature for anyone running a large
  OLTP system, and it deserves a proper workout rather than a summary —
  plan hinting has been the single longest-running argument in this
  community, and what shipped is a more interesting answer than either
  camp was asking for.

**Replication**

- **Logical replication now replicates sequence values.** If you have ever
  cut over to a logical replica and discovered every `nextval()` handing
  back a number the publisher used months ago, this is the release that
  fixes it. `CREATE PUBLICATION ... FOR ALL SEQUENCES`, `ALTER
  SUBSCRIPTION ... REFRESH PUBLICATION SEQUENCES`, and a new
  `pg_get_sequence_data()` to see what the subscriber thinks it has.
- **Logical replication no longer needs a restart.** With `wal_level =
  replica`, PG 19 raises the effective level automatically when a slot
  first needs it, and reports what is actually in force through the new
  read-only `effective_wal_level`. The prerequisite that made "just add
  logical replication" a maintenance-window conversation is gone.
- **`WAIT FOR`** — blocks until a standby has replayed WAL to a given
  point, giving read-your-writes against a replica without polling
  `pg_stat_replication`. Small in surface area, but it changes what you
  can safely route to a standby, which is an architecture question.
- Also in this area: `retain_dead_tuples` on a publication for conflict
  detection (with a new `update_deleted` count in
  `pg_stat_subscription_stats`), publications that can *exclude* tables,
  and subscriptions that can borrow `postgres_fdw` connection parameters
  instead of repeating a connection string.

**Operations**

- **Data checksums can be turned on and off while the server runs**
  (`pg_enable_data_checksums()` / `pg_disable_data_checksums()`).
  Previously this meant `pg_checksums` against a stopped cluster, which
  in practice meant most clusters that started without checksums stayed
  without them forever. Now it is a decision you can revisit.
- **Parallel autovacuum** and a **priority scoring system** for which
  tables get vacuumed first. The parallel part is opt-in —
  `autovacuum_max_parallel_workers` defaults to 0 — and only covers the
  index phases. The scoring half is the more interesting one: five
  weighting variables and a `pg_stat_autovacuum_scores` view, which turns
  "why has this table not been vacuumed" into a question with an answer.
- **Automatic scaling of I/O workers**, following through on PG 18's
  asynchronous I/O. If you tuned `io_method = worker` last year, revisit
  it: `io_min_workers` and `io_max_workers` now let the pool size itself.
- **New observability surface**: `pg_stat_lock` for per-lock-type
  statistics, `pg_stat_recovery`, `pg_get_multixact_stats()`, and WAL
  full-page-image byte accounting in `VACUUM`/`ANALYZE` logging. Also
  worth knowing before your log volume changes: `log_lock_waits` is now on
  by default, autovacuum's analyze logging moved to its own
  `log_autoanalyze_min_duration`, and wraparound warnings now start at 100
  million transactions instead of 40 million.
- **Server-side SNI**, so one instance can present different certificates
  by requested hostname.

**Smaller SQL additions, for completeness**

- `oid8`, a 64-bit unsigned integer type.
- `encode()`/`decode()` gain `base64url` and `base32hex`.
- Casts between `bytea` and `uuid`, and more `jsonpath` string methods.
- `GRANT`/`REVOKE ... GRANTED BY`, to name the role doing the granting.
- Full-text stemmers for Polish and Esperanto.

---

Working these examples up for the new edition of [*The Art of
PostgreSQL*](https://theartofpostgresql.com) is what sent me through the PG
19 notes in this much detail — several of them map straight onto existing
chapters on temporal ranges, partitioning, constraints and window functions,
which is a good sign that the release is filling real gaps rather than
adding surface area.

+++
title     = "Plan Advice in PostgreSQL 19"
date      = "2026-09-15T09:00:00+0200"
tags      = ["PostgreSQL", "SQL", "Performance"]
categories = ["PostgreSQL", "Performance"]
icon      = "🐘"
+++

There is a conversation that happens in every PostgreSQL shop eventually. A
query that has been fine for a year gets slow overnight. Nothing was
deployed. The data grew a little, `ANALYZE` ran, and the planner — entirely
reasonably, on the numbers it had — picked a different plan. The old plan
was better. You would like it back.

PostgreSQL 19 ships two new modules for exactly this: `pg_plan_advice`,
which can read a plan back out as a string and enforce it later, and
`pg_stash_advice`, which keeps those strings keyed by query id and applies
them automatically.

{{< lab >}}
Every query below ran against [the Lab](https://theartofpostgresql.com/lab/),
the free dataset bundle used throughout this blog, on PostgreSQL 19 Beta 3:
`POSTGRES_VERSION=19beta3 PG_MAJOR=19 docker compose up`. Both modules are
contrib, and the Lab image ships them. Add them to
`shared_preload_libraries` — `pg_plan_advice,pg_stash_advice` — or load
`pg_plan_advice` into a single session with `LOAD 'pg_plan_advice'`.
{{< /lab >}}

<!--more-->

<!--toc-->

---

## Reading a plan back out

Start with a query that joins three F1 tables:

```sql
  select drivers.surname, count(*) as races
    from f1db.results
    join f1db.races using(raceid)
    join f1db.drivers using(driverid)
   where races.year = 2017
group by drivers.surname
order by races desc, drivers.surname
   limit 5;
```

```results
  surname   | races 
------------+-------
 Bottas     |    11
 Ericsson   |    11
 Grosjean   |    11
 Hamilton   |    11
 Hülkenberg |    11
```

Nothing remarkable. Now ask the planner not just what it did, but to
describe what it did in a form it can read back:

```sql
 explain (costs off, plan_advice)
  select drivers.surname, count(*) as races
    from f1db.results
    join f1db.races using(raceid)
    join f1db.drivers using(driverid)
   where races.year = 2017
group by drivers.surname;
```

```results
                        QUERY PLAN                        
----------------------------------------------------------
 HashAggregate
   Group Key: drivers.surname
   ->  Hash Join
         Hash Cond: (results.driverid = drivers.driverid)
         ->  Hash Join
               Hash Cond: (results.raceid = races.raceid)
               ->  Seq Scan on results
               ->  Hash
                     ->  Seq Scan on races
                           Filter: (year = 2017)
         ->  Hash
               ->  Seq Scan on drivers
 Generated Plan Advice:
   JOIN_ORDER(results races drivers)
   HASH_JOIN(races drivers)
   SEQ_SCAN(results races drivers)
   NO_GATHER(results races drivers)
```

That trailing block is the whole idea. Four lines, describing four
decisions the planner made: which table drives the join and in what order,
which join method to use, how to reach each relation, and whether to go
parallel.

{{< image src="fig-advice-anatomy.svg" title="The same plan expressed twice: as a plan tree on the left, and as four lines of advice on the right. Join order, join method, scan method and parallelism each get one line, colour-matched to the part of the tree they describe." >}}

`JOIN_ORDER(results races drivers)` says `results` is the driving table,
joined first to `races` and then to `drivers`. `HASH_JOIN(races drivers)`
says each of those belongs on the *inner* side of a hash join.
`SEQ_SCAN(...)` and `NO_GATHER(...)` say how to read each table and that
none of it should run in parallel.

Note what is *not* here. There is no cost, no row estimate, no timing —
advice describes outcomes, not the reasoning that produced them. That is a
deliberate design choice, and it is the reason advice survives a change in
statistics: it does not mention any.

---

## Making the planner take it

Feed a string back through `pg_plan_advice.advice` and the planner is
obliged to follow it. Here is the same query, told to drive from `drivers`
instead:

```sql
set pg_plan_advice.advice = 'JOIN_ORDER(drivers results races)';

 explain (costs off, plan_advice)
  select drivers.surname, count(*) as races
    from f1db.results
    join f1db.races using(raceid)
    join f1db.drivers using(driverid)
   where races.year = 2017
group by drivers.surname;
```

```results
SET
                                 QUERY PLAN                                  
-----------------------------------------------------------------------------
 Finalize GroupAggregate
   Group Key: drivers.surname
   ->  Sort
         Sort Key: drivers.surname
         ->  Hash Join
               Hash Cond: (results.raceid = races.raceid)
               ->  Merge Join
                     Merge Cond: (drivers.driverid = results.driverid)
                     ->  Sort
                           Sort Key: drivers.driverid
                           ->  Seq Scan on drivers
                     ->  Sort
                           Sort Key: results.driverid
                           ->  Partial HashAggregate
                                 Group Key: results.raceid, results.driverid
                                 ->  Seq Scan on results
               ->  Hash
                     ->  Seq Scan on races
                           Filter: (year = 2017)
 Supplied Plan Advice:
   JOIN_ORDER(drivers results races) /* matched */
 Generated Plan Advice:
   JOIN_ORDER(drivers results races)
   MERGE_JOIN_PLAIN(results)
   HASH_JOIN(races)
   SEQ_SCAN(drivers results races)
   NO_GATHER(results races drivers)
```

Two things to notice. The plan really did change — `drivers` is now the
outer relation, and the planner reached for a merge join to get there.
And the output now carries *two* blocks: `Supplied Plan Advice`, echoing
what you asked for with a `/* matched */` annotation, and
`Generated Plan Advice` describing the plan you actually got.

That round trip is the feature. You can take the generated advice from a
plan you liked, hand it back later, and confirm from the `matched`
annotations that every piece of it landed.

---

## When advice does not win

Advice constrains the planner's choice *among plans it would consider*. It
does not resurrect plans that have been taken off the table. Turn off hash
joins and ask for one anyway:

```sql
reset pg_plan_advice.advice;

set enable_hashjoin = off;

set pg_plan_advice.advice = 'JOIN_ORDER(results races drivers) HASH_JOIN(races)';

 explain (costs off)
  select drivers.surname, count(*) as races
    from f1db.results
    join f1db.races using(raceid)
    join f1db.drivers using(driverid)
   where races.year = 2017
group by drivers.surname;
```

```results
RESET
SET
SET
                              QUERY PLAN                               
-----------------------------------------------------------------------
 Finalize GroupAggregate
   Group Key: drivers.surname
   ->  Sort
         Sort Key: drivers.surname
         ->  Nested Loop
               ->  Nested Loop
                     Disabled: true
                     ->  Partial HashAggregate
                           Group Key: results.raceid, results.driverid
                           ->  Seq Scan on results
                     ->  Index Scan using idx_49556_primary on races
                           Index Cond: (raceid = results.raceid)
                           Filter: (year = 2017)
               ->  Index Scan using idx_49514_primary on drivers
                     Index Cond: (driverid = results.driverid)
 Supplied Plan Advice:
   JOIN_ORDER(results races drivers) /* matched */
   HASH_JOIN(races) /* matched, failed */
```

`JOIN_ORDER` matched. `HASH_JOIN(races)` reports `matched, failed` — the
advice was understood, it applied to the right part of the query, and the
planner still could not honour it.

This annotation is the most useful thing in the whole interface, and it is
easy to walk past. Advice that silently does nothing would be worse than no
advice at all; you would carry a string in your configuration for two years
believing it was holding a plan in place. Here you can check.

---

## Stashing advice by query id

Setting `pg_plan_advice.advice` by hand works for experimenting, but you
cannot ask an application to do it. `pg_stash_advice` closes that gap: it
maps query ids to advice strings in shared memory, and applies them to any
query whose id matches.

```sql
create extension if not exists pg_stash_advice;

select pg_create_advice_stash('production');

select pg_set_stashed_advice(
         'production', -5243066567089054587,
         'JOIN_ORDER(drivers results races)'
       );

select * from pg_get_advice_stash_contents('production');
```

```results
NOTICE:  extension "pg_stash_advice" already exists, skipping
CREATE EXTENSION
 pg_create_advice_stash 
------------------------
 
(1 row)

 pg_set_stashed_advice 
-----------------------
 
(1 row)

 stash_name |       query_id       |           advice_string           
------------+----------------------+-----------------------------------
 production | -5243066567089054587 | JOIN_ORDER(drivers results races)
(1 row)
```

The query id comes from `EXPLAIN (VERBOSE)`, or — more usefully — from
`pg_stat_statements`, which is where you were already looking when you
noticed the query had got slow. From then on, the application changes
nothing:

```sql
set pg_stash_advice.stash_name = 'production';

 explain (costs off)
  select drivers.surname, count(*) as races
    from f1db.results
    join f1db.races using(raceid)
    join f1db.drivers using(driverid)
   where races.year = 2017
group by drivers.surname;
```

```results
SET
                                 QUERY PLAN                                  
-----------------------------------------------------------------------------
 Finalize GroupAggregate
   Group Key: drivers.surname
   ->  Sort
         Sort Key: drivers.surname
         ->  Hash Join
               Hash Cond: (results.raceid = races.raceid)
               ->  Hash Join
                     Hash Cond: (drivers.driverid = results.driverid)
                     ->  Seq Scan on drivers
                     ->  Hash
                           ->  Partial HashAggregate
                                 Group Key: results.raceid, results.driverid
                                 ->  Seq Scan on results
               ->  Hash
                     ->  Seq Scan on races
                           Filter: (year = 2017)
 Supplied Plan Advice:
   JOIN_ORDER(drivers results races) /* matched */
```

No `LOAD`, no `SET advice`, no rewritten query. The plan changed because
the stash matched the query id.

{{< image src="fig-advice-lifecycle.svg" title="The plan advice workflow: find the query in pg_stat_statements, read its plan back with EXPLAIN (PLAN_ADVICE), keep only the lines that matter, and stash it by query id. The dashed return path is the step people forget." >}}

---

## The part the documentation says twice

Both modules' documentation carries the same warning, and it is worth
repeating rather than paraphrasing: the planner's ability to change its
mind as the data changes is a feature. Advice takes that away. If the
distribution shifts under a pinned plan, you get the old plan applied to
new data, which is exactly the failure the planner exists to prevent.

The README is blunter still — bad advice producing a bad plan is
"user error, not a defect in this module".

So the discipline that makes this useful is *trimming*. The generated
string describes every decision, but you almost never want to pin every
decision. If the problem is that the join order flipped, keep
`JOIN_ORDER(...)` and delete the rest; the planner keeps its freedom
everywhere else, and the one thing you needed stays fixed. The README
works through a star-schema example on exactly this point: dropping the
`JOIN_ORDER` line while keeping the join methods gives the planner room to
reorder while still forcing hash joins where you wanted them.

Advice is also not free. Applying it costs planning time even when the plan
does not change, which is another reason to reach for it per-query rather
than cluster-wide.

---

## What this replaces

If you have run PostgreSQL at scale you have met the alternatives. There is
`pg_hint_plan`, which is an out-of-tree extension with hints embedded in
query comments. There is the `enable_*` family, which is a blunt
per-session instrument. There is `plan_cache_mode`, which addresses a
narrower problem. And there is the traditional answer — restructure the
query until the planner agrees with you — which works but is not available
when the query comes out of an ORM you do not control.

What is new here is the round trip: a plan can be *read out*, and the same
string put back. You are not writing hints from first principles and hoping
they describe the plan you remember. You are keeping a plan you measured.

That is a smaller feature than a hint language, and a much more useful one.

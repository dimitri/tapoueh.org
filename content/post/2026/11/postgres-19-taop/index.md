+++
title     = "What Postgres 19 changes in The Art of PostgreSQL"
date      = "2026-11-25T18:00:00+0100"
tags      = ["PostgreSQL", "SQL", "Performance"]
categories = ["PostgreSQL", "SQL"]
icon      = "🐘"
+++

Postgres 19.0 was released on November 19. We covered the beta in
September, in [Getting Ready for PostgreSQL
19](/blog/2026/09/getting-ready-for-postgresql-19/) and [Plan Advice in
PostgreSQL 19](/blog/2026/09/plan-advice-in-postgresql-19/). With the
release out, we read the final release notes against everything we teach:
the book's chapters, the six courses, their quizzes and exercises. This
article is the result. It covers what 19 makes wrong in our material, what it
gives us to teach, and what we find most interesting in it.

The release-day posts mostly lead with `REPACK`, plan advice and `ON
CONFLICT DO SELECT`. Our review lands somewhere else. The change with the
most consequences for SQL material is quieter: the planner now reads your
`NOT NULL` constraints and rewrites queries with them.

{{< lab >}}
Every query below ran on Postgres 19.0 with the Lab's data, and on 18.6
wherever we compare the two. The Lab still starts Postgres 16 by default; a
seeded image for 19 is on its way.
{{< /lab >}}

<!--more-->

<!--toc-->

---

## TL;DR

- **`NOT IN` and `LEFT JOIN … IS NULL` now become anti joins** when `NOT
  NULL` constraints prove there are no NULLs. A `CHECK (x IS NOT NULL)`
  doesn't count. The `NOT IN` trap is unchanged, but the reason we gave
  for avoiding it is now only half true.
- **Eager aggregation is on by default**, so a `GROUP BY` over a join can
  now aggregate below the join.
- **Built-in functions now shadow helpers we defined.** `random(int, int)`
  has been built in since 17 and `random(timestamptz, timestamptz)` since
  19, and both win over the book's own versions without a word.
- **Full-text search and `pg_trgm` need a reindex after the upgrade** on
  ICU and builtin-collation databases, and stemmers changed. Our own
  earlier explanation of the PG 18 step was wrong.
- **Our EXPLAIN captures all need a re-run.** JIT is off by default,
  subplans have new names, and 18's output changes are in there too.
- **To teach:** `IGNORE NULLS`, `ON CONFLICT DO SELECT`, `CREATE SCHEMA`
  with forward references, `COPY` with `on_error set_null` and JSON output,
  `REPACK`, and a set of tools for keeping plans stable.
- **To remove:** a `FOR PORTION OF` preview in a course. It was withdrawn
  before the release.

---

## What made it into 19.0

Seven features were withdrawn between the first beta and the release.

| Feature | Withdrawn |
|---|---|
| Non-text output formats for `pg_dumpall` | June 18 |
| `GROUP BY ALL` | July 17 |
| `ALTER TABLE … MERGE/SPLIT PARTITION(S)` | August 27 |
| SQL/PGQ property graphs | September 7 |
| `pg_get_role_ddl()` and its siblings | mid-September |
| `UPDATE/DELETE … FOR PORTION OF` | September 15 |
| Online enabling and disabling of data checksums | September 16 |

We wrote up four of them in September, with what went wrong in each one. One
of them had also made it into a course: the Reliable Aggregation course had a
"PostgreSQL 19 preview" on `FOR PORTION OF`, written against a beta. That
section now goes. A temporal update is again a delete and re-insert, and 19's
`range_minus_multi()` computes the pieces left over:

```sql
select range_minus_multi(
         daterange('2014-01-01', '2021-01-01'),
         daterange('2017-01-01', '2020-01-01')
       );
```

```results
    range_minus_multi
-------------------------
 [2014-01-01,2017-01-01)
 [2020-01-01,2021-01-01)
```

The lesson for us is about writing ahead of a release. A feature committed to
the development branch is not a feature of the release until the release
ships, and this cycle proved it seven times.

---

## What 19 makes wrong in what we teach

### NOT IN is no longer the slow one

The book and the Advanced Joins course both warn against `NOT IN` with a
subquery. The reason is correct and stays correct: if the subquery returns a
single `NULL`, `x NOT IN (…)` is never true, and the query quietly returns
nothing. Both then add a second argument: prefer `NOT EXISTS`, because that's
the form the planner can run as a hash anti join.

That second argument changed in 19. When the planner can prove that neither
side contains a `NULL`, it now converts `NOT IN` into an anti join too.
Here's the same query on both versions. Both `driverid` columns are declared
`NOT NULL`:

```sql
explain (costs off)
 select surname
   from f1db.drivers d
  where driverid not in (select driverid
                           from f1db.results r
                          where r.position = 1);
```

On 18.6:

```results
                         QUERY PLAN
------------------------------------------------------------
 Seq Scan on drivers d
   Filter: (NOT (ANY (driverid = (hashed SubPlan 1).col1)))
   SubPlan 1
     ->  Seq Scan on results r
           Filter: ("position" = 1)
```

On 19.0:

```results
               QUERY PLAN
----------------------------------------
 Hash Right Anti Join
   Hash Cond: (r.driverid = d.driverid)
   ->  Seq Scan on results r
         Filter: ("position" = 1)
   ->  Hash
         ->  Seq Scan on drivers d
```

The proof comes from the schema. `drivers.number` is nullable, and a `NOT IN`
over it keeps the old plan on 19:

```sql
explain (costs off)
 select surname
   from f1db.drivers d
  where number not in (select number from f1db.results);
```

```results
                          QUERY PLAN
--------------------------------------------------------------
 Seq Scan on drivers d
   Filter: (NOT (ANY (number = (hashed SubPlan any_1).col1)))
   SubPlan any_1
     ->  Seq Scan on results
```

It has to be a real `NOT NULL` constraint. We tried with two otherwise
identical tables: on the one whose column has `check (id is not null)`, the
`NOT IN` stays a hashed SubPlan. On the one whose column is declared `not
null`, it becomes a `Hash Anti Join`.

The same reasoning now applies to an outer join used as an anti join, when
the `IS NULL` test is on a column the schema declares `NOT NULL`:

```sql
explain (costs off)
 select d.surname
   from f1db.drivers d
        left join f1db.results r
               on r.driverid = d.driverid
              and r.position = 1
  where r.resultid is null;
```

18.6 joins every row and filters afterwards:

```results
               QUERY PLAN
----------------------------------------
 Hash Right Join
   Hash Cond: (r.driverid = d.driverid)
   Filter: (r.resultid IS NULL)
   ->  Seq Scan on results r
         Filter: ("position" = 1)
   ->  Hash
         ->  Seq Scan on drivers d
```

19.0 knows that `resultid` can only be `NULL` when no row matched:

```results
               QUERY PLAN
----------------------------------------
 Hash Right Anti Join
   Hash Cond: (r.driverid = d.driverid)
   ->  Seq Scan on results r
         Filter: ("position" = 1)
   ->  Hash
         ->  Seq Scan on drivers d
```

So we keep the rule and rewrite the reason. Prefer `NOT EXISTS` because its
meaning doesn't depend on `NULL`s, and declare `NOT NULL` wherever it's
true, because the planner now uses it. Those two conversions are part of a
series of changes in 19 that all read nullability:

- `IS DISTINCT FROM` becomes plain `=` on non-nullable inputs.
- `x IS NOT DISTINCT FROM NULL` becomes `x IS NULL`.
- `COALESCE()` stops at its first non-nullable argument.
- `IS TRUE` and `IS FALSE` on a non-nullable input become plain boolean
  tests.

That changes one of our recipes. The Data Modeling course adds a `NOT NULL`
to a large table in two steps: `ADD CONSTRAINT … CHECK (col IS NOT NULL) NOT
VALID`, then `VALIDATE CONSTRAINT`. That never produces a `NOT NULL`
constraint, so 19 can't use it for these rewrites. Since 18 the direct form
exists, `ALTER TABLE … ADD CONSTRAINT … NOT NULL col NOT VALID`, followed by
a separate `VALIDATE`. That's the recipe we now teach.

### Our own random() functions are no longer called

To generate test data, the book defines two helpers, `random(int, int)` and
`random(timestamptz, timestamptz)`, and says they "don't exist in the
distribution of PostgreSQL". That stopped being true for the first one in 17
and for the second in 19. Built-in functions live in `pg_catalog`, which is
searched first, so the helpers are still installed but no longer called. On
18.6, with the book's timestamp helper in `public`, the call is inlined from
our SQL function:

```sql
explain (verbose, costs off)
 select random(timestamptz '2017-01-01', timestamptz '2017-12-31');
```

```results
Result
  Output: ('2017-01-01 00:00:00+00'::timestamp with time zone + ((random(0, 31449600))::double precision * '00:00:01'::interval))
```

On 19.0, with the same helper installed:

```results
Result
  Output: random('2017-01-01 00:00:00+00'::timestamp with time zone, '2017-12-31 00:00:00+00'::timestamp with time zone)
```

Nothing errors. Only the numbers change, and the built-in is the better
one. Our integer helper computed `a + ((b - a) * random())::int`. The
cast rounds, so the two end values come out half as often as the ones in
between. Here are 300,000 draws between 1 and 3:

| value | book helper | built-in |
|---|---:|---:|
| 1 | 74,971 | 100,094 |
| 2 | 150,520 | 99,655 |
| 3 | 74,509 | 100,251 |

The helpers go, and the text now points at `random(min, max)`, which 19
extends to `date`, `timestamp` and `timestamptz`.

### Full-text search and pg_trgm after an upgrade

The book's trigram chapter, and our [PG 11–18 round-up in
July](/blog/2026/07/sql-improvements-in-postgresql-1118-a-personal-selection/#pg_trgm-rebuild-indexes-after-upgrading-to-pg-18),
say that PG 18 made `pg_trgm` lower-case strings consistently, and that every
trigram index must be rebuilt after the upgrade. That's not what the 18
release notes say. Here's what changed in each version:

- **In 18,** full-text search started reading its configuration files and
  dictionaries with the cluster's default collation provider instead of
  always using libc. Only clusters whose provider is ICU or builtin can see
  different results, and for those the notes recommend reindexing full-text
  and `pg_trgm` indexes after `pg_upgrade`.
- **In 19,** the same move reaches character classification when parsing
  text. On those databases, the recommendation is to reindex the full-text
  and `pg_trgm` indexes and also to recompute any stored `tsvector`.
- **Also in 19,** the Snowball stemmers were updated. Dutch moved to a
  different algorithm (the previous one is kept as `dutch_porter`). English,
  French, German, Spanish and others now produce different stems for some
  words. A `tsvector` stored before the upgrade keeps the old stems, while
  `to_tsquery()` produces the new ones.

The book recommends the builtin `C.UTF-8` collation for stable sort order,
which is one of the configurations these notes are about. The two chapters
now point at each other, and the upgrade section covers the three changes
above.

### Smaller corrections

- **`json_array()` over an empty query now returns `[]`.** It used to
  return `NULL`:

  ```sql
  select json_array(select 1 where false) as empty_json_array;
  ```

  ```results
   empty_json_array
  ------------------
   []
  ```

- **JIT is off by default.** The query plans course has a module on JIT
  compilation, and its example now needs `set jit = on`. The `JIT:` block
  also disappears from every other plan we captured.
- **Subplans have new names.** `SubPlan 1` is now `SubPlan expr_1`,
  `SubPlan any_1` or `exists_1`, depending on what the subplan computes. The
  release notes don't mention it, and course text that quotes `SubPlan 1`
  needs a fix.
- **Our captured plans come from Postgres 16.** That predates 18's
  changes: fractional `rows=`, `Buffers:` printed by default with `ANALYZE`,
  `Index Searches:` and B-tree skip scan. Skip scan also weakens the
  leftmost-prefix rule in the indexing module, where an index on `(a, b)`
  could not serve a filter on `b` alone.
- **Errors that have nothing to do with 19.** The review also found these:
  - `uuidv7()` is from 18, not 17.
  - `MERGE … WHEN NOT MATCHED BY SOURCE` is from 17, not 15.
  - A pg_stat_statements query used `blk_read_time`, which was renamed in
    17. The query fails on every supported version since.

---

## What 19 gives us to teach

### Eager aggregation

Both the aggregation course and the joins course teach "aggregate, then
join": when you sum points per constructor, sum them per `constructorid`
first and join the names afterwards. In 19 the planner can do that by
itself, and `enable_eager_aggregate` is on by default:

```sql
explain (costs off)
  select c.name, sum(r.points)
    from f1db.results r
         join f1db.constructors c using (constructorid)
group by c.name;
```

On 18.6, all 26,576 results go through the join before the aggregate:

```results
                       QUERY PLAN
--------------------------------------------------------
 HashAggregate
   Group Key: c.name
   ->  Hash Join
         Hash Cond: (r.constructorid = c.constructorid)
         ->  Seq Scan on results r
         ->  Hash
               ->  Seq Scan on constructors c
```

On 19.0, a `Partial HashAggregate` reduces them to one row per constructor
below the join, and a `Finalize` step merges the groups that share a name:

```results
                          QUERY PLAN
--------------------------------------------------------------
 Finalize GroupAggregate
   Group Key: c.name
   ->  Sort
         Sort Key: c.name
         ->  Hash Join
               Hash Cond: (r.constructorid = c.constructorid)
               ->  Partial HashAggregate
                     Group Key: r.constructorid
                     ->  Seq Scan on results r
               ->  Hash
                     ->  Seq Scan on constructors c
```

Partial and Finalize nodes used to appear only with parallel query, and the
query plans course explains them in that context. They now show up on a
single process too, which is worth a paragraph of its own. We still teach
aggregating before the join explicitly. It also gives the right answer when
the join duplicates rows, which no planner can do for you.

### Window functions that skip NULLs

`lag()`, `lead()`, `first_value()`, `last_value()` and `nth_value()` accept
`IGNORE NULLS`. The September article has a worked example on Räikkönen's
2017 season. The window functions course gets it in three modules:
row navigation, frame edge cases, and gaps and islands, where carrying the
last known value forward used to take a subquery.

### Get-or-create in one statement

`INSERT … ON CONFLICT DO SELECT … RETURNING` returns the row that already
exists, optionally locked `FOR UPDATE` or `FOR SHARE`. The book's
concurrency chapter has carried a commented-out section on row locks and
`ON CONFLICT` for a while. This is the feature that finally gets it
written, alongside two other 19 changes: `log_lock_waits` is now on by
default, and the new `pg_stat_lock` view reports per-lock-type statistics.

### A whole model in one CREATE SCHEMA

`CREATE SCHEMA` can create its tables in the same statement, and since 19 a
foreign key can reference a table that appears later in that statement:

```sql
create schema demo_fk
  create table race
   (
     raceid    bigint primary key,
     circuitid bigint references circuit
   )
  create table circuit
   (
     circuitid bigint primary key,
     name      text
   );
```

18.6 stops at the first table with `ERROR: relation "circuit" does not
exist`, and 19.0 creates both. The data modeling chapters write their models
as a single script, and they no longer need to order the tables by their
dependencies.

### COPY

The book opens with a data loading example, and `COPY` gains three things in
19.

`on_error set_null` loads the row and puts a `NULL` in each field that
doesn't parse. `on_error ignore` (since 17) skips the whole row:

```sql
create temp table t_import (id int, points numeric, label text);

copy t_import from stdin (format csv, on_error set_null);
1,10.5,ok
2,DNF,bad points
three,3,bad id
\.

table t_import;
```

```results
NOTICE:  in 2 rows, columns were set to null due to data type incompatibility
 id | points |   label
----+--------+------------
  1 |   10.5 | ok
  2 |        | bad points
    |      3 | bad id
```

`HEADER` now takes a number, to skip several header lines. And `COPY TO`
writes JSON, one object per line, or one array with `force_array`:

```sql
copy (select driverid, code, surname
        from f1db.drivers
    order by driverid
       limit 3)
  to stdout (format json, force_array);
```

```results
[
 {"driverid":1,"code":"HAM","surname":"Hamilton"}
,{"driverid":2,"code":"HEI","surname":"Heidfeld"}
,{"driverid":3,"code":"ROS","surname":"Rosberg"}
]
```

### Keeping a good plan

The [plan advice article](/blog/2026/09/plan-advice-in-postgresql-19/)
covers `pg_plan_advice` and `pg_stash_advice`. In the query optimization
course they move from a preview to a module. Other 19 changes answer
neighbouring questions in the same course:

- **pg_stat_statements counts generic and custom plans separately**, in
  `generic_plan_calls` and `custom_plan_calls`. When one prepared statement
  has two plans and only one of them is slow, these counts are the first
  clue.
- **Extended statistics can travel with a dump.** `pg_dump --statistics`
  now includes them. `pg_restore_extended_stats()` and
  `pg_clear_extended_stats()` manage them directly.
- **Extended statistics work on virtual generated columns.** The planner
  can now estimate a computed expression correctly without the value being
  stored. The statistics module of the optimization course has used stored
  columns and expression indexes for that until now.
- **`EXPLAIN (ANALYZE, IO)`** reports asynchronous I/O and prefetching per
  node. A Memoize node now prints the planner's estimates, cache capacity
  and expected hit ratio included, so you can compare them with the actual
  hits.

### And the rest, briefly

- **`REPACK`** replaces both `VACUUM FULL` and `CLUSTER` in the book and in
  the bloat module. `REPACK (CONCURRENTLY)` comes with its restrictions:
  - it needs a primary key or a replica identity;
  - it isn't MVCC-safe;
  - it takes a short exclusive lock at the end.
- **Constraints:** `ALTER CONSTRAINT … [NOT] ENFORCED` now works on `CHECK`
  constraints. While rewriting the exchange-rates model around it, we also
  catch up with 18: a temporal primary key with `WITHOUT OVERLAPS` and a
  foreign key with `PERIOD`.
- **SQL/JSON:** the jsonpath string methods (`lower()`, `split_part()`, and
  so on) are immutable, so they can go in an expression index. `IS JSON`
  now works on domains.
- **Replication:**
  - `WAIT FOR LSN` gives read-your-writes on a standby.
  - Logical replication can start without a server restart.
  - Sequences replicate.

  These three rewrite the book's 2017-era section on scaling out.
- **psql:** `\pset display_true` and `\pset display_false` change how
  booleans print, and `%S` puts the `search_path` in the prompt.
- **Migration course:** `standard_conforming_strings` is now always on, so
  MySQL dumps with backslash-escaped strings must be converted before they
  load.

---

## What we find most interesting

**The schema as planner input.** The book argues that constraints belong in
the database: they document the model, and they keep bad data out wherever
the writes come from. Postgres 19 adds a third argument. Most of the
planner work in this release reads nullability, and a column declared `NOT
NULL` now changes plans, while a `CHECK` that says the same thing doesn't.
When a student asks whether a constraint is worth the trouble, we now have a
plan to show them.

**Plan stability as a set of tools.** Plan advice gets the attention, but
19 also counts generic and custom plans, carries extended statistics through
a dump, and allows statistics on computed columns. Together they answer
"why did this query change plan, and how do I keep the good one" better than
any single feature has before.

**A release that shipped late on purpose.** Seven features came out after
the first beta, and the release landed in November rather than the usual
September or October. We said in
September what we think of that, and we still think it.

**A personal note.** 19 also adds event triggers written in PL/Python. I
wrote event triggers in the first place, back in 9.3, and the release notes
credit Euler Taveira and me for this one.

---

## The update plan

The list below is the work, in the order we're doing it:

1. **Move the Lab to Postgres 19 by default,** and say so in one place in
   the book instead of three.
2. **Fix what's wrong.** That means the `FOR PORTION OF` section, the
   `random()` helpers, the full-text search and `pg_trgm` upgrade section,
   the `NOT IN` reasoning, the JIT module, the pg_stat_statements query,
   and the version mistakes listed above.
3. **Re-run every captured output on 19.0.** That's every `EXPLAIN` in the
   book, in the courses and in the exercise solutions, so that the 18 and
   19 output changes show up as they really look.
4. **Write the new material:**
   - `IGNORE NULLS` in the window functions course;
   - the locking section with `ON CONFLICT DO SELECT`;
   - eager aggregation in the aggregation, joins and query plans courses;
   - `NOT NULL` and the planner in data modeling;
   - `COPY` in the opening chapter;
   - `REPACK`;
   - plan stability in the optimization course.
5. **Update the quizzes.** Two book quizzes and one course quiz rest on a
   claim that is no longer true.

Each of those changes ships to readers of the book and students of the
course as it lands.

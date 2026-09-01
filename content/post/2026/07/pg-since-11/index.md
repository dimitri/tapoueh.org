+++
title     = "SQL Improvements in PostgreSQL 11–18: A Personal Selection"
date      = "2026-07-28T16:39:25+0200"
tags      = ["PostgreSQL", "SQL"]
categories = ["PostgreSQL", "SQL"]
icon      = "🐘"
+++

Seven major versions of PostgreSQL shipped between 2018 and 2025, one per
year without exception, and each with a changelog of 150 to 200 user-visible changes.  Each release covers a broad canvas — performance, replication, administration,
and security — but every one of them also advanced the SQL layer, filling gaps
in the standard, adding missing functionality, or cleaning up long-standing
rough edges.  Working through the new edition of *The Art of PostgreSQL* forced me
to catalogue them all; this is my selection of the features I kept reaching for
while rewriting the examples.  I hope it's useful beyond that context —
organized by theme, with the version each feature landed in.

{{< lab >}}
All queries in this article run against [the Lab](https://theartofpostgresql.com/lab/), a free dataset bundle that includes the F1 database, geopolitical data, music data, and more — so you can try every example yourself. The Lab is a `docker compose up` away: it starts PostgreSQL with all datasets loaded and a query UI web app that runs in your browser, with EXPLAIN plan diagrams, and SVG rendering for PostGIS results on geo queries.
{{< /lab >}}

To get a sense of where the community actually puts its effort,
[Noriyoshi Shinoda](https://github.com/nori-shinoda/documents/) of
Hewlett-Packard Enterprise Japan has been publishing a meticulous
"PostgreSQL New Features with Examples" series for every major release
since version 9.4 — each edition catalogues every user-visible change with
a working code example.  Counting his categories across PG 11–18 gives a
clear picture of contributor priorities:

{{< tbl caption="New features by category per release (Shinoda count)" >}}
| Release | SQL | Performance | Admin & Ops | Replication | Security | Other |
|---------|----:|------------:|------------:|------------:|---------:|------:|
| PG 11   |   5 |           6 |           5 |           3 |        4 |     3 |
| PG 12   |   4 |           6 |           7 |           3 |        3 |     5 |
| PG 13   |   7 |           8 |          10 |           6 |        5 |     6 |
| PG 14   |   4 |           7 |           6 |           5 |        3 |     5 |
| PG 15   |   2 |           5 |           6 |           5 |        3 |     8 |
| PG 16   |   4 |           6 |           8 |           5 |        7 |     8 |
| PG 17   |   5 |           7 |           8 |           8 |        4 |     6 |
| PG 18   |   4 |           3 |           4 |           3 |        2 |     5 |
| **Sum** | **35** | **48** | **54** | **38** | **31** | **46** |
{{< /tbl >}}

SQL is the smallest category.  Admin & Ops leads across the whole period,
followed by Performance — which reflects who funds the project: Amazon
(Aurora), Google (AlloyDB), and Microsoft (Azure) all have strong commercial
interests in PostgreSQL being easy to operate, scale, and replicate at cloud
scale.  Replication & HA shows the steepest growth trajectory, climbing from
3 features in PG 11 to 8 in PG 17, tracking the same commercial priorities.
The framing of this article is deliberately SQL-centric: execution strategy, query syntax,
data types, schema modeling, and DML.  Administration, replication, and
authentication improvements are real too; a few that struck me as particularly worth knowing about are collected at the end.

<!--more-->

<!--toc-->

---

Each of the seven releases covered in this article added a landmark SQL feature;
the table below names it alongside the release date and an approximate count of
user-visible changes from the official release notes.

| Version | Released | ~Changes | SQL headline |
|---------|----------|---------|--------------|
| PG 11 | Oct 2018 | 180 | Stored procedures (`CREATE PROCEDURE` / `CALL`), JIT compilation, covering indexes (`INCLUDE`) |
| PG 12 | Oct 2019 | 200 | Generated columns, JSON path language (`@?` / `@@`), CTE inlining, `REINDEX CONCURRENTLY` |
| PG 13 | Sep 2020 | 150 | Incremental sort, B-tree deduplication, `gen_random_uuid()` built-in |
| PG 14 | Sep 2021 | 200 | Multirange types, JSONB subscript syntax (`doc['key']`), SEARCH/CYCLE in CTEs |
| PG 15 | Oct 2022 | 180 | SQL `MERGE`, `NULLS NOT DISTINCT`, row/column filters in logical replication |
| PG 16 | Sep 2023 | 150 | Logical replication on standbys, `pg_stat_io`, `ANY_VALUE()`, `IS JSON` predicate |
| PG 17 | Sep 2024 | 180 | `JSON_TABLE()`, incremental backups, `COPY ON_ERROR IGNORE`, built-in `C.UTF-8` collation |
| PG 18 | Sep 2025 | 150 | Virtual generated columns, `uuidv7()`, `RETURNING OLD/NEW`, async I/O |

Change counts are approximate (±15%) and cover user-visible items in each version's release notes; they exclude point-release bug fixes.

---

What follows is my personal selection of SQL-level improvements from those
seven releases — features I consider key to writing efficient PostgreSQL queries, grouped
by theme.  Knowing what PostgreSQL can do also matters more than ever in the AI
era: you cannot ask an AI to write a window function or a range query — or reach
for a multi-range datatype in your schema — if you do not know those tools
exist.  [There is a longer write-up on SQL and AI here](https://theartofpostgresql.com/sql-and-ai/).

## Query execution

### Window frame GROUPS mode and EXCLUDE (PG 11)

The `ROWS` frame mode counts physical rows; `RANGE` counts by value distance.
PostgreSQL 11 added a third mode, `GROUPS`, which counts *peer groups* —
consecutive rows that share the same ORDER BY value.  This is the natural
unit for ranked or tied data.

{{< image src="fig-window-frame-spec.svg" title="Three window frame specifications shown on a 7-row result set: UNBOUNDED PRECEDING to CURRENT ROW (running total), 1 PRECEDING to 1 FOLLOWING (sliding window), and CURRENT ROW to UNBOUNDED FOLLOWING (remaining total)." >}}

PostgreSQL 11 also added the `EXCLUDE` sub-clause as part of the same
SQL:2011 compliance push — `EXCLUDE CURRENT ROW`, `EXCLUDE TIES`, or
`EXCLUDE GROUP` — which removes specific rows from the frame.
`EXCLUDE TIES` keeps the current row in the frame but removes other rows that
share the same ORDER BY value.  A single race is a bad showcase — every
scoring position has a unique points value, so no ties ever appear.  The 2007
season standings are the right dataset: Hamilton and Alonso both ended on 109
points in the closest championship in F1 history.

```sql
select surname,
       sum(results.points) as season_pts,
       sum(sum(results.points)) over (
           order by sum(results.points) desc
           rows between unbounded preceding and current row
       ) as running,
       sum(sum(results.points)) over (
           order by sum(results.points) desc
           rows between unbounded preceding and current row
           exclude ties
       ) as running_excl_ties
from results
join drivers using(driverid)
join races   using(raceid)
where extract(year from races.date) = 2007
group by driverid, drivers.surname
order by season_pts desc;
```

```results
   surname    | season_pts | running | running_excl_ties
--------------+------------+---------+------------------
 Räikkönen    |        110 |     110 |               110
 Hamilton     |        109 |     219 |               219
 Alonso       |        109 |     328 |               219
 Massa        |         94 |     422 |               422
 Heidfeld     |         61 |     483 |               483
 Kubica       |         39 |     522 |               522
 Kovalainen   |         30 |     552 |               552
 Fisichella   |         21 |     573 |               573
 Rosberg      |         20 |     593 |               593
 Coulthard    |         14 |     607 |               607
 Wurz         |         13 |     620 |               620
 Webber       |         10 |     630 |               630
 Trulli       |          8 |     638 |               638
 Button       |          6 |     644 |               644
 Vettel       |          6 |     650 |               644
 R Schumacher |          5 |     655 |               649
 Sato         |          4 |     659 |               653
 Liuzzi       |          3 |     662 |               656
 Davidson     |          2 |     664 |               658
 Sutil        |          1 |     665 |               659
(20 rows)
```

Hamilton and Alonso both show 219 in `running_excl_ties` — Räikkönen's 110
plus their own 109, with the peer's 109 excluded from the frame.  Button and
Vettel, tied at 6, both show 644 for the same reason.

Both `GROUPS` and `EXCLUDE` are standard SQL, now available in PostgreSQL.

---

## Aggregation and set operations

### FETCH FIRST n ROWS WITH TIES (PG 13)

`LIMIT n` truncates to exactly *n* rows; when the cutoff falls inside a tie,
which rows survive is arbitrary.  `FETCH FIRST n ROWS WITH TIES` extends the
result to include every row that ties with the last kept row:

```sql
-- All drivers who tied for second place in the 2007 season:
select surname, sum(points) as pts
from results join races using(raceid) join drivers using(driverid)
where extract(year from races.date) = 2007
group by driverid, surname
order by pts desc
fetch first 2 rows with ties;
```

```results
  surname  | pts 
-----------+-----
 Räikkönen | 110
 Alonso    | 109
 Hamilton  | 109
(3 rows)
```

Räikkönen won with 110 points; Alonso and Hamilton both scored 109, so the tie
at second place is included even though it pushes the result past two rows.
`WITH TIES` is SQL-standard syntax and applies only to `FETCH FIRST`; `LIMIT n`
has no ties extension.

### ANY_VALUE() (PG 16)

When a column is not part of the GROUP BY key, SQL requires wrapping it in an
aggregate.  `min()` or `max()` carry an implicit ordering that the query does
not need.  `any_value(col)` makes the intent explicit: return an arbitrary
representative from the group, no ordering guarantee:

```sql
select constructors.name                as constructor,
       any_value(drivers.surname)       as a_driver,   -- representative, not min/max
       count(distinct driverid)         as num_drivers,
       sum(results.points)              as total_points
from results
join races        using(raceid)
join drivers      using(driverid)
join constructors using(constructorid)
where extract(year from races.date) = 2017
group by constructorid, constructors.name
order by total_points desc
limit 5;
```

```results
 constructor | a_driver  | num_drivers | total_points 
-------------+-----------+-------------+--------------
 Mercedes    | Hamilton  |           2 |          503
 Ferrari     | Räikkönen |           2 |          385
 Red Bull    | Ricciardo |           2 |          270
 Force India | Pérez     |           2 |          133
 Williams    | Massa     |           3 |           65
(5 rows)
```

Standardized in SQL:2023, available from PostgreSQL 16.

---

## WITH queries and recursion

### CTE materialization hints (PG 12)

Before PostgreSQL 12, every CTE was an *optimization fence*: the planner
materialized it once and used the result as-is, even when inlining it as a
subquery would have allowed predicate pushdown.  PG 12 changed the default:
non-recursive, non-volatile CTEs are now inlined by the planner.

Two keywords give explicit control:

```sql
-- Force materialization (stable snapshot, prevents repeated evaluation)
with accidents as materialized (
    select season, count(*) as n from results
    join races using(raceid) join status using(statusid)
    where status = 'Accident'
    group by season
)
...

-- Let the planner inline and push predicates through
with top_drivers as not materialized (
    select driverid, sum(points) as pts
    from results group by driverid
)
select drivers.surname, pts
from top_drivers
join drivers using(driverid)
where pts > 500
order by pts desc
limit 6;
```

```results
  surname   |  pts   
------------+--------
 Hamilton   |   2528
 Vettel     |   2355
 Alonso     |   1842
 Rosberg    | 1594.5
 Schumacher |   1566
 Räikkönen  |   1498
(6 rows)
```

This change silently altered the performance of a large body of existing
queries.  `MATERIALIZED` is correct when a CTE has side effects or is
referenced more than once.  `NOT MATERIALIZED` is correct when the planner
needs to see filters that live outside the CTE.

### SEARCH and CYCLE in recursive CTEs (PG 14)

Recursive CTEs visit rows in whatever internal order PostgreSQL's work table
produces.  Two questions arise: *in what order did the planner visit each
row?* and *what happens when the graph has cycles?*

**SEARCH** controls traversal order.  `SEARCH DEPTH FIRST BY col SET
order_col` produces a pre-order depth-first traversal; `SEARCH BREADTH
FIRST BY col SET order_col` produces level-by-level breadth-first.  Before
PG 14, achieving the same required threading an explicit integer counter
through the recursive term.

**CYCLE** detects and stops on revisited rows.  `CYCLE col SET is_cycle
USING path` adds an `is_cycle` boolean and a `path` column tracking the
sequence of values seen so far.  When the recursive term would revisit a
value, `is_cycle` is set to true and the row is not fed back into the next
round.

```sql
with recursive borders(isocode, name, depth) as (
    select isocode, name, 0 from geoname.country where isocode = 250
    union all
    select c.isocode, c.name, b.depth + 1
    from geoname.neighbour n
    join geoname.country c on c.isocode = n.neighbour
    join borders b          on b.isocode = n.isocode
    where b.depth < 4
)
cycle isocode set is_cycle using path
select depth, isocode, name from borders
where not is_cycle
order by depth, name;
```

```results
 depth | isocode |     name      
-------+---------+---------------
     0 |     250 | France
     1 |      20 | Andorra
     1 |      56 | Belgium
     1 |     276 | Germany
     1 |     380 | Italy
     1 |     442 | Luxembourg
     1 |     492 | Monaco
     1 |     724 | Spain
     1 |     756 | Switzerland
...
(36 rows)
```

Wrapping the same CTE in a summary shows how many distinct countries are reachable at each hop:

```sql
select depth, count(distinct isocode) as countries
from borders
where not is_cycle
group by depth
order by depth;
```

```results
 depth | countries 
-------+-----------
     0 |         1
     1 |         8
     2 |        19
     3 |        26
     4 |        43
(5 rows)
```

{{< image src="fig-neighbour-reach.svg" title="Countries reachable from France in 1–4 border hops, colour-coded by depth. Map produced with PostGIS from the geoname dataset." >}}

Before PG 14, the equivalent required `WHERE NOT id = ANY(path)` with a
manually constructed array — fragile and easy to get wrong on graphs with
multiple edges between the same pair of nodes.

---

## Data types

### Built-in gen_random_uuid() (PG 13)

Since PostgreSQL 13, `gen_random_uuid()` is a built-in function that generates
a cryptographically random UUID v4 without installing the `uuid-ossp`
extension:

```sql
select gen_random_uuid();
-- fbb850cc-dd26-4904-96ef-15ad8dfaff07
```

### uuidv7() — time-sortable UUIDs (PG 18)

PostgreSQL 18 adds `uuidv7()`, which generates a time-sortable UUID v7.  The
first 48 bits encode the current millisecond timestamp, so rows inserted in
sequence also sort in lexicographic order on the UUID column — a significant
advantage for B-tree index locality and clustered writes:

```sql
select uuidv7() as v7, gen_random_uuid() as v4;
```

For new schemas that use UUIDs as primary keys and expect heavy insert loads
or range scans, `uuidv7()` is the better choice.

### Multi-Ranges and range_agg() (PG 14)

{{< image src="fig-range-operators.svg" title="Three range type operators on a number line: && (overlap), @> (contains), and -|- (adjacent)." >}}

Every range type has gained a multirange counterpart: `int4multirange`,
`datemultirange`, `tstzmultirange`, and so on — an ordered, non-overlapping
collection of sub-ranges stored in a single column value.

The motivating problem is membership with gaps.  John Frusciante played guitar
for the Red Hot Chili Peppers three separate times.  He joined in 1988, left in
1992, came back in 1998, left again in 2009, and rejoined in 2019.  A plain
`UNIQUE (band_id, composer_id)` constraint would forbid the second stint.  A
`tstzrange` column with a GiST exclusion constraint handles exactly this:

```sql
create table composer.bandmember (
  id          serial    primary key,
  band_id     int       not null references composer.band(id),
  composer_id int       not null references composer.composer(id),
  active      tstzrange not null,
  exclude using gist (band_id with =, composer_id with =, active with &&)
);
```

The `EXCLUDE USING GIST` constraint says: no two rows with the same
`(band_id, composer_id)` may have *overlapping* `active` ranges.  Three
non-overlapping rows for Frusciante coexist; a fourth row that overlaps any of
the three is rejected at write time with no trigger required.

```sql
select b.name                                   as band,
       c.name                                   as member,
       lower(bm.active)::date                   as joined,
       case when upper(bm.active) = 'infinity' then null
            else upper(bm.active)::date end     as departed,
       (upper(bm.active) = 'infinity')          as current
  from composer.band          b
       join composer.bandmember bm on bm.band_id = b.id
       join composer.composer   c  on c.id = bm.composer_id
 where b.name = 'Red Hot Chili Peppers'
 order by lower(bm.active);
```

```results
         band          │      member      │   joined   │  departed  │ current
═══════════════════════╪══════════════════╪════════════╪════════════╪═════════
 Red Hot Chili Peppers │ Anthony Kiedis   │ 1983-01-01 │            │ t
 Red Hot Chili Peppers │ Flea             │ 1983-01-01 │            │ t
 Red Hot Chili Peppers │ Chad Smith       │ 1988-01-01 │            │ t
 Red Hot Chili Peppers │ John Frusciante  │ 1988-01-01 │ 1992-05-07 │ f
 Red Hot Chili Peppers │ Dave Navarro     │ 1993-09-01 │ 1998-04-01 │ f
 Red Hot Chili Peppers │ John Frusciante  │ 1998-04-01 │ 2009-12-31 │ f
 Red Hot Chili Peppers │ John Frusciante  │ 2019-12-15 │            │ t
(7 rows)
```

The `@>` (containment) operator asks: *does this range contain this point?*  A
single operator answers "was this person a member at this moment?":

```sql
select b.name as band,
       c.name as member
  from composer.band          b
       join composer.bandmember bm on bm.band_id = b.id
       join composer.composer   c  on c.id = bm.composer_id
 where bm.active @> '1985-07-13 12:00:00+00'::timestamptz
 order by b.name, c.name;
```

```results
         band            │      member
═════════════════════════╪══════════════════
 King Diamond            │ King Diamond
 King Diamond            │ Mikkey Dee
 Metallica               │ Cliff Burton
 Metallica               │ James Hetfield
 Metallica               │ Kirk Hammett
 Metallica               │ Lars Ulrich
 Motörhead               │ Lemmy Kilmister
 Motörhead               │ Phil Campbell
 Motörhead               │ Pete Gill
 Motörhead               │ Würzel
 Queen                   │ Brian May
 Queen                   │ Freddie Mercury
 Queen                   │ John Deacon
 Queen                   │ Roger Taylor
 Red Hot Chili Peppers   │ Anthony Kiedis
 Red Hot Chili Peppers   │ Flea
 Scorpions               │ Klaus Meine
 Scorpions               │ Matthias Jabs
 Scorpions               │ Rudolf Schenker
(19 rows)
```

The date is 13 July 1985 — Live Aid.  John Frusciante isn't in RHCP yet; he
joined in January 1988.  Phil Taylor isn't in Motörhead either: his first stint
ended in May 1984 and his second didn't begin until June 1987.

{{< image src="fig-range-band-timeline.svg" title="Band membership timelines for Queen, RHCP, and Motörhead, with a point-in-time query at 1985-07-13 (Live Aid). Orange dots mark the 19 members the @> query returns. Phil Taylor's two Motörhead stints appear as separate bars with a gap." >}}

`range_agg()` is the aggregate counterpart: it collapses a set of individual
ranges into a single multirange, automatically merging overlapping or adjacent
sub-ranges and preserving genuine gaps.  A `tstzmultirange` model stores one
row per `(band, member)` pair regardless of how many rejoins occurred:

```sql
create table composer.bandmember_mr (
  id          serial          primary key,
  band_id     int             not null references composer.band(id),
  composer_id int             not null references composer.composer(id),
  active      tstzmultirange  not null,
  unique (band_id, composer_id)
);

insert into composer.bandmember_mr (band_id, composer_id, active)
select band_id, composer_id, range_agg(active)
  from composer.bandmember
 group by band_id, composer_id;
```

Thirty-two stints become thirty rows: Phil Taylor's two Motörhead stints and
Frusciante's three RHCP stints each collapse into one `tstzmultirange` row
carrying multiple sub-ranges.

{{< image src="fig-range-multirange.svg" title="RHCP lineup as tstzrange (7 rows, top) vs tstzmultirange (5 rows, bottom). Frusciante's three stints are three separate rows in the top half and one row with three sub-ranges in the bottom half. A query at 2004-01-01 finds him in both models." >}}

The `@>` operator works identically on `tstzmultirange` — it checks whether the
timestamp falls inside *any* sub-range.  `unnest(tstzmultirange)` expands
sub-ranges back to individual `tstzrange` rows when the full stint history is
needed again.

### Built-in C.UTF-8 collation (PG 17)

Until PostgreSQL 16, every collation came from the operating system's `libc`
or the ICU library — excellent, but environment-dependent.  A database created
with `en_US.UTF-8` may sort differently on macOS than on Linux with the same
locale name, because sort tables vary between OS versions.

PostgreSQL 17 ships a built-in `C.UTF-8` collation (also accessible as
`pg_c_utf8`) that is:

- **Locale-independent** — it sorts by Unicode code point, the same way on every platform and every OS version.
- **Immutable** — the sort order is part of the PostgreSQL release; it never changes on an OS upgrade.
- **Fast** — it avoids locale-aware comparison functions and is roughly as fast as the classic `C` locale.

The built-in locale is named `C.UTF-8` in `BUILTIN_LOCALE`, but the
corresponding SQL collation is `pg_c_utf8`.  Creating a database with it
requires `TEMPLATE template0` when the cluster default is `libc` (which it
is unless you changed it at `initdb` time):

```sql
-- cluster default is libc:
select datname, datlocprovider, datlocale
  from pg_database
 where datname = 'template1';
```

```results
  datname  | datlocprovider | datlocale
-----------+----------------+-----------
 template1 | c              |
```

```sql
-- create a database with the portable built-in collation
-- template0 is required when switching locale provider
create database myapp
    encoding        'UTF8'
    locale_provider builtin
    builtin_locale  'C.UTF-8'
    template        template0;
```

```results
CREATE DATABASE
```

```sql
-- reconnect and confirm
select datname, datlocprovider, datlocale
  from pg_database
 where datname = 'myapp';
```

```results
 datname | datlocprovider | datlocale
---------+----------------+-----------
 myapp   | b              | C.UTF-8
```

```sql
-- code-point order: uppercase before lowercase, ASCII before extended Unicode
-- pg_c_utf8 works in any database, regardless of its own locale provider
select word
  from (values ('ångström'), ('banana'), ('Ångström'), ('Azure'), ('azure')) as t(word)
 order by word collate pg_c_utf8;
```

```results
   word
----------
 Azure
 azure
 banana
 Ångström
 ångström
```

```sql
-- apply per-column on an existing table
alter table articles
    alter column title type text collate pg_c_utf8;
```

Use `pg_c_utf8` whenever you need reproducible `ORDER BY` across
environments: multi-region deployments, CI pipelines that compare sort order
against a fixture, or logical replicas on different OS versions.  Its
code-point sort order is less human-friendly than a language-aware ICU
collation (accented characters sort after all ASCII letters), so prefer a
named ICU collation for end-user-facing sorted lists and save `pg_c_utf8` for
internal keys and system tables where stability matters more than linguistic
naturalness.

### Incremental Sort (PG 13)

{{< image src="fig-incremental-sort.svg" title="Incremental Sort: pre-sorted groups enter the sort node one at a time; each group is sorted independently and flushed before the next arrives." >}}

When leading ORDER BY columns already arrive pre-sorted from an index or an
earlier pipeline node, PostgreSQL 13 avoids sorting the full result set from
scratch.  Instead it partitions the input into runs of equal leading-key
values and sorts each run independently — *incremental sort*.  The planner
picks it automatically when the estimated cost is lower; it shows up in
`EXPLAIN` as an `Incremental Sort` node.  For queries that order on many
columns where the first few come from an index, this can cut sort cost by an
order of magnitude.

---

## JSON

{{< image src="fig-jsonb-operators.svg" title="JSONB navigation operators: -> returns a JSON sub-object, ->> returns a text value. Chaining navigates nested structures." >}}

### SQL/JSON path language (PG 12)

PostgreSQL 12 introduced a path language for `jsonb`.  A path expression
navigates through a JSON document like an XPath expression does through XML:

```sql
select info @? '$.address.city'  -- does this path exist?
from customers
where info @@ '$.orders[*].total > 100';  -- path predicate
```

### JSONB subscript syntax (PG 14)

PostgreSQL 14 introduced bracket subscript syntax for `jsonb`, consistent
with array indexing:

```sql
-- Before PG 14
select info->'address'->>'city' from customers;

-- From PG 14
select info['address']['city'] from customers;
```

The subscript form also works on the left-hand side of `UPDATE … SET`:

```sql
update customers set info['address']['city'] = '"Lyon"' where id = 42;
```

### IS JSON predicate (PG 16)

```sql
select id from inputs where payload is json;     -- valid JSON?
select id from inputs where payload is json object; -- specifically a JSON object?
```

### SQL/JSON constructors and JSON_TABLE (PG 16–17)

PostgreSQL 16 added the SQL:2023 standard JSON constructors:

```sql
select json_object('constructor': constructors.name,
                   'points':      sum(results.points))
from results
join races        using(raceid)
join constructors using(constructorid)
where extract(year from races.date) = 2016
group by constructorid, constructors.name
order by sum(results.points) desc
limit 3;
```

```results
                 json_object                  
----------------------------------------------
 {"constructor" : "Mercedes", "points" : 765}
 {"constructor" : "Red Bull", "points" : 468}
 {"constructor" : "Ferrari", "points" : 398}
(3 rows)
```

PostgreSQL 17 completed the picture with `JSON_TABLE()`, which turns a JSON
array into a relational result set in a single expression — no lateral join
or `json_array_elements` unwrapping needed:

```sql
select t.name, t.score
from events,
     json_table(
         events.payload,
         '$.results[*]'
         columns (
             name  text path '$.driver',
             score int  path '$.points'
         )
     ) as t;
```

---

## Schema modeling

### Generated stored columns (PG 12)

A *generated stored column* is computed automatically from other columns in the
same row and materialized on disk at insert and update time:

```sql
create table product (
    id        bigint generated always as identity primary key,
    price_ht  numeric(12,2) not null,
    vat_rate  numeric(5,4)  not null default 0.20,
    price_ttc numeric(12,2)
              generated always as (price_ht * (1 + vat_rate)) stored
);
```

The `STORED` keyword writes the value at write time; it can be indexed
directly.  The expression is read-only — INSERT and UPDATE may not supply a
value for it unless `OVERRIDING SYSTEM VALUE` is used.

### Virtual generated columns (PG 18)

PostgreSQL 18 adds *virtual* generated columns: the expression is evaluated
at query time and occupies no on-disk storage.

```sql
create table product (
    id        bigint generated always as identity primary key,
    price_ht  numeric(12,2) not null,
    vat_rate  numeric(5,4)  not null default 0.20,
    price_ttc numeric(12,2)
              generated always as (price_ht * (1 + vat_rate)) virtual
);
```

The trade-off is clear:

| | STORED | VIRTUAL |
|---|---|---|
| Storage | one column width per row | none |
| Write cost | evaluated on every INSERT/UPDATE | no write overhead |
| Read cost | plain column read | expression evaluated per row |
| Directly indexable | yes | no — use a functional index |

Choose STORED when the expression is expensive to recompute or when the column
must be indexed.  Choose VIRTUAL when the expression is cheap, the table is
large, and write throughput matters.

### NULLS NOT DISTINCT on unique constraints (PG 15)

By default, a UNIQUE constraint follows SQL's three-valued logic: `NULL ≠
NULL`, so any number of NULL values are permitted in a unique column.  That is
correct when the column models "absent" — absence is not a duplicate.  But
some columns model an *optional unique identifier* (an IBAN, a registration
number): here, "absent" should mean "not yet assigned", and two unassigned
rows should still collide.

PostgreSQL 15 added `NULLS NOT DISTINCT` to make the constraint treat all
NULL values as equal for the purpose of uniqueness — at most one NULL
is allowed:

```sql
-- Standard: multiple NULLs allowed
create unique index on customer (external_ref);

-- NULLS NOT DISTINCT: at most one NULL permitted
create unique index on customer (external_ref) nulls not distinct;

-- Inline table constraint syntax
create table customer (
    id           bigint generated always as identity primary key,
    external_ref text,
    unique nulls not distinct (external_ref)
);
```

### Partition improvements (PG 12–19)

{{< image src="fig-partition-timeline.svg" title="Declarative partitioning milestones, PG 10–19." >}}

Declarative partitioning has received a steady stream of improvements since
PG 11.  The highlights, in version order:

- **PG 12** — planning-time and runtime partition pruning, making partitioned
  tables viable for OLTP workloads.
- **PG 13** — logical replication at the partitioned-table level.
- **PG 14** — `ATTACH PARTITION` is near-instant when a matching `CHECK`
  constraint already exists, eliminating the full-table scan.
- **PG 15** — `MERGE` statement support on partitioned tables; foreign key
  references *from* a partitioned table.
- **PG 19** — `SPLIT PARTITION` and `MERGE PARTITIONS` restructure a
  partitioned table in a single statement, without detaching and recreating
  partitions by hand. Not an online operation, though: both take `ACCESS
  EXCLUSIVE` on the parent and on every partition involved.

---

## DML

### MERGE (PG 15), extended in PG 17

PostgreSQL 15 added the standard SQL `MERGE` command.  It does in a single
statement what previously required two or three: insert a row if it does not
exist in the target, update it if it does, and optionally delete it if a
condition holds:

```sql
merge into driver_points dp
using (
    select d.driverid, d.surname,
           coalesce(sum(res.points), 0) as total
    from drivers d
    left join results res using(driverid)
    group by d.driverid, d.surname
) src
on dp.driverid = src.driverid
when matched then
    update set total = src.total, surname = src.surname
when not matched then
    insert (driverid, surname, total)
    values (src.driverid, src.surname, src.total);
```

PostgreSQL 17 added a third branch, `WHEN NOT MATCHED BY SOURCE`, covering
target rows with no counterpart in the source — the missing piece for a
complete full-table synchronization:

```sql
when not matched by source then delete;
```

PostgreSQL 17 also extended `RETURNING` to `MERGE`, with `merge_action()`
reporting which branch fired for each affected row:

```sql
merge into driver_points dp
using (values (1, 'Hamilton', 103.0)) as src(driverid, surname, total)
on dp.driverid = src.driverid
when matched     then update set total = src.total
when not matched then insert (driverid, surname, total)
                      values (src.driverid, src.surname, src.total)
returning merge_action(), dp.*;
-- merge_action() returns 'INSERT', 'UPDATE', or 'DELETE' per row
```

### RETURNING OLD and NEW (PG 18)

PostgreSQL 18 extends `RETURNING` to expose *both* the before and after image
of a changed row in a single statement.  Use the pseudo-references `OLD` and
`NEW` to qualify column names:

```sql
update account
   set balance = balance - 100
 where account_id = 42
returning old.balance as balance_before,
          new.balance as balance_after;
```

This enables inline audit-trail patterns without triggers.  The classical
approach requires a `BEFORE UPDATE` trigger that writes to a history table;
with `RETURNING OLD/NEW` the same result is achievable in one CTE:

```sql
with changes as (
    update account
       set balance = balance - t.amount
      from transfers t
     where t.transfer_id = $1
       and account.account_id = t.debit_account
    returning
        old.account_id,
        old.balance    as balance_before,
        new.balance    as balance_after,
        now()          as changed_at
)
insert into account_audit (account_id, balance_before, balance_after, changed_at)
select account_id, balance_before, balance_after, changed_at
from changes;
```

`OLD.*` returns NULL for INSERT (no previous row); `NEW.*` is NULL for
DELETE (no new row).

---

## Data loading

### COPY ON_ERROR IGNORE and REJECT_LIMIT (PG 17 / PG 18)

Before PostgreSQL 17, a single malformed row in a CSV file aborted the entire
`COPY` command.  PG 17 adds `ON_ERROR IGNORE`, which skips rows that fail
type conversion and continues loading:

```sql
copy events from '/data/events.csv'
    with (format csv, header, on_error ignore, log_verbosity verbose);
```

`LOG_VERBOSITY VERBOSE` logs each skipped row with the reason.  PostgreSQL 18
adds `REJECT_LIMIT n`, which aborts the load if more than *n* rows are
skipped — a safety valve against silently losing a large fraction of the
data.

---

## Extensions

### pg_trgm: rebuild indexes after upgrading to PG 18

{{< image src="fig-trigram-similarity.svg" title="Trigram decomposition: each string is split into overlapping three-character windows (padded with spaces). Shared trigrams between two strings determine their similarity score." >}}

PostgreSQL 18 changed how `pg_trgm` generates trigrams: lower-casing is now
applied consistently before splitting a string into its three-character windows.
Earlier versions had an inconsistency in which some code paths skipped the
lower-casing step.

A GiST or GIN trigram index built on PG 17 or earlier encodes trigrams using
the old (partially case-sensitive) scheme.  After upgrading to PG 18, queries
that use `%`, `<->`, or `~*` will compare against index entries built with
the old scheme — some matches will be silently missed, with no error or
warning.

Rebuild all trigram indexes immediately after upgrading:

```sql
-- Find all trigram indexes in the database
select schemaname, tablename, indexname
from pg_indexes
where indexdef ilike '%gist_trgm_ops%'
   or indexdef ilike '%gin_trgm_ops%';

-- Rebuild without locking writes
reindex index concurrently track_title_idx;

-- Or rebuild all indexes on a table at once
reindex table concurrently lastfm.track;
```

The query above returns the indexes that need rebuilding:

```results
 schemaname | tablename |       indexname       
------------+-----------+-----------------------
 imdb       | name      | name_primaryname_idx1
 imdb       | uname     | uname_name_idx
 lastfm     | track     | track_artist_idx
 lastfm     | track     | track_title_idx
(4 rows)
```

This is a one-time migration step.

---

## Beyond SQL: a few operational improvements worth knowing

These improvements fall outside SQL syntax but affect any PostgreSQL shop:

**REINDEX CONCURRENTLY (PG 12)** — rebuild an index without taking an
exclusive lock on the table.  `CREATE INDEX CONCURRENTLY` has existed for
years; now you can also rebuild existing indexes non-blockingly.

**pg_stat_io (PG 16)** — a new system view that tracks read, write, and
extend operations broken down by backend type (client backend, autovacuum,
checkpointer) and I/O context (heap, index, WAL).  The missing piece for
diagnosing I/O-bound workloads without reaching for OS-level tools.

**ICU locale provider at cluster level (PG 15)** — before PG 15, ICU
collations could only be attached to individual columns via an explicit
`COLLATE` clause; `libc` was the only option at `initdb` or `CREATE DATABASE`
time.  PG 15 lifted that restriction: you can now choose ICU as the
locale provider for the whole cluster or database, getting stable,
OS-independent sort behaviour for non-C locales.  The default remains
`libc` unless you opt in.

**Logical replication: row and column filters (PG 15)** — publications can
now include a `WHERE` clause to replicate only matching rows and a column
list to replicate only selected columns.  Selective logical replication
without a custom decode plugin.

**Incremental backups (PG 17)** — `pg_basebackup` can now take incremental
backups using WAL summarization; `pg_combinebackup` reconstructs the full
image from a base backup and one or more incrementals.  This changes the
operational backup story significantly for large databases where nightly full
backups are impractical.

---

That is seven major versions of steady, focused work.  None of these features
are exotic: they each solve a real problem that SQL writers hit regularly.
PostgreSQL 18 is not yet the end of the list — the project is already well
into planning PG 19.

The release pace has coincided with a remarkable shift in the market.  The
[2024 Stack Overflow Developer Survey](https://survey.stackoverflow.co/2024/technology#databases)
— the largest annual poll of working developers — found PostgreSQL used by
**48.7% of all respondents** and **51.9% of professional developers**, making
it the most-used database for the second year running, ahead of MySQL and SQL
Server.  [DB-Engines](https://db-engines.com/en/ranking), which aggregates
signals from job boards, forums, and search trends, ranks it fourth overall
but has [awarded it "DBMS of the Year"](https://db-engines.com/en/blog_post/106)
four times (2017, 2018, 2020, 2023) — more than any other system.  Adoption
and development velocity are feeding each other.

If any of this made you want to go deeper, the new
edition of [*The Art of PostgreSQL*](https://theartofpostgresql.com) covers
all of it, with the full context of schema design, query optimization, and
application integration behind each feature.  Every query in the book is also
available in the Lab, organized into parts, chapters, and sections that follow
the book's structure — real open data, runnable in the browser, with EXPLAIN
diagrams and PostGIS map output ready to explore.

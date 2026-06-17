+++
title = "One Query, Two Aggregations: GROUPING SETS and FILTER"
date = "2026-08-01T00:00:00+00:00"
tags = ["PostgreSQL", "SQL", "GROUPING SETS", "Aggregation", "Lab"]
categories = ["PostgreSQL", "YeSQL"]
+++

Formula 1 tracks two world championships in parallel: one for drivers, one
for constructors. Finding both champions for every season requires
aggregating points over two different groupings of the same data. The
obvious approach — two separate queries joined together — works but scans
the results table twice. `GROUPING SETS` does it in one pass.

<!--more-->
<!--toc-->

## The problem

The `f1db` schema in The Lab contains the full Formula 1 history. The
`results` table has one row per driver per race, with a `points` column.
To find each season's champion driver and champion constructor we need:

1. Points per driver per season (grouped by `year, driverid`).
2. Points per constructor per season (grouped by `year, constructorid`).

These are genuinely different groups. A `GROUP BY year, driverid,
constructorid` would give per-driver-per-constructor subtotals — useful
for knowing that Alain Prost scored 72 points for McLaren in 1989, but
not for knowing that McLaren scored 141 points in total.

## GROUPING SETS

`GROUP BY GROUPING SETS((a, b), (a, c))` is shorthand for:

```sql
GROUP BY a, b  UNION ALL  GROUP BY a, c
```

…but without the second scan. PostgreSQL reads the input once and routes
each row into the right bucket. Columns not in the current grouping set
carry a `NULL`:

```sql
select year as season, driverid, constructorid,
       sum(points) as points
  from results join races using(raceid)
 group by grouping sets((year, driverid),
                        (year, constructorid))
 having sum(points) > 0
 order by year, points desc;
```

When `driverid` is `NULL`, the row is a constructor subtotal. When
`constructorid` is `NULL`, it is a driver subtotal. (Both `NULL` would
mean a grand total — we did not ask for that here, but `ROLLUP` and `CUBE`
add such totals automatically.)

## FILTER clause

`FILTER (WHERE condition)` scopes an aggregate to rows that match the
condition. It is a cleaner alternative to `CASE` inside `SUM()`:

```sql
-- same as:  sum(case when driverid is null then points end)
max(points) filter(where driverid is null) as ctops
```

We use two `FILTER` clauses to extract, in a single aggregate step, the
maximum constructor points and the maximum driver points for each season:

```sql
with points as (
   select year as season, driverid, constructorid,
          sum(points) as points
     from results join races using(raceid)
 group by grouping sets((year, driverid),
                        (year, constructorid))
   having sum(points) > 0
),
tops as (
   select season,
          max(points) filter(where driverid is null)    as ctops,
          max(points) filter(where constructorid is null) as dtops
     from points
 group by season
)
select tops.season,
       format('%s %s', drivers.forename, drivers.surname) as "Driver",
       champ_driver.points                                 as "Pts",
       constructors.name                                   as "Constructor",
       champ_constructor.points                            as "Pts"
  from tops
       join points as champ_driver
         on champ_driver.season = tops.season
        and champ_driver.constructorid is null
        and champ_driver.points = tops.dtops
       join points as champ_constructor
         on champ_constructor.season = tops.season
        and champ_constructor.driverid is null
        and champ_constructor.points = tops.ctops
       join drivers      using(driverid)
       join constructors using(constructorid)
order by season;
```

```
 season │       Driver        │ Pts │ Constructor │ Pts
════════╪═════════════════════╪═════╪═════════════╪═════
   1950 │ Nino Farina         │  30 │ Alfa Romeo  │  ...
   1985 │ Alain Prost         │  73 │ McLaren     │  90
   1989 │ Alain Prost         │  76 │ McLaren     │ 141
   1994 │ Michael Schumacher  │  92 │ Williams    │ 118
   2016 │ Nico Rosberg        │ 385 │ Mercedes    │ 765
```

## Daisy-chained CTEs

The query uses three CTEs, each building on the previous:

1. **`points`** — `GROUPING SETS` to compute subtotals for both drivers and
   constructors in one scan.
2. **`tops`** — `FILTER` to extract the maximum for each group within the
   already-aggregated data. (SQL cannot nest aggregates directly:
   `max(sum(...))` is an error.)
3. **`champs`** (the outer query) — joins `tops` back to `points` *twice*,
   once for drivers and once for constructors, to find who achieved those
   maximum values.

This is a common pattern: when you need to aggregate an aggregate, the
only way is a pipeline of CTEs (or derived tables).

## ROLLUP and CUBE

`ROLLUP(a, b)` is shorthand for `GROUPING SETS((a,b),(a),())` — it adds
subtotals and a grand total automatically, which is useful for reports.
`CUBE(a, b)` is `GROUPING SETS((a,b),(a),(b),())` — every combination.
For most analytical queries `GROUPING SETS` with explicit sets is clearest.

{{< lab >}}
```sh
\i starter-kit/02-grouping-sets.md
```
{{< /lab >}}

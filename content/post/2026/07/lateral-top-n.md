+++
title = "Top-N Per Group with LATERAL"
date = "2026-07-15T00:00:00+00:00"
tags = ["PostgreSQL", "SQL", "LATERAL", "Lab"]
categories = ["PostgreSQL", "YeSQL"]
+++

Each driver's three best finishes in 2017. The LIMIT must run once per
driver — which is exactly what LATERAL does and a plain join cannot.

<!--more-->
<!--toc-->

## The problem a plain join cannot solve

A JOIN between `drivers` and `results` returns every finish for every
driver. There is no way to say "stop after three per driver" at the join
level — that limit only makes sense per group, not over the whole result
set. You could reach for a window function and filter afterward, but the
cleanest expression is a subquery that runs once per driver, with the
driver's id in scope. LATERAL makes that possible.

## LATERAL is SQL's manual loop

```sql
select d.surname          as driver,
       top3.race,
       top3.pos,
       top3.points
  from f1db.drivers d
  join lateral (
      select r.name             as race,
             res.positionorder  as pos,
             res.points
        from f1db.results res
        join f1db.races   r on r.raceid = res.raceid
       where res.driverid = d.driverid
         and r.year       = 2017
         and res.points   > 0
       order by res.points desc
       limit 3
  ) top3 on true
 where d.surname in ('Hamilton', 'Vettel', 'Bottas', 'Ricciardo', 'Verstappen')
 order by d.surname, top3.points desc;
```

The inner subquery references `d.driverid` from the outer row. Without
`LATERAL` that reference is a syntax error — the outer table is not in
scope inside a subquery in `FROM`. With it, the subquery re-evaluates for
each driver row, using that driver's id. `ORDER BY points DESC LIMIT 3`
runs once per driver and returns at most three rows.

```
  driver   │         race          │ pos │ points
═══════════╪═══════════════════════╪═════╪════════
 Bottas    │ Russian Grand Prix    │   1 │   25
 Bottas    │ Austrian Grand Prix   │   1 │   25
 Bottas    │ Abu Dhabi Grand Prix  │   2 │   18
 Hamilton  │ British Grand Prix    │   1 │   25
 Hamilton  │ Italian Grand Prix    │   1 │   25
 Hamilton  │ Singapore Grand Prix  │   1 │   25
 Ricciardo │ Azerbaijan Grand Prix │   1 │   25
 Ricciardo │ Malaysian Grand Prix  │   1 │   25
 Ricciardo │ Monaco Grand Prix     │   1 │   25
 Verstappen│ Malaysian Grand Prix  │   3 │   15
 ...
```

Three rows per driver. The planner implements this as a Nested Loop: for
each driver row on the outer side, it runs the inner subquery — sorting
and limiting inside the loop.

## join lateral ... on true

`JOIN LATERAL (...) AS alias ON TRUE` is the standard spelling. `ON TRUE`
means every outer row joins to whatever the subquery returns. Use `LEFT
JOIN LATERAL` if you want outer rows with no matching subquery result to
appear with NULLs rather than be dropped.

## When to reach for LATERAL

LATERAL is the right tool when the subquery needs a value from the outer
row and needs its own `ORDER BY` or `LIMIT`. The canonical cases:

- **Top-N per group** — as above; N is fixed and the ordering column is
  indexed. The planner can use an Index Scan inside the loop and stop
  after N rows.
- **Nearest-neighbour per row** — find the three closest locations to
  each store; the distance expression references the store's coordinates.
- **Time-series joins** — for each event, find the measurement reading
  immediately before it.
- **Set-returning functions** — `LATERAL` is implicit when you call a
  function in `FROM`; you only write it explicitly with subqueries.

{{< lab >}}
The top-three-per-decade query in the Lab is a close relative — rank
combined with LATERAL to pull the top winners decade by decade:

```sql
\i 04-sql-select/15-sql-102/03_01_f1db.decade.top3.sql
```
{{< /lab >}}

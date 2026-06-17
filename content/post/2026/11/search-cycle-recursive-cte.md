+++
title = "WITH RECURSIVE, Upgraded: SEARCH and CYCLE in PostgreSQL 14"
date = "2026-11-15T00:00:00+00:00"
tags = ["PostgreSQL", "SQL", "CTE", "Recursive", "Graph", "Lab"]
categories = ["PostgreSQL", "YeSQL"]
+++

`WITH RECURSIVE` is one of SQL's most powerful features and one of its
least understood. PostgreSQL 14 added two standard sub-clauses —
`SEARCH` and `CYCLE` — that handle the two most common headaches with
recursive queries: controlling traversal order and detecting infinite
loops. Both replace boilerplate that recursive queries have carried for
decades.

<!--more-->
<!--toc-->

## How WITH RECURSIVE works

A recursive CTE has two parts joined by `UNION ALL`:

1. The **base case** — seeds the result with the starting rows.
2. The **recursive term** — references the CTE by name and produces
   more rows from the ones found so far.

PostgreSQL runs the recursive term repeatedly, each time seeing only the
rows the previous round added, and stops when a round adds nothing.

The canonical example is walking a tree stored as a parent reference.
In The Lab's `hydrorivers` schema, `next_down` is the id of the reach
this one flows into. To collect the entire Loire basin:

```sql
with recursive loire as (
    select hyriv_id, geom, ord_stra, 1 as depth
      from hydrorivers.rivers
     where hyriv_id = 20446779           -- the mouth at Saint-Nazaire

    union all

    select r.hyriv_id, r.geom, r.ord_stra, loire.depth + 1
      from hydrorivers.rivers r
           join loire on r.next_down = loire.hyriv_id
)
select count(*) from loire;
-- 6297
```

6 297 river reaches gathered from a single parent-reference column.

## SEARCH: controlling traversal order

By default, the traversal order of a recursive CTE is implementation-
defined. If you need breadth-first (level by level) or depth-first
(follow one branch to its end before the next), PostgreSQL 14 lets you
declare it:

```sql
with recursive tree as (
    select id, parent_id, name, 0 as depth
      from categories where parent_id is null

    union all

    select c.id, c.parent_id, c.name, tree.depth + 1
      from categories c
           join tree on c.parent_id = tree.id
)
search breadth first by id set traversal_order
select id, name, depth, traversal_order
  from tree
 order by traversal_order;
```

`SEARCH BREADTH FIRST BY id SET traversal_order` adds a synthetic
column `traversal_order` that, when sorted, gives you breadth-first
order (level 0, then all of level 1, then all of level 2, …).

Switch to `SEARCH DEPTH FIRST BY id SET traversal_order` and the same
column gives you depth-first order (follow the first branch all the way
down, then backtrack). The data does not change; only the generated
ordering column changes.

Before PostgreSQL 14, you had to carry a depth counter and sort on it
for breadth-first, or maintain an array of visited nodes and sort on
that for depth-first. Both approaches were fragile and verbose.

## CYCLE: detecting infinite loops

A recursive CTE over a graph (not a tree) can loop forever if the graph
has cycles. The standard workaround was to maintain an array of visited
node ids and check `NOT (id = ANY(visited))` on each step. This works
but is awkward and does not scale to large graphs.

PostgreSQL 14's `CYCLE` clause does it for you:

```sql
with recursive reachable as (
    select from_id, to_id, from_id as source
      from edges where from_id = 1

    union all

    select e.from_id, e.to_id, r.source
      from edges e
           join reachable r on e.from_id = r.to_id
)
cycle to_id set is_cycle using visited_path
select * from reachable where not is_cycle;
```

`CYCLE to_id SET is_cycle USING visited_path` adds:

- **`is_cycle`** — a boolean that is `true` when the current row would
  create a cycle (i.e., `to_id` is already in `visited_path`).
- **`visited_path`** — an array of visited `to_id` values, useful for
  debugging or displaying the path taken.

Filter on `where not is_cycle` to exclude the cycle-detecting rows;
or let them through and use `is_cycle` to identify where loops occur.

## Combining SEARCH and CYCLE

Both clauses can appear on the same CTE:

```sql
with recursive deps as (
    select dep_id, depended_on_id
      from dependencies
     where dep_id = :start

    union all

    select d.dep_id, d.depended_on_id
      from dependencies d
           join deps on d.dep_id = deps.depended_on_id
)
search depth first by dep_id set dfs_order
cycle dep_id set is_cycle using path
select dep_id, depended_on_id, dfs_order, is_cycle, path
  from deps
 order by dfs_order;
```

A dependency resolver, with depth-first ordering and cycle detection,
in a single CTE.

## Requires PostgreSQL 14 or later

`SEARCH` and `CYCLE` were added in PostgreSQL 14 (released October 2021).
If you are on an older version, the array-based workarounds still work;
they are just more verbose.

{{< lab >}}
The recursive river-basin example runs in The Lab:

```sh
\i starter-kit/06-recursive-rivers.md
```
{{< /lab >}}

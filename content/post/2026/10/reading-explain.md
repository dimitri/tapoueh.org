+++
title = "Reading PostgreSQL's EXPLAIN Output"
date = "2026-10-15T00:00:00+00:00"
tags = ["PostgreSQL", "Performance", "EXPLAIN", "Query Optimization"]
categories = ["PostgreSQL", "YeSQL"]
+++

A query is slow. `EXPLAIN ANALYZE` shows why — estimated rows, actual
rows, time per node, loops, buffer hits and misses. The format is dense
the first time you read it. Once you know what each line means, it becomes
the most useful diagnostic tool PostgreSQL has.

<!--more-->
<!--toc-->

## The three EXPLAIN forms

```sql
EXPLAIN query;                           -- estimated plan only, no execution
EXPLAIN ANALYZE query;                   -- execute and show actual times
EXPLAIN (ANALYZE, BUFFERS) query;        -- add I/O statistics
```

Always start with `EXPLAIN (ANALYZE, BUFFERS)` for performance work. The
estimated plan alone is useful for checking the planner's choices, but
`ANALYZE` adds the actual row counts and timings that reveal where the
estimates were wrong.

Be aware: `EXPLAIN ANALYZE` *executes the query*. For `UPDATE` or `DELETE`,
wrap it in a transaction and roll back:

```sql
begin;
explain analyze delete from sessions where expires_at < now();
rollback;
```

## Reading a node

Each line in EXPLAIN output is a *node* — a step in the execution tree.
The tree is indented with `->`, and outer nodes read their input from inner
nodes. The rightmost, most-indented node executes first.

```
Hash Join  (cost=184.00..512.75 rows=3420 width=72)
           (actual time=1.823..8.441 rows=3380 loops=1)
  Hash Cond: (orders.customer_id = customers.id)
  ->  Seq Scan on orders  (cost=0.00..210.50 rows=10050 width=40)
                          (actual time=0.012..1.432 rows=10050 loops=1)
  ->  Hash  (cost=95.50..95.50 rows=3560 width=32)
            (actual time=0.887..0.888 rows=3560 loops=1)
        Buckets: 4096  Batches: 1  Memory Usage: 224kB
        ->  Seq Scan on customers  (cost=0.00..95.50 rows=3560 width=32)
                                   (actual time=0.009..0.456 rows=3560 loops=1)
```

Each node shows:
- **Node type** — what operation (Seq Scan, Index Scan, Hash Join, …)
- **`cost=startup..total`** — the planner's estimate of cost units. Startup
  is the cost before the first row; total is the cost to produce all rows.
  These are not milliseconds; they are planner units calibrated to
  sequential disk page reads.
- **`rows=N`** — estimated row count.
- **`actual time=startup..total`** — measured milliseconds (only with
  `ANALYZE`).
- **`rows=N loops=M`** — actual rows produced, and how many times this node
  was executed. A nested-loop inner node may loop thousands of times.

## Spotting mis-estimates

The most useful thing `EXPLAIN ANALYZE` tells you is where the planner's
row estimates were wrong:

```
->  Seq Scan on events  (cost=0.00..8432.10 rows=3 width=88)
                        (actual time=0.023..112.450 rows=84203 loops=1)
```

Estimated 3 rows; got 84 203. That is a catastrophic mis-estimate.
The planner chose a plan appropriate for a tiny result set; the query
actually processed tens of thousands of rows. This is where you look for
a missing index, stale statistics (`ANALYZE events`), or a correlated
predicate that the planner's statistics model can't represent (in which
case extended statistics help: `CREATE STATISTICS ... ON col1, col2 FROM
table`).

## Scan nodes

| Node | When used |
|---|---|
| `Seq Scan` | Full table scan; appropriate for large fractions of the table |
| `Index Scan` | Uses an index; fetches rows from the heap (random I/O) |
| `Index Only Scan` | All needed columns are in the index; no heap access |
| `Bitmap Heap Scan` | Builds a bitmap of matching pages, then reads them in order — batched random I/O |

An `Index Scan` on a large fraction of a table can be *slower* than a
`Seq Scan` because random I/O is more expensive than sequential I/O.
The planner knows this and switches to `Seq Scan` when the fraction is
high enough (controlled by `random_page_cost` vs `seq_page_cost`).

## Join nodes

| Node | When used |
|---|---|
| `Nested Loop` | Fast for small inner sets or when the inner side is index-driven |
| `Hash Join` | Good when one side fits in memory (`work_mem`) and the join condition is equality |
| `Merge Join` | Both sides pre-sorted on the join key; often beats Hash Join for sorted input |

## BUFFERS: the I/O view

`EXPLAIN (ANALYZE, BUFFERS)` adds:

```
Buffers: shared hit=842 read=1203 written=0
```

- **hit** — pages already in PostgreSQL's buffer cache (no I/O)
- **read** — pages read from disk (or OS cache)
- **written** — dirty pages written during the query

A large `read` count means the working set does not fit in `shared_buffers`.
A large `written` count during a read query often indicates autovacuum
or checkpoint contention.

## "Rows Removed by Filter"

```
->  Seq Scan on orders  (cost=0.00..210.50 rows=42 width=40)
                        (actual time=0.012..2.891 rows=42 loops=1)
      Filter: (status = 'pending')
      Rows Removed by Filter: 9958
```

This node read 10 000 rows but returned 42. If this is a hot path, it is
almost always a candidate for an index on `status`. The filter selectivity
(42 / 10 000 = 0.4%) is high enough that an index would help dramatically.

## The `loops` multiplier

In a Nested Loop, the inner node's `actual time` is *per loop*, not total.
The total time is `actual time × loops`. Easy to miss; it is the most
common reason people underestimate how expensive a Nested Loop is:

```
->  Index Scan using ... on line_items
        (actual time=0.045..0.078 rows=3 loops=8420)
```

Total time for this node: `0.078 ms × 8 420 = 657 ms`. Not 0.078 ms.

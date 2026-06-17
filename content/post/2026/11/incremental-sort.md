+++
title = "Incremental Sort: PostgreSQL 13's Under-the-Radar Feature"
date = "2026-11-01T00:00:00+00:00"
tags = ["PostgreSQL", "Performance", "Query Optimization", "Sorting"]
categories = ["PostgreSQL", "YeSQL"]
+++

Sorting is expensive. A full sort reads every row, builds the sorted
output, then lets the query proceed. PostgreSQL 13 added *incremental
sort*: when the data is already sorted on a prefix of the required key,
sort only the tail within each pre-sorted group. Combined with `LIMIT`,
this can turn an O(N log N) sort into something much closer to O(k log k)
where k is the limit.

<!--more-->
<!--toc-->

## The problem with ORDER BY on composite keys

Suppose you have an index on `(customer_id)` and a query that orders by
`(customer_id, order_date)`:

```sql
select customer_id, order_date, total
  from orders
 order by customer_id, order_date
 limit 20;
```

PostgreSQL 12 and earlier: the index on `customer_id` tells the planner
nothing about `order_date`. To satisfy the full `ORDER BY`, it does a
full table scan, sorts everything, and takes the first 20 rows.

With an index on `(customer_id, order_date)`, the sort is free — the index
already provides that order. But indices on composite keys are not always
available, and they have a cost of their own.

## Incremental sort

PostgreSQL 13 introduced a smarter plan: use the existing index on
`customer_id` to avoid sorting on `customer_id`, then sort only the
`order_date` values *within each `customer_id` group* as those groups
stream through.

```
Limit  (cost=... rows=20 ...)
  ->  Incremental Sort
        Sort Key: customer_id, order_date
        Presorted Key: customer_id
        ->  Index Scan using orders_customer_id_idx on orders
```

Instead of one big sort of the entire table, you get many small sorts —
one per customer — and with `LIMIT 20`, you often only sort the first
customer's rows before you have your 20 results.

## Reading it in EXPLAIN ANALYZE

```
Incremental Sort  (cost=1.57..2882.34 rows=50000 width=16)
                  (actual time=0.145..14.823 rows=20 loops=1)
  Sort Key: customer_id, order_date
  Presorted Key: customer_id
  Full-sort Groups: 1  Sort Method: quicksort  Memory: 26kB
  Pre-sorted Groups: 19  Sort Method: quicksort  Average Memory: 28kB
```

- **`Presorted Key`** — the prefix that was already sorted. The planner
  used this prefix for free from the index.
- **`Full-sort Groups`** — groups that were sorted with a standard sort
  (happens when the group is large enough to warrant it).
- **`Pre-sorted Groups`** — groups sorted incrementally as they arrived.

With `LIMIT 20` and 19 pre-sorted groups of a handful of rows each, total
sort work is tiny compared to a full sort of the whole table.

## When incremental sort helps

Incremental sort is most useful when:

1. You have an index on a prefix of the `ORDER BY` key.
2. The query has a `LIMIT` that is small relative to the table size.
3. The non-prefix sort key has low cardinality within each group (so each
   group sort is cheap).

It is less useful (or not used at all) when:

- There is already a full composite index covering the entire `ORDER BY`.
- The `LIMIT` is large or absent — you end up sorting nearly everything
  anyway.
- The data has very few distinct values for the prefix key (few groups,
  each enormous).

## Enabling it

Incremental sort is on by default in PostgreSQL 13+. To test the
difference or to disable it (debugging only):

```sql
set enable_incremental_sort = off;
```

## Interaction with window functions

Incremental sort also benefits window functions that order on a composite
key when the leading columns are already sorted:

```sql
select customer_id, order_date,
       rank() over (partition by customer_id order by order_date) as rn
  from orders;
```

If `customer_id` comes from an index scan, PostgreSQL 13+ can use
incremental sort for the `ORDER BY order_date` within each partition,
rather than re-sorting the entire result set.

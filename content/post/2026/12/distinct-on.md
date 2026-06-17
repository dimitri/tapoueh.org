+++
title = "PostgreSQL's DISTINCT ON: Elegant Top-1 Per Group"
date = "2026-12-01T00:00:00+00:00"
tags = ["PostgreSQL", "SQL", "DISTINCT ON", "Query Patterns"]
categories = ["PostgreSQL", "YeSQL"]
+++

The most recent order per customer. `DISTINCT ON (customer_id)` with
`ORDER BY customer_id, order_date DESC` keeps exactly one row per
customer: the one that sorts first. It is PostgreSQL-specific syntax, it
reads as intent, and with a composite index on `(customer_id, order_date
DESC)` it needs no sort at all.

<!--more-->
<!--toc-->

## The pattern

```sql
select distinct on (customer_id)
       customer_id, order_date, total
  from orders
 order by customer_id, order_date desc;
```

This returns one row per `customer_id`: the most recent order for each
customer. The mechanics are:

1. `DISTINCT ON (customer_id)` means: for each distinct value of
   `customer_id`, keep exactly one row.
2. The `ORDER BY` clause determines which row is kept: `customer_id` must
   appear first (so PostgreSQL knows which group each row belongs to), and
   `order_date desc` picks the most recent one within each group.

The rule is: `DISTINCT ON` columns must form a left prefix of `ORDER BY`.
PostgreSQL sorts by `customer_id, order_date desc`, takes the first row
of each `customer_id` group, and discards the rest.

## Compared to window functions

The equivalent using `ROW_NUMBER()`:

```sql
select customer_id, order_date, total
  from (
      select customer_id, order_date, total,
             row_number() over (
                 partition by customer_id
                 order by order_date desc
             ) as rn
        from orders
  ) ranked
 where rn = 1;
```

Both produce the same result. The window function approach is more
portable (it is standard SQL) and scales to top-N (change `rn = 1` to
`rn <= 3`). `DISTINCT ON` is more concise for top-1 and integrates
naturally with `LIMIT`:

```sql
-- The 5 most-recently-active customers:
select distinct on (customer_id)
       customer_id, order_date, total
  from orders
 order by customer_id, order_date desc
 limit 5;
```

Wait — is this right? `LIMIT 5` here limits the *result* to 5 rows, not
5 per customer. After `DISTINCT ON` collapses to one row per customer,
`LIMIT 5` gives the first 5 customers in `customer_id` order. If you want
the 5 customers who ordered most recently, you need a subquery:

```sql
select *
  from (
      select distinct on (customer_id)
             customer_id, order_date, total
        from orders
       order by customer_id, order_date desc
  ) latest
 order by order_date desc
 limit 5;
```

## Index support

`DISTINCT ON` can use an index on the `(customer_id, order_date desc)`
composite key to avoid a sort entirely:

```sql
create index on orders (customer_id, order_date desc);

explain select distinct on (customer_id)
               customer_id, order_date, total
          from orders
         order by customer_id, order_date desc;
```

```
Unique  (cost=0.42..289.58 rows=850 width=24)
  ->  Index Scan using orders_customer_id_order_date_idx on orders
        (cost=0.42..267.08 rows=9000 width=24)
```

The `Unique` node deduplicated the stream from the index scan — no sort,
no hash, just a sequential read of the index. This is typically the fastest
path for top-1-per-group when the table is large.

## When to use each approach

| Situation | Best tool |
|---|---|
| Top-1 per group, clean ordering | `DISTINCT ON` |
| Top-N per group (N > 1) | `ROW_NUMBER()` or `LATERAL` |
| Top-1 per group, need other aggregates alongside | Window functions |
| Top-1 per group, composite condition on "first" | `LATERAL` (most flexible) |
| Portability across databases | Window functions |

## A practical example: most recent price

A common use case is fetching the current price of each product from a
price history table:

```sql
select distinct on (product_id)
       product_id, price, effective_date
  from price_history
 order by product_id, effective_date desc;
```

Clean, readable, and with an index on `(product_id, effective_date desc)`,
fast even on a table with millions of historical price records.

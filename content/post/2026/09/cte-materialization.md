+++
title = "CTE Materialization: When PostgreSQL 12 Changed the Rules"
date = "2026-09-15T00:00:00+00:00"
tags = ["PostgreSQL", "SQL", "CTE", "Query Optimization", "Performance"]
categories = ["PostgreSQL", "YeSQL"]
+++

For years, a Common Table Expression in PostgreSQL was an *optimization
fence*: the planner materialised it into a temporary result set, then
queried that set, never pushing predicates from the outer query into the
CTE's scan. PostgreSQL 12 changed this. Understanding what changed — and
when each behaviour is correct — matters for anyone writing non-trivial
queries.

<!--more-->
<!--toc-->

## The old behaviour (before PostgreSQL 12)

A CTE was always materialised. PostgreSQL ran the CTE once, stored the
rows, and then ran the outer query against the stored set. No predicates
leaked inward, no statistics from the outer query influenced the CTE's
plan. This was a fence.

```sql
-- PostgreSQL 11 and earlier: 'active_users' is fully materialised
-- even though the outer WHERE filters to a single user.
with active_users as (
    select * from users where last_login > now() - interval '30 days'
)
select * from active_users where id = 42;
```

Under the old rules, the planner could not use an index on `users.id`
inside the CTE. It scanned all active users, stored them, then found
the one with `id = 42`. On a large table this was meaningfully slower
than a direct `WHERE last_login > ... AND id = 42` query.

## What PostgreSQL 12 changed

From PostgreSQL 12, a CTE with no side effects (no `INSERT`/`UPDATE`/
`DELETE`/`RETURNING`, not using `VOLATILE` functions) is *inlined by
default* — the planner treats it like a subquery or view and can push
predicates through it:

```sql
-- PostgreSQL 12+: planner may inline 'active_users', enabling
-- an index scan on users.id.
with active_users as (
    select * from users where last_login > now() - interval '30 days'
)
select * from active_users where id = 42;
```

The query now behaves as if you had written the join conditions directly.
The planner sees the full predicate set and can choose the best index.

## Forcing the old behaviour: MATERIALIZED

Sometimes the old fence was *useful*. If a CTE is expensive to compute
and referenced multiple times, you want PostgreSQL to run it once and
cache the result. You also want materialisation when the CTE has side
effects and you need them to complete before the outer query begins.

PostgreSQL 12 added explicit keywords:

```sql
with ranked_products as materialized (
    select product_id,
           rank() over (partition by category_id order by revenue desc) as rnk
      from sales_summary
)
select *
  from ranked_products r1
  join ranked_products r2 on r1.category_id = r2.category_id
                          and r1.rnk = 1
                          and r2.rnk = 2;
```

Without `MATERIALIZED`, an inlining planner might compute the window
function twice (once for each self-reference). With it, the CTE executes
once and both references read the cached rows.

## Forcing inlining: NOT MATERIALIZED

`NOT MATERIALIZED` is the complement — it tells the planner to treat the
CTE exactly like a subquery, even if it would otherwise choose to
materialise (perhaps because the CTE has a `VOLATILE` function call that
the old rule would fence):

```sql
with latest_rates as not materialized (
    select currency, rate
      from exchange_rates
     where ts = (select max(ts) from exchange_rates)
)
select amount * rate
  from transactions
  join latest_rates using(currency);
```

This is rarely needed; the planner's default heuristics are usually right.
`NOT MATERIALIZED` is a last resort when you have profiled and confirmed
that the planner is materialising something it should not.

## The deduplication use case

One place where `MATERIALIZED` is clearly correct is when a CTE generates
expensive unique values and you reference the result multiple times —
for example, generating a batch of UUIDs for an insert:

```sql
with new_ids as materialized (
    select gen_random_uuid() as id
      from generate_series(1, 1000)
)
insert into orders (id, created_at)
select id, now() from new_ids;
```

Without `MATERIALIZED`, a sufficiently aggressive inliner might evaluate
`gen_random_uuid()` twice. With it, the 1 000 UUIDs are generated once
and reused.

## Summary of the rules

| CTE type | PostgreSQL 11 | PostgreSQL 12+ default | Keyword to override |
|---|---|---|---|
| Read-only, referenced once | Materialised | Inlined | `MATERIALIZED` to force cache |
| Read-only, referenced multiple times | Materialised | Inlined (may run N times) | `MATERIALIZED` to run once |
| Contains DML or VOLATILE functions | Materialised | Materialised | `NOT MATERIALIZED` (use with care) |

The practical advice is simple: in PostgreSQL 12+ you do not usually need
to think about this. Write your CTEs for clarity. Add `MATERIALIZED` only
when you profile and find that an expensive CTE is being re-evaluated
unnecessarily. Add `NOT MATERIALIZED` almost never.

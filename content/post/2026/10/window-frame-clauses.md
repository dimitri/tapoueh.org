+++
title = "Window Frame Clauses: ROWS, RANGE, and GROUPS"
date = "2026-10-01T00:00:00+00:00"
tags = ["PostgreSQL", "SQL", "Window Functions", "Analytics"]
categories = ["PostgreSQL", "YeSQL"]
+++

A window function operates over a *frame* — a subset of the partition
defined by the `OVER` clause. Most people use `sum() over (order by ...)` and
accept the default frame without examining it. The default is reasonable for
running totals, but once you have duplicate values in the ordering column,
or once you need a rolling N-row or N-value window, the three frame modes —
`ROWS`, `RANGE`, and `GROUPS` — diverge in ways that matter.

<!--more-->
<!--toc-->

## The default frame

Any window function with `ORDER BY` but without an explicit `FRAME`
clause gets this default:

```sql
RANGE BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW
```

`RANGE` means the frame extends to include *all rows whose ORDER BY value
equals the current row's value* — the "peer group". So if three rows share
the same date, the running sum at each of those rows already includes all
three. This is almost always what you want for a running total; it avoids
the confusing situation where identical-date rows show different cumulative
values.

## ROWS: literal row counting

`ROWS` counts physical rows in the partition, ignoring ties:

```sql
select date, amount,
       sum(amount) over (
           order by date
           rows between unbounded preceding and current row
       ) as running_total
  from daily_sales;
```

With `ROWS`, the frame for each row is exactly: all rows from the start of
the partition up to and including *this physical row*. Two rows with the
same date get different running totals. This is correct when your data has
no duplicates in the ordering column, or when you intentionally want
distinct row-by-row accumulation (e.g., a ledger where each transaction is
distinct even if two happen in the same second).

`ROWS` also enables rolling windows:

```sql
select week, revenue,
       avg(revenue) over (
           order by week
           rows between 3 preceding and current row
       ) as rolling_4w_avg
  from weekly_revenue;
```

"The current row and the 3 before it" — a 4-week moving average, strictly
by row position.

## RANGE: value-based windows

`RANGE` extends the frame to *all rows with the same ORDER BY value as
the boundary*. For `CURRENT ROW`, that means the whole peer group. But
`RANGE` also supports numeric and temporal offsets:

```sql
select ts, value,
       avg(value) over (
           order by ts
           range between interval '7 days' preceding and current row
       ) as rolling_7d_avg
  from measurements;
```

This averages every measurement within the 7 days before the current row's
timestamp — regardless of how many rows that includes. Add data points and
the window automatically widens; remove them and it narrows. This is a
*data-driven* window, not a count-based one.

`RANGE` with offsets requires that the ORDER BY expression is compatible
with the offset type: numeric offset for numeric ordering, interval for
timestamp ordering.

## GROUPS: peer-group counting

`GROUPS` is the PostgreSQL addition (also SQL:2011 standard) between
`ROWS` and `RANGE`. It counts *peer groups* — sets of rows with the same
ORDER BY value — rather than individual rows:

```sql
select score, player,
       sum(points) over (
           order by score
           groups between 1 preceding and 1 following
       ) as neighborhood_total
  from leaderboard;
```

"The current group and the one before and the one after" — regardless of
how many rows are in each group. Where `ROWS 1 PRECEDING AND 1 FOLLOWING`
always picks exactly 3 rows, `GROUPS 1 PRECEDING AND 1 FOLLOWING` picks
one group below, the current group, and one group above — potentially many
more rows if there are ties.

## The EXCLUDE clause

All three frame modes support an `EXCLUDE` sub-clause that removes rows
from the frame *after* it has been computed:

| Clause | Removes from frame |
|---|---|
| `EXCLUDE NO OTHERS` | Nothing (the default) |
| `EXCLUDE CURRENT ROW` | The current row |
| `EXCLUDE GROUP` | The current row and its peers |
| `EXCLUDE TIES` | Peers of the current row, but not the current row itself |

`EXCLUDE TIES` is useful for "rank the current row against others with the
same value without counting itself":

```sql
select player, score,
       count(*) over (
           order by score
           groups between current row and current row
           exclude ties
       ) as is_unique_score
  from leaderboard;
```

## Named windows

If the same `OVER` clause appears multiple times, name it to avoid
repetition:

```sql
select date, amount,
       sum(amount)   over w as running_total,
       avg(amount)   over w as running_avg,
       count(amount) over w as running_count
  from daily_sales
window w as (order by date rows between unbounded preceding and current row);
```

Named windows can also be refined: `OVER (w order by ...)` inherits the
named window's partition and adds an ordering, provided the base definition
has no ordering of its own.

## Quick reference

| Mode | Frame extent | Offset unit | Typical use |
|---|---|---|---|
| `ROWS` | Literal row positions | Rows | Rolling N-row windows, no-tie data |
| `RANGE` | Value-based boundary | Same type as ORDER BY | Time-based windows, peer-inclusive totals |
| `GROUPS` | Peer-group count | Groups of equal values | Tie-aware neighbourhood windows |

+++
title = "Computing Multiple Percentiles at Once with percentile_cont()"
date = "2026-08-15T00:00:00+00:00"
tags = ["PostgreSQL", "SQL", "Aggregation", "Statistics", "Lab"]
categories = ["PostgreSQL", "YeSQL"]
+++

The PostgreSQL project's commit log is in The Lab: every commit since
2000, with the author timestamp (`ats`) and the committer timestamp
(`cts`). The difference is how long a patch waited in review before
landing. The average is 34 hours. The median is under 4 minutes. Both
are true; `percentile_cont()` makes sense of the distribution — median,
90th, 95th, 99th — in a single ordered pass.

<!--more-->
<!--toc-->

## The dataset

The Lab's `commitlog` schema contains the PostgreSQL project's own git
history: every commit with its author timestamp (`ats`) and committer
timestamp (`cts`). The difference `cts - ats` is how long a patch sat in
review before landing. That distribution is not Gaussian — a few patches
wait months or years — so the mean is misleading, and percentiles are
exactly the right tool.

## The basic form

`percentile_cont(fraction)` is an *ordered-set aggregate*. Instead of
operating on all rows as a group (like `sum` or `count`), it sorts them
first, then picks the value at the requested position:

```sql
select project,
       percentile_cont(0.5) within group(order by cts - ats) as median
  from commitlog
 where ats <> cts
 group by project;
```

```
 project  │      median
══════════╪══════════════════
 postgres │ @ 3 mins 46 secs
```

The `WITHIN GROUP (ORDER BY ...)` clause is what makes it an ordered-set
aggregate. The `WHERE ats <> cts` skips patches where the timestamps are
identical (committed in the same second as authored — usually automated
merges with no real review time).

## Multiple percentiles in one call

Here is the key trick: pass an *array* of fractions instead of a scalar.
PostgreSQL returns an array of results, indexed in the same order:

```sql
select project,
       percentile_cont(array[0.5, 0.90, 0.95, 0.99])
          within group(order by cts - ats) as percentiles
  from commitlog
 where ats <> cts
 group by project;
```

```
 project  │ {@ 3 mins 46 secs,
           │  @ 3 hours 21 mins 57 secs,
           │  @ 2 days 22 hours 57 mins 3 secs,
           │  @ 34 days 20 hours 53 mins 12.28 secs}
```

The data sorts once. All four quantiles come from a single pass over
that sorted order. Compare that to computing four separate subqueries or
calling a sorting function four times from application code.

## Unpacking the array

Array subscripts in PostgreSQL start at 1. Wrap the aggregate in a CTE
or derived table to give each percentile a named column:

```sql
with perc_arrays as (
   select project,
          avg(cts - ats) as average,
          percentile_cont(array[0.5, 0.90, 0.95, 0.99])
             within group(order by cts - ats) as parr
     from commitlog
    where ats <> cts
 group by project
)
select project,
       average,
       parr[1] as median,
       parr[2] as "%90th",
       parr[3] as "%95th",
       parr[4] as "%99th"
  from perc_arrays;
```

```
 project  │         average          │      median       │      %90th       │       %95th        │         %99th
══════════╪══════════════════════════╪═══════════════════╪══════════════════╪════════════════════╪══════════════════════════
 postgres │ @ 1 day 10 hrs 36 mins   │ @ 3 mins 46 secs  │ @ 3 hrs 21 mins  │ @ 2 days 22 hrs    │ @ 34 days 20 hrs 53 mins
```

The average is 34 hours. The median is under 4 minutes. Half of all
patches were committed within minutes of being written (likely by the
authors themselves). But the 99th percentile is 34 days — a handful of
patches waited a month or more. An average would have you believing review
takes about a day and a half; percentiles tell the true story.

## percentile_disc vs percentile_cont

`percentile_cont` *interpolates* between adjacent values when the exact
fraction does not land on a row boundary. `percentile_disc` returns the
nearest actual value from the dataset. For continuous data like durations
or measurements, `percentile_cont` is usually what you want. For discrete
data (integer counts, categorical scores), `percentile_disc` is safer.

## Other ordered-set aggregates

The same `WITHIN GROUP (ORDER BY ...)` syntax works for:

- `mode()` — the most frequent value (no fraction needed)
- `rank(value)` and `dense_rank(value)` — the rank a hypothetical value
  would receive in the sorted set
- `percent_rank(value)` and `cume_dist(value)` — the fraction of rows
  that a hypothetical value would beat

{{< lab >}}
```sh
\i starter-kit/03-percentiles.md
```
{{< /lab >}}

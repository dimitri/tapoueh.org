+++
title = "pg_stat_io: Understanding PostgreSQL I/O at a Glance"
date = "2026-12-15T00:00:00+00:00"
tags = ["PostgreSQL", "Performance", "Monitoring", "I/O", "pg_stat_io"]
categories = ["PostgreSQL", "YeSQL"]
+++

PostgreSQL 16 added `pg_stat_io` — a system view that finally breaks down
I/O statistics by *who* is doing the I/O: client backends, autovacuum,
the background writer, the checkpointer, WAL writer, and more. Before this
view existed, you could see that *something* was doing a lot of reads, but
not what. Now you can.

<!--more-->
<!--toc-->

## What pg_stat_io tracks

Each row in `pg_stat_io` describes I/O activity for a combination of:

- **`backend_type`** — which PostgreSQL process did this I/O (client
  backend, autovacuum worker, bgwriter, checkpointer, walwriter, etc.)
- **`object`** — what was accessed (relation, temp relation, WAL, etc.)
- **`context`** — the reason for the access (normal, vacuum, bulkread,
  bulkwrite, etc.)

The key counters per row:

| Column | Meaning |
|---|---|
| `reads` | Blocks read from disk (or OS page cache) |
| `read_time` | Milliseconds spent on reads (if `track_io_timing = on`) |
| `writes` | Blocks written |
| `write_time` | Milliseconds spent on writes |
| `extends` | Relation extensions (new blocks allocated) |
| `hits` | Blocks found in PostgreSQL's shared buffer cache |
| `evictions` | Dirty blocks evicted from shared_buffers to make room |
| `reuses` | Blocks reused from a recycled buffer (no I/O) |
| `fsyncs` | fsync calls |

Statistics accumulate since the last `pg_stat_reset_shared('io')` call
(or server start). Compare across time by snapshotting and diffing.

## Buffer hit ratio

The first thing to check is the buffer hit ratio per backend type:

```sql
select backend_type,
       sum(hits)  as cache_hits,
       sum(reads) as disk_reads,
       round(
           sum(hits)::numeric
           / nullif(sum(hits) + sum(reads), 0) * 100,
           1
       ) as hit_pct
  from pg_stat_io
 where object = 'relation'
   and context = 'normal'
 group by backend_type
 order by disk_reads desc;
```

```
    backend_type     │ cache_hits │ disk_reads │ hit_pct
═════════════════════╪════════════╪════════════╪══════════
 client backend      │  428391042 │   12043891 │    97.3
 autovacuum worker   │   18204310 │    9821044 │    65.0
 bgwriter            │          0 │          0 │
 checkpointer        │          0 │   48203091 │     0.0
```

Client backends are hitting the cache 97.3% of the time — healthy. But
autovacuum is reading from disk 35% of the time, which means it is
regularly touching tables whose pages are not in `shared_buffers`. And the
checkpointer has zero hits because it always flushes dirty pages to disk —
that is expected.

## Finding autovacuum I/O pressure

If autovacuum's disk reads are high, it may be competing with client
queries for buffer space. The solution is usually to tune autovacuum
workers, increase `shared_buffers`, or use `vacuum_cost_delay` to
throttle autovacuum:

```sql
select backend_type, context, reads, writes, extends, hits, evictions
  from pg_stat_io
 where backend_type = 'autovacuum worker'
 order by reads desc;
```

## Evictions: when shared_buffers is too small

`evictions` counts how often PostgreSQL had to write a dirty buffer to
disk to make room for a new one. High evictions from client backends in
the `normal` context means your working set does not fit in
`shared_buffers`:

```sql
select backend_type, context, evictions, reads,
       round(evictions::numeric / nullif(reads, 0) * 100, 1) as eviction_rate_pct
  from pg_stat_io
 where evictions > 0
 order by evictions desc;
```

An eviction rate above a few percent for client backends is a signal to
increase `shared_buffers` (typically to 25% of RAM, up to ~8 GB for most
workloads, beyond which diminishing returns kick in).

## Timing I/O: track_io_timing

`read_time` and `write_time` are only populated when `track_io_timing = on`
(off by default due to clock overhead). Enable it to understand how much
clock time your I/O is consuming:

```sql
alter system set track_io_timing = on;
select pg_reload_conf();
```

Then query `pg_stat_io` to find the slowest I/O context:

```sql
select backend_type, context,
       reads,
       round(read_time) as read_ms,
       round(read_time / nullif(reads, 0), 3) as ms_per_read
  from pg_stat_io
 where reads > 0
   and read_time > 0
 order by read_time desc;
```

A high `ms_per_read` (above 1–2 ms) for client backends indicates slow
storage or OS page cache pressure. Under 0.1 ms is typical for modern
NVMe with a warm OS cache.

## Resetting statistics

```sql
select pg_stat_reset_shared('io');
```

This resets all counters in `pg_stat_io`. Useful before a benchmark or
after a significant configuration change.

## Summary

`pg_stat_io` is the tool to reach for when:

- You know a query is slow but `EXPLAIN ANALYZE BUFFERS` shows too many
  reads and you want to know *which processes* are responsible.
- Autovacuum is impacting query performance (high reads, high evictions).
- You are sizing `shared_buffers` (check eviction rates).
- You suspect I/O is a bottleneck and want to confirm with timing data.

It arrived in PostgreSQL 16 (October 2023). On older versions,
`pg_stat_bgwriter` provides some of this information for the background
writer and checkpointer, but without the per-backend-type breakdown.

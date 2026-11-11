# regionseq

A sequence access method for primary keys that several regions mint at the same
time. It needs a PostgreSQL with the sequence access method patches ("Sequence
Access Methods, round two" on pgsql-hackers, v29 or later); no released version
has them.

## The ids

A `bigint` of 63 bits, most significant first:

| Bits | Content |
|---|---|
| 41 | milliseconds since 2026-01-01 00:00:00 UTC (until 2095) |
| 6 | region number, 0 to 63, from `regionseq.region_id` |
| 4 | timeline of the server, modulo 16 |
| 12 | counter, within one millisecond of one region: 4,096 ids a millisecond, 4.1 million a second |

Ids of one region increase strictly, whatever the clock does. The sequence keeps
the last (millisecond, counter) it made and never goes below it:

* the clock is ahead: the clock's millisecond, counter 0;
* the clock is behind or equal (same millisecond, or the clock was set back):
  the last millisecond, counter + 1;
* the counter would pass 4095: the next millisecond, without waiting for it. A
  region that makes more than 4.1 million ids a second runs ahead of the clock
  for as long as it does, and nothing is refused.

The clock itself is watched: a reading more than `regionseq.max_clock_regression`
(60 s) behind the highest one the sequence has seen is an error, not a slow
crawl.

The timeline changes at every promotion and at a restore to a point in time. A
promoted standby can have a stale copy of the state and a clock behind the old
primary's, and the ids it makes still differ from the old primary's, because the
timeline bits do: sixteen promotions of one region inside the time that a clock was
wrong are needed to repeat an id. Two standbys promoted independently from the
same primary (split brain) get the same timeline, and that is for fencing. Ids stay *increasing* across a promotion only
if the standby has the WAL (a standby that is gated, as with
`synchronized_standby_slots`), by the following.

After a crash or a promotion the ids stay above the ones already made: the WAL
holds a high-water mark one second ahead of the ids, as a plain sequence's
32 pre-logged values do, so at most one WAL record a second per sequence.

## Install

```
make && make install            # PG_CONFIG=/path/to/pg_config if needed
```

```
# postgresql.conf, a different number in every region (a standby has its
# primary's)
shared_preload_libraries = 'regionseq'
regionseq.region_id = 2
```

```sql
CREATE EXTENSION regionseq;

CREATE TABLE orders (
    id bigint GENERATED ALWAYS AS IDENTITY (SEQUENCE NAME orders_id_seq) PRIMARY KEY,
    ...
);
-- or for every sequence created after:
SET default_sequence_access_method = 'regionseq';
```

`regionseq.region_id` has no default: until it is set, `nextval` fails. It can
only be changed by restarting the server.

## Functions

`regionseq_region(id)`, `regionseq_timeline(id)`, `regionseq_time(id)`, `regionseq_counter(id)` decode an
id. A publication's row filter can't call them (they're not built in) but can
say the same with operators, on the key alone:

```sql
CREATE PUBLICATION p_region_2 FOR TABLE orders WHERE (((id >> 16) & 63) = 2);
```

## Limits

* `INCREMENT` and `CACHE` other than 1 are errors. `MAXVALUE` is honoured, so an
  `int` or `smallint` identity column fails at its first `nextval`, with a hint,
  instead of keeping part of an id.
* Logged and temporary sequences only: the state must survive a crash.
* `setval(seq, id)` reads the time and counter of the id and ignores its region.
* One id costs an exclusive lock of the sequence's page, as a plain sequence
  does; there is no per-session cache.

## Why these bits

The id has 63 bits to share between time, capacity, region and timeline, and the
time and capacity share a budget that doesn't depend on the precision of the
clock: `log2(lifetime in ticks) + log2(ids per tick)` is the same for
milliseconds (41 + 12), seconds (31 + 22) and minutes (25 + 28), for the same
lifetime (69 years) and the same peak rate (4.1 million ids a second a region).
A coarser tick doesn't free bits, it only moves them from the time to the
counter, and coarsens the order between regions. The bits are bought by a lower
peak rate or a shorter life; 4,096 a millisecond is what one sequence does under
its lock, so a region never runs ahead of the clock for lack of counter. That
leaves 10 bits: 6 for the region (64 regions) and 4 for the timeline.

The timeline is Postgres's own number for a promotion or a restore to a point in
time. Reports from production show small numbers (11 after about ten switchovers
on a Patroni cluster, the 40s in a restart loop), so 16 values is room for a
flapping cluster within the minutes that a clock or a standby is wrong. What the
timeline can't do is tell apart two standbys promoted independently from the
same primary: they pick the same number.

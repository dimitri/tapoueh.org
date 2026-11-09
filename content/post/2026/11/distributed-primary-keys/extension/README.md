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
| 16 | counter, within one millisecond of one region |

Ids of one region increase strictly, whatever the clock does. The sequence keeps
the last (millisecond, counter) it made and never goes below it:

* the clock is ahead: the clock's millisecond, counter 0;
* the clock is behind or equal (same millisecond, or the clock was set back):
  the last millisecond, counter + 1;
* the counter would pass 65535: the next millisecond, without waiting for it.

A clock that is more than `regionseq.max_clock_regression` (60 s) behind the
last id is an error, not a slow crawl.

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

`regionseq_region(id)`, `regionseq_time(id)`, `regionseq_counter(id)` decode an
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

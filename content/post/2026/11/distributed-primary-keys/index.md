+++
title     = "Distributed primary keys"
date      = "2026-11-09T18:00:00+0100"
tags      = ["PostgreSQL", "Replication", "Data Modeling", "Architecture"]
categories = ["PostgreSQL", "Architecture"]
icon      = "🔑"
+++

A single Postgres server hands out primary keys without effort: a `bigint`
identity column asks one sequence for the next number, and no two rows ever get
the same one. Put writers in several regions, each accepting inserts while the
links between them are slow, down, or both, and that comfortable guarantee
needs rethinking. Two regions that each ask their own sequence for "the next
number" will hand out the same one, and the first time those rows meet on a
subscriber, one of them is rejected.

This article is about that choice: which primary key to give a table that
several regions write at the same time. It continues the logical replication
series. [Part
1](/blog/2026/09/ten-years-of-postgres-logical-replication/#minting-ids-on-the-workers)
listed the ways to mint ids that cannot collide, and [Part
4](/blog/2026/11/postgres-logical-replication-beyond-one-writer/) built the
topology that uses them, a group of nodes per region with every region
subscribed to the others. Here we measure what each choice costs, in size, WAL,
joins, failovers and clocks, on the same workload. We take the
basics for granted: a primary key is a unique, non-null constraint, and a
generated id is a surrogate key that does not replace the natural key, because
it cannot stop two rows that describe the same thing. [Database Normalization
and Primary Keys](/blog/2018/03/database-normalization-and-primary-keys/)
covers both, and what this article adds is where the generated id comes from.

**TL;DR** We compared five primary keys for a table that several regions write
at once, on size, WAL, joins, failovers and clocks. A sequence per region is the
trap: when a standby takes over, it can hand out numbers that were already used.
`uuidv7()` is cheap and needs nothing to coordinate, a composite `(region, id)`
key lets a publication filter on the region without a second index, and a
`bigint` that holds a timestamp, the region and a counter does the same with a
single column, provided the clocks agree. Whatever the key, 32 bits is a wall.

<!--more-->

{{< lab >}}
All the measurements come from throwaway containers on Postgres 19 release
candidate 1, on one laptop, with four writers unless said otherwise. The sizes
and WAL volumes are exact; the rates are only good for comparing the keys with
each other. The scripts and the output of every run are in the [lab
directory](#going-further) of this article.
{{< /lab >}}

<!--toc-->

## The short version

- **On one server, the random UUID is the expensive key.** After a checkpoint it
  writes 487 bytes of WAL per row against 182 for `uuidv7()`, and its index is
  27% larger, though both are the native 16-byte `uuid` type. The difference is
  where the new keys land in the index, which [the single-server
  section](#what-each-one-costs-on-one-server) explains. A `uuidv7()` index
  behaves like a sequence's.
- **A composite `(region, id)` key adds storage overhead in the child tables,
  and filtering on the region adds a second index to maintain.** The composite
  key repeats 10 to 16 bytes in every table that points at it. A uuid needs a
  second index to be filtered by region, about 85 bytes of WAL a row, and a
  `bigint` that holds the region in its bits is filtered on the key alone.
- **When a standby takes over, a sequence can hand out numbers that were
  already used; a key built from the clock can't, unless the new server's
  clock is behind.** A sequence only knows what its standby had received:
  promoted while the standby was a few seconds behind, ours gave out 300,000
  numbers a second time. A time-based `bigint` (a timestamp followed by a
  counter) carries on from the clock instead. If the new primary's clock is
  behind the old one's, its ids are lower than the old ones, and a few of them
  can be equal to ids already given out.
- **A clock that steps back does the same to the time-based `bigint`,** and
  doesn't repeat the ids of `uuidv7()`.
- **A sequence access method could make the key increasing per region through a
  failover or a clock that steps back,** at the price of state to maintain. It
  doesn't exist in any release; we built it on a patch to see what it would
  change, and the section lays out the trade-offs for the reader to weigh.
- **Whatever the key, 32 bits is a wall.**



## What a key has to do here

- **Be unique everywhere**, without asking another region first. That is the one
  thing the single-server sequence doesn't survive.
- **Say who owns the row**, because the topology filters on it: a region
  publishes only its own rows. A row filter can only use columns that the
  replica identity covers, which a composite key does by construction and a
  single-column key doesn't, so we count the extra index it needs.
- **Stay cheap to index and to join on**, since every child table repeats it.
- **Survive a takeover:** a promotion, a switchover, a clock that is not exactly
  right.

## Five candidates

Every table below has the same columns: the key, `region smallint` and
`payload text` (32 bytes). Region `r` of the lab is a number from 0 to 3.

~~~ sql
-- one sequence per region, interleaved: region r runs `increment by 10 start
-- with r + 1`, here region 0
create sequence s increment by 10 start with 1;

create table t
 (
  id      bigint primary key default nextval('s'),
  region  smallint not null,
  payload text not null
);

-- the region in the key, one sequence per region counting by 1
create sequence s;

create table t
 (
  region  smallint not null default 0,
  id      bigint not null default nextval('s'),
  payload text not null,

  primary key (region, id)
);

-- random uuid, and time-ordered uuid
create table t
 (
  id      uuid primary key default gen_random_uuid(),  -- or default uuidv7()
  region  smallint not null,
  payload text not null
);

-- a bigint made of 41 bits of milliseconds since 2026-01-01 UTC, 4 bits of
-- region and 18 of a per-region counter; each region sets app.region
create sequence s;

create table t
 (
  id      bigint primary key default (
            ((extract(epoch from clock_timestamp() - timestamptz '2026-01-01 UTC') * 1000)::bigint << 22)
            | (current_setting('app.region')::bigint << 18)
            | (nextval('s') & 262143)
          ),
  region  smallint not null,
  payload text not null
);
~~~

| Key | How regions avoid each other | Size of one key |
|---|---|---|
| One sequence per region, with a step | the ranges interleave | 8 bytes |
| Composite `(region, id)` | the region is in the key | 2 + 8 bytes, 16 once aligned in a row or an index entry |
| `gen_random_uuid()` (v4) | the space is too large to collide | 16 bytes |
| `uuidv7()` | the same, with a time prefix | 16 bytes |
| Time, region, counter | the region is in the bits | 8 bytes |

The last one is the Snowflake layout, written as an expression in the column
default: time first, so that ids sort by creation, then the region, then a
counter that only has to differ between the ids that one region makes within the
same millisecond.

Each region sets its number once, for its database (`alter database postgres
set app.region = 1`); a custom setting needs a dotted name, which is why it
isn't just `region`. The origin is spelled in UTC, since a bare `'2026-01-01'`
would move by hours with the `TimeZone` of the session that inserts.

{{< image src="fig-bits.svg" title="The 63 bits of the time, region and counter key, and the expressions that read them." >}}

The UUID advice of [2018](/blog/2018/03/database-modelization-anti-patterns/)
was to reach for them when a central sequence would be a single point of
failure, with random numbers in mind. Since then `gen_random_uuid()` became
built in with Postgres 13 and `uuidv7()` with 18 ([the 11-18
selection](/blog/2026/07/sql-improvements-in-postgresql-1118-a-personal-selection/#uuidv7--time-sortable-uuids-pg-18)
has the details), so the choice is no longer between a random key and an
extension, and the overhead of the random one can be measured.

## What each one costs on one server

We start on one server, so that the numbers of the next sections have something
to be compared with. Four writers, one per region number, insert 100 rows per
transaction until the table holds 5,000,000 rows, with the default 128 MB of
`shared_buffers`. Then we run a checkpoint and insert 500,000 more rows, which is
what a server does in the minutes after each checkpoint. That second run is where
the cost of a random key shows, because the first change to a page after a
checkpoint writes the whole 8 kB page to the WAL, a full-page image. Each figure
is the median of three runs:

| Key | Table | Pkey | WAL/row | After checkpoint | FPIs | Index misses |
|---|---|---|---|---|---|---|
| Sequence with a step | 383 MB | 147 MB | 176 bytes | 176 bytes | 8 | 4 |
| `(region, id)` | 422 MB | 158 MB | 183 bytes | 183 bytes | 20 | 16 |
| `gen_random_uuid()` | 422 MB | 200 MB | 197 bytes | **487 bytes** | **24,396** | **198,663** |
| `uuidv7()` | 422 MB | 158 MB | 182 bytes | 182 bytes | 10 | 4 |
| Time, region, counter | 383 MB | 151 MB | 175 bytes | 174 bytes | 9 | 4 |

Pkey is the primary key index. The last three columns are for the 500,000 rows
inserted after the checkpoint: WAL per row, full-page images (FPIs), and index
pages missing from `shared_buffers` (index misses).

{{< image src="fig-wal.svg" title="WAL per row in the 500,000 inserts after a checkpoint." >}}

The random uuid index has about 24,000 pages, and the 500,000 inserts wrote
24,396 full-page images: nearly every page of the index was touched and written
whole once. That's 487 bytes of WAL for each row, which carries about 183 bytes
of its own. The 198,663 index pages missing from `shared_buffers` say the same
from the buffer side: with an index larger than `shared_buffers`, four inserts
in ten found their page somewhere else. The other four keys put a new row into
the rightmost leaf of the index or near it, and read four pages in the same run.
`uuidv7()` behaves like the sequences from the point of view of the index.

{{< image src="fig-btree.svg" title="Where the new rows land in the index: random pages cost a full-page image each, the last page costs one." >}}

The size columns hold two more results. A random uuid's index is 27% larger than
`uuidv7()`'s for the same 16-byte keys, because random inserts split pages that
then stay partly empty, where appending at the right end leaves the pages
behind it full. And the `(region, id)` table is 10% larger than the one with a
single `bigint`: a `smallint` followed by a `bigint` is padded to 16 bytes, and
the whole row to the next multiple of 8, so a composite key of this shape costs
what a uuid costs in the heap.

Others measured the same index behaviour on one writer. [A 2024
benchmark](https://ardentperf.com/2024/02/03/uuid-benchmark-war/) found
`uuidv7()` as fast as a `bigint` and a random UUID about 30% slower, and
[credativ's look at Postgres
18](https://www.credativ.de/en/blog/postgresql-en/a-deeper-look-at-old-uuidv4-vs-new-uuidv7-in-postgresql-18/)
measured the same 27% on the index and 71% leaf density against 90%, with
inserts 12 to 28 times slower for the random key at 50 million rows, a scale we
didn't reach. Neither measures WAL or full-page images, and neither has a
second writer.

### What filtering by region adds

The topology publishes the rows of a region with a filter on `region`. A filter
on `UPDATE` and `DELETE` needs the filter column in the replica identity, so a
table keyed on `id` alone needs a second unique index that includes `region`,
set as the replica identity. Without it, a publication `for table t where
(region = 1)` and a first `UPDATE`:

~~~ results
ERROR:  cannot update table "t"
DETAIL:  Column used in the publication WHERE expression is not part of the replica identity.
~~~

With `create unique index t_id_region on t (id, region)` and `alter table t
replica identity using index t_id_region` the `UPDATE` goes through. The
`(region, id)` key doesn't need any of this, since the key already holds the
column. The single-server benchmark again, with the second index on each of the
other four:

| Key | 2nd index | Rows/s | WAL/row | After<br />checkpoint | FPIs |
|---|---|---|---|---|---|
| Sequence with a step | 204 MB | 1.36 M (was 1.91) | 263 bytes (was 176) | 263 bytes | 11 |
| `gen_random_uuid()` | 270 MB | **0.51 M** (was 1.17) | 301 bytes (was 197) | **954 bytes** | **57,657** |
| `uuidv7()` | 203 MB | 1.01 M (was 1.35) | 266 bytes (was 182) | 266 bytes | 11 |
| Time, region, counter | 211 MB | 1.10 M (was 1.51) | 260 bytes (was 175) | 259 bytes | 11 |

The second index is larger than the primary key it accompanies, because it holds
the key and the region, and it adds about 85 bytes of WAL to every row. For a
random uuid both indexes receive writes at random, and the rows after a
checkpoint cost 954 bytes each, five times the row with a sequence.

The time, region and counter `bigint` doesn't need that index. Its region is in
its bits, and a row filter may use the operators that read them, which are
built in, so the filter reads the key and nothing else. Here is the publisher of
region 1, with the table above, its primary key as the replica identity (the
default), and no other index:

~~~ sql
create publication region_1 for table t where (((id >> 18) & 15) = 1);

-- 1,000 rows made here, with region 1 in the bits of their ids
insert into t(region, payload)
     select 1, 'mine'
       from generate_series(1, 1000);

-- 1,000 rows that came from region 2, with region 2 in the bits of their ids
insert into t(id, region, payload)
     select (2::bigint << 18) | n, 2, 'theirs'
       from generate_series(1, 1000) n;

-- change 100 rows of each, then delete 50 of each
update t set payload = payload || ', updated' where (id & 262143) <= 100;

delete from t where (id & 262143) between 101 and 150;
~~~

Then we count the rows by `payload` on the server that made them (the
publisher) and on the server that receives the publication (the subscriber). The
subscriber has the rows of region 1 with their 100 updates and 50 deletes
applied, and no row of region 2: the filter kept them from being sent at all.
The subscription reports no error.

| `payload` | rows on the publisher | rows on the subscriber |
|---|---|---|
| `mine` | 850 | 850 |
| `mine, updated` | 100 | 100 |
| `theirs` | 850 | 0 |
| `theirs, updated` | 100 | 0 |

A `uuidv7()` has no such bits, so it needs the second index.

## What the composite key costs in the child tables

The composite key looked cheap in the one-server table, and the overhead shows
up in other tables: a table that points at the key repeats both columns, in the
column list and in the index on the foreign key. The [2018
article](/blog/2018/03/database-normalization-and-primary-keys/) shows it with
an `article` keyed on three columns, whose `comment` table repeats all three,
and stops at "quite a big table". We measured how big, with a `parent` table of
1,000,000 rows and a `child` of 5,000,000 rows that has a foreign key to it and
an index on that foreign key:

~~~ sql
-- a single-column key
create table parent
 (
  id      bigint primary key,
  payload text not null
);

create table child
 (
  cid       bigint generated always as identity primary key,
  parent_id bigint not null references parent(id),
  payload   text not null
);

create index on child(parent_id);

-- a composite key: the child repeats both columns, in the foreign key and in
-- its index
create table parent
 (
  region  smallint,
  id      bigint,
  payload text not null,

  primary key (region, id)
);

create table child
 (
  cid           bigint generated always as identity primary key,
  parent_region smallint not null,
  parent_id     bigint not null,
  payload       text not null,

  foreign key (parent_region, parent_id) references parent(region, id)
);

create index on child(parent_region, parent_id);
~~~

| Parent key | Parent | Child | Both | Join |
|---|---|---|---|---|
| `bigint` | 77 + 22 MB | 422 + 171 MB | 692 MB | 395 ms |
| `uuid`<br />(v4 or v7, 16 bytes) | 84 + 40 MB | 465 + 180 MB | 770 MB | 433 ms |
| `(region smallint, id bigint)`<br />columns in key order | 84 + 32 MB | 465 + 180 MB | 761 MB | 450 ms |
| `(region smallint, id bigint)`<br />`bigint` first in the table | 77 + 32 MB | 422 + 180 MB | 710 MB | 519 ms |
| `(region text, id bigint)` | 89 + 52 MB | 482 + 184 MB | 807 MB | 487 ms |

Parent and Child are the heap plus the indexes of each table, and Join is the join
of all the children to their parents. The join times vary by up to 20% from one
run to the next, so the only thing we
read in them is that the `bigint` was the fastest. The sizes are the clearer
result. A `(smallint, bigint)` key in the order we'd naturally write it, region
first, costs what a uuid costs, 16 bytes per row in the heap and in every index
entry, because of the padding between the two columns. Declaring the table with
the `bigint` first and the `smallint` after it, with the primary key still on
`(region, id)`, keeps the heap at the size of the single `bigint` and saves 7%
over the pair of tables. A region kept as text (`ap-southeast` is 13 bytes) is
the most expensive, 17% more than the `bigint` for the pair, and every further
table that points at the key repeats it. We'd take the composite key where few
tables point at it, and a single-column key where many do.

## Across regions: what happens when a standby takes over

### Writers in three regions at once

Three servers on a network, each a region, each subscribed to the other two
with `origin = none` so that a row isn't sent back where it came from. Every
region runs 200 transactions of 100 rows a second for 10 seconds, then we wait
for the three links to catch up and compare the row count and a digest of the
keys on each server.

{{< image src="fig-mesh.svg" title="Three regions that write to each other: every row crosses each link once, and origin = none stops it from going round." >}}

| Key | Written | Per server | Digests | Apply errors |
|---|---|---|---|---|
| Sequence with a step | 612,500 | 612,500 | equal | 0 |
| `(region, id)` | 600,300 | 600,300 | equal | 0 |
| `gen_random_uuid()` | 608,100 | 608,100 | equal | 0 |
| `uuidv7()` | 590,300 | 590,300 | equal | 0 |
| Time, region, counter | 597,000 | 597,000 | equal | 0 |
| Control: every region's sequence starts at 1 | 203,300 | 203,300 | equal | 0 |

Every key does what it's there for: no collision, no stopped subscription, and
the same rows everywhere. The control shows what the test can fail on: the rows
of one region reached the other two before their first batch, and the writers of
those two stopped at the first insert on a duplicate key, which is why only
203,300 rows were written.

### A stale standby

A sequence is state, and a standby's copy of it is as old as the last piece of
WAL it received. We took a primary and a physical standby, a client taking
numbers from a sequence as fast as it could, killed the primary after three
seconds and promoted the standby by hand, so as to control how stale it was. The
first number the new primary hands out, against the last number the client
received, in three runs of each:

| | Streaming standby | Standby cut off three seconds before the kill |
|---|---|---|
| Numbers skipped, which is harmless | 13, 16, 3 | 0, 0, 0 |
| Numbers handed out twice, which are duplicates | 0, 0, 0 | 302,000, 297,000, 299,000 |

{{< image src="fig-stale.svg" title="A standby cut off before the kill hands out numbers the old primary already used." >}}

A sequence logs 32 numbers ahead of itself, so a promotion that follows an
up-to-date standby skips a few numbers and never repeats one. When the standby
hadn't received the last seconds of WAL, the new primary starts where the
standby's copy was and hands out numbers the old primary already used. That is
the failure of the per-region sequence and of the composite key built on it: the
state has to be at least as new as the rows other regions have already
received, or their subscriptions stop on `insert_exists`. A synchronous standby
and a logical slot that doesn't advance before the standby has the WAL (`failover
= true` with `synchronized_standby_slots`, which the next section runs) keep the
state current. After any promotion that may follow a lagging standby we also
move each sequence past the highest id that the other regions hold for that
region, with a `setval` built from the catalog [the way the 2010
article](/blog/2010/02/resetting-sequences.-all-of-them-please/) does it.

A UUID has no state to rewind. The time-based `bigint` has a counter, but the
time in front of it has moved on by at least the time the takeover took, so a
repeated counter value doesn't repeat the key, provided the clocks agree. In six
runs the first id after the promotion was higher than the last one received by
310 to 350 ms of timestamp, from a streaming standby and from a cut-off one.

### A managed takeover, with the clock of the new primary behind

A region in production is managed, and its standby is not stale, so we ran that
too. A region is a [pg_auto_failover](https://oss.theartofpostgresql.com/pg_auto_failover/)
formation of a monitor and two nodes, the second a synchronous standby. A
subscriber in another region follows it with a logical slot created with
`failover = true`, which pg_auto_failover keeps synchronized to the standby and
gates with `synchronized_standby_slots`. A writer inserts 1,000 ids per
statement through a two-host URI, the primary goes away, and node 2 is promoted
with its clock 0 to 60 seconds behind. We count the ids refused for repeating an
id the old primary gave, and the ids made after the takeover that are below the
highest id made before (5 to 7 million ids a run, one run per cell):

| Takeover | Clock behind | Time-based `bigint`: refused / below | `uuidv7()` |
|---|---|---|---|
| Primary killed, writes back after 34 s | 0 s or 2 s | 0 / 0 | 0 / 0 |
| Primary killed | 60 s | 1 / 3,223,000 (61%) | 0 / 3,229,318 (61%) |
| `pg_autoctl perform switchover`, writes back after 2 to 4 s | 5 s | 0 / 289,000 (11%) | 0 / 527,474 (20%) |
| Switchover | 10 s | 2 / 1,376,000 (55%) | 0 / 1,296,000 (53%) |
| Switchover | 60 s | 0 / 1,892,000 (all) | 0 / 2,346,000 (all) |

Whatever the key, no acknowledged row was missing on the new primary, and the
subscriber ended with exactly the rows of the new primary: the failover slot
survived the promotion and the subscription reconnected by itself. The clock keys
are the ones that need the clocks of a region's nodes to agree. A hard failover
takes tens of seconds, longer than a small skew, which hides it; a controlled
switchover takes seconds, and shows a skew of 5 s. When the new primary's clock
is behind, its time-based ids go below what the old primary made, and the plain
`bigint` can repeat one (the refused batches, which the writer retried);
`uuidv7()` never repeats, in exchange for the same inverted order.

{{< image src="fig-clock-behind.svg" title="After a takeover onto a clock that is behind, time-based ids go below the highest one made before; regionseq doesn't." >}}

Three things about the tool, since they came up. With Postgres's default
`checkpoint_timeout` the promotion took 274 seconds instead of 34: the keeper
confirms a promotion once the timeline has advanced, which it appears to read
from the checkpoint that follows, spread over 270 seconds by default; the runs
set `checkpoint_completion_target = 0`. With 2 seconds of pause between two switchovers the
logical slot was gone from both nodes and the subscription had to be recreated,
because the demoted node hadn't had time to receive the slot before the next
promotion; with 30 seconds between switchovers the slot was on both nodes after
each of twenty. A rolling maintenance should check the slot on the standby before
the next switchover.

### Clocks that disagree, and clocks that step back

A `uuidv7()` is as ordered as the clocks that made it. We shifted the timestamp
of three regions with the `uuidv7(interval)` form of the built-in function, as a
skewed clock would, and inserted 180,000 rows at about 3,000 a second from the
three of them in turn. The region shifts are the offsets of the three clocks, and out of order counts
the rows with a key below one already inserted:

| Region shifts | Out of order | Index size | Leaf density |
|---|---|---|---|
| 0, 0, 0 | 0% | 5.7 MB | 90% |
| 0, +5 ms, -5 ms | 66.7% | 5.5 MB | 93% |
| 0, +50 ms, -50 ms | 66.7% | 8.8 MB | 58% |
| 0, +2 s, -2 s | 66.7% | 7.6 MB | 68% |
| one region 1 hour ahead | 66.7% | 8.7 MB | 59% |

With a 5 ms skew two rows in three already arrive with a key lower than one
that's in the index, and the index doesn't care: the three streams of keys land
within the same few pages. At 50 ms and beyond they land on separate parts of
the tree. Only the rightmost page of a B-tree is split to leave it full; any
other page splits in the middle and leaves two half-full pages, and with three
streams of keys, each behind the next, two of them are never at the right edge.
So the index is 28% to 55% bigger from 50 ms of skew up, which is what the
random uuid cost on one server. That is three streams into one server. In the
three-server mesh the index starts at 68% leaf density because the rows arrive
in batches from the apply workers, and a skew of 50 ms or 2 s moved it by 1 to
5 points for `uuidv7()` and for the time-based `bigint`. Uniqueness and
correctness don't depend on the clocks, since the region is in the bits of the
`bigint` and the rest of the uuid is random; the order of keys across regions
does, and so does the index a little.

A clock that steps back inside one region is a different matter, since it makes
the same millisecond come round twice. At full speed, with four writers and the
clock of the server stepped back while they insert, 183 to 197 of about 71,000
transactions of the time-based `bigint` failed on a duplicate key when the step
was 2 seconds, and none at 50 or 200 ms; the `uuidv7()` ones never failed. A
repeat needs the millisecond to come round with the same counter value, so it
takes a step long enough for the counters to line up, and a narrower counter
lines up more often. The failure is loud, bounded to the transactions of the
window, and retried by the application.

## Two traps on the way

### The 32-bit wall

One failure has nothing to do with regions, and every candidate above can hit
it: a key stored on 32 bits. `serial` is the familiar way to get one, and the
width of the column and the width of the sequence behind it are two separate
things. What Postgres 19 builds for each way of writing an auto-numbered column:

~~~ sql
create table t_serial
 (
  id serial primary key
);

create table t_bigserial
 (
  id bigserial primary key
);

create table t_ident
 (
  id int generated always as identity primary key
);

create table t_identbig
 (
  id bigint generated always as identity primary key
);

create sequence s_plain;

create table t_plain
 (
  id int primary key default nextval('s_plain')
);
~~~

~~~ results
    table    | column_type | sequence_type |    sequence_max
-------------+-------------+---------------+---------------------
 t_bigserial | bigint      | bigint        | 9223372036854775807
 t_ident     | integer     | integer       |          2147483647
 t_identbig  | bigint      | bigint        | 9223372036854775807
 t_plain     | integer     | bigint        | 9223372036854775807
 t_serial    | integer     | integer       |          2147483647
~~~

A `serial` or an `int` identity column gets a sequence of its own width, which
stops at 2,147,483,647 with a clear message, before it hands out anything the
column can't hold. The dangerous one is the last kind: a sequence created on its
own, which is `bigint` unless we say otherwise, used as the default of an `int`
column. The sequence goes on, and the column is the one that stops, with `ERROR:
integer out of range`. Every failed insert took a value from the sequence
anyway, so the sequence is already two past the wall after two attempts, and
nothing in the error says which side to look at. Sequences are not
transactional, so an application that retries a failing insert in a loop moves
the sequence faster than it adds rows.

A `serial` column made before Postgres 10 is the same combination, found in the
wild rather than built by hand: before the `AS` clause of `CREATE SEQUENCE`, a
sequence was always a `bigint`, including the one behind a `serial`, and the
dump of `pg_dump` carries no `AS integer` for it, so it stays that way through
every upgrade. This query lists the integer columns whose owned sequence is
wider than the column:

~~~ sql
select c.oid::regclass as "table",
       a.attname as "column",
       a.atttypid::regtype as column_type,
       q.oid::regclass as sequence,
       s.seqtypid::regtype as sequence_type,
       s.seqmax
  from pg_sequence s
  join pg_class q on q.oid = s.seqrelid
  join pg_depend d
    on d.objid = q.oid
   and d.classid = 'pg_class'::regclass
   and d.refclassid = 'pg_class'::regclass
   and d.deptype in ('a', 'i')
  join pg_class c on c.oid = d.refobjid
  join pg_attribute a
    on a.attrelid = c.oid
   and a.attnum = d.refobjsubid
 where a.atttypid in ('int2'::regtype, 'int4'::regtype)
   and s.seqmax
     > case a.atttypid when 'int2'::regtype then 32767 else 2147483647 end;
~~~

In a system with several writers the wall comes closer than the table size
suggests. A sequence per region with a step of 10 spends 10 values for each row
it keeps, which leaves 214,748,364 rows per region before the wall, about 25
days of a writer doing 100 inserts a second. Widening a column later rewrites
the table, and every foreign key that points at it has to follow. DDL isn't
replicated either, so a link has two columns to widen and the order matters:
widen the receiving side first, the publishing side after, or the subscriber
stops at the first value past 2,147,483,647, which can be days after the `ALTER
TABLE`. In a mesh where every region both publishes and subscribes, all of them
have to be widened before any starts minting large ids. The cheaper way is the
first day's: `bigint`, and `generated always as identity` over `serial`.

### `uuidv7()` before Postgres 18

The built-in `uuidv7()` arrived with Postgres 18, and the regions of a system
don't all upgrade on the same day. A function that is short enough to write in
SQL fills the gap: it takes a random v4 uuid, overwrites its first 48 bits with
the Unix time in milliseconds, and sets the version nibble to 7. The variant
bits that `gen_random_uuid()` already put there are the right ones for [RFC
9562](https://www.rfc-editor.org/rfc/rfc9562).

~~~ sql
create function public.uuidv7()
 returns uuid
language sql
volatile
parallel safe
      as $$
select encode(
         set_bit(
           set_bit(
             overlay(
               uuid_send(gen_random_uuid()) placing substring(int8send((extract(epoch from clock_timestamp()) * 1000)::bigint) from 3) from 1 for 6
             ),
             52, 1
           ),
           53, 1
         ),
         'hex'
       )::uuid
$$;
~~~

`gen_random_uuid()` is built in from Postgres 13; before that, `pgcrypto` has
it. On 13, 17 and 19 the version nibble is 7, the variant is `8`, `9`, `a` or
`b`, and the 48 bits read back as the current time. Two differences with the
built-in one are worth knowing. We generated a million ids in a single statement
and counted how many are smaller than the one before:

| | ids smaller than the previous one | time for 1,000,000 ids |
|---|---|---|
| SQL function | about 500,000 | 810 ms |
| built-in `uuidv7()` | 0 | 472 ms |

The SQL function fills everything after the millisecond with random bits, so two
ids made in the same millisecond come out in random order. Across milliseconds
the order holds, and that's what the index cares about: new keys land in the
pages of the last few milliseconds, not anywhere in the tree. The built-in
function spends 12 bits on the sub-millisecond fraction and stays ordered inside
a session. The second difference is name resolution: an unqualified `uuidv7()`
finds `pg_catalog` first, so on 18 and later a call written without a schema runs
the built-in function, but a column default is dumped with the schema
(`DEFAULT public.uuidv7()`) and keeps calling the SQL one. Once every region runs
18 or later, `alter table ... alter column id set default uuidv7()` resolves to
the built-in one, and `public.uuidv7()` can go.

Last, a `uuidv7()` value carries the time of its creation to the millisecond.
When the key appears in a URL or an API, that's a public timestamp for the row,
which a random v4 doesn't give away.

## Which key to pick

- **One writer region, the others only read:** `bigint generated always as
  identity`. There is nothing to coordinate, the 32-bit wall is the one trap, and
  the query above finds the columns that are on it.
- **Several writer regions, each row owned by one, few tables pointing at the
  key:** `(region, id)`, with one identity column per region and the `bigint`
  first in the table. The filter works without a second index, no clock is
  involved and there is no step to outgrow. It takes 3% to 10% more storage in a
  pair of tables, and each region's sequence has to survive a failover, which a
  synchronous standby and a gated slot ensure.
- **The same, with many tables pointing at the key:** a single-column key, which
  takes 8 or 16 bytes in every child instead of 10 or 16. Either the time, region
  and counter `bigint`, filtered on its bits (`where ((id >> 18) & 15) = 1`, no
  second index), or `uuidv7()` with a `region` column and a unique `(id, region)`
  as replica identity. The `bigint` needs nothing else and survives a promotion,
  but it repeats ids when a clock steps back by seconds, and it is an expression
  to write and keep, with 16 regions at most. `uuidv7()` is a built-in function
  and never repeats, and its companion index adds about 85 bytes of WAL per row.
- **Rows created anywhere, nobody filtering on the owner:** `uuidv7()`. It takes
  16 bytes, has an index that behaves like a sequence's, and needs nothing to
  coordinate, as long as the clocks agree to better than the time a takeover
  takes.
- **The creation time must not leak:** `gen_random_uuid()`. Its index is 27%
  larger, it writes 2.7 times the WAL after a checkpoint (487 bytes a row against
  182), and 5 times with a companion index (954).

If we had to keep one rule: `uuidv7()` where nothing filters on the owner and the
clocks are kept in sync, `(region, id)` where the owner is the point, a random
uuid only when the creation time must stay hidden, and never 32 bits. For a table
that already has a sequence, the first steps come before the key: the query above
for its width, and the step and headroom arithmetic of [Part
1](/blog/2026/09/ten-years-of-postgres-logical-replication/#minting-ids-on-the-workers)
if it's to become one of several writers. We'd keep the old tables single-writer and use the new
key for new ones.

## What a sequence access method could change

The time-based `bigint` is an expression because nothing lets a sequence compute
its own next value. Two patch sets, from 2013 to 2016 and from 2023 to 2026,
tried to add that, as `create sequence ... using method`, so that a `serial` or
an identity column could be fed by a Snowflake-style id instead of a counter.
Both were returned with feedback; the reviews of the second asked for the use
case before the code, and in May 2026 its author dropped it from the CommitFest.
This article is a use case, so we asked what a method would change. Nothing
indicates that the facility lands in Postgres 20, and it may well not, so what
follows is a thought experiment, not a feature to wait for. If Postgres 20 did
ship it, a method would be an extension like any other, loaded with
`shared_preload_libraries`, and the question would be what it does for a
multi-region key that the other keys don't. We built the second patch set into a
Postgres development build and wrote the method, `regionseq`, as an extension to
measure that.

An id of `regionseq` is 41 bits of milliseconds, 6 of region from a server
setting that has no default, 4 of timeline and 12 of counter. The sequence keeps
the last millisecond and counter it made and never goes below them: when the
clock is behind it keeps counting in the last millisecond, and when the counter
runs out it takes the next one instead of waiting. A high-water mark one second
ahead is logged to the WAL, so ids stay above what was made after a crash and on
a standby, and the timeline bits, which change at every promotion, make a
promoted standby's ids differ from the old primary's even if its copy of the state
is stale. Sixteen values for the timeline is what production reports suggest:
timelines are in the tens (11 after about ten switchovers in a
[forum thread](https://forums.percona.com/t/patronictl-tl-mean/27989), 40 to 81 on
a [Patroni test cluster](https://github.com/patroni/patroni/issues/1733) that is
killed repeatedly), and what counts is how many promotions fall within the
seconds or minutes that a standby or a clock is wrong. The table, the index and
the WAL are those of the expression, since a method makes the same `bigint`.

{{< image src="fig-bits-regionseq.svg" title="The 63 bits of a regionseq id." >}}

What a key can promise comes in three levels. **Unique:** an id is never made
twice. **Increasing per region:** what one region makes only goes up, which is
what a consumer that remembers "the last id I saw from this origin" needs, and
with the region in the key it can ask for one origin at a time. **Time-like:**
the time in the id is the creation time. Across regions only the last is
available, and not exactly: replication lag already puts a row from another
region below ids that a reader has seen, so no key lets a reader keep one cursor
over all regions, and the second level is the one to design on.

The same tests, on the keys that carry a clock (one run per cell, about 200,000
ids for the first lines):

| | Time-based `bigint` | `uuidv7()` | `regionseq` |
|---|---|---|---|
| Clock steps back 2 s / 10 s: ids lower than one the same client already got | 18% / 59% | 0 / 0 | 0 / 0 |
| Same, lower than one any client got | 18% / 59% | 16% / 47% | 0.1% |
| Time in the id ahead of the clock | none | up to 2 s / 10 s | up to 2 s / 10 s |
| Takeover onto a clock 60 s behind, standby current: repeats / ids below | 1 / 61% | 0 / 61% | 0 / 0 |
| Switchover onto a clock 5 s, 10 s, 60 s behind: ids below | 11%, 55%, all | 20%, 53%, all | 0, 0, 0 |
| Failover from a cut-off standby with a clock 2 s behind: repeated ids | 782 of 200,000 | 0 | 0 |
| `int` column | `integer out of range` | not applicable | `reached maximum value`, with a hint |

{{< image src="fig-ids-below.svg" title="The same switchover measurement, for the three keys that carry a clock." >}}

`uuidv7()` keeps each session increasing, because a backend remembers its last
timestamp and adds to it, and the sessions do it separately, so a region's ids
interleave. `regionseq` keeps the whole region increasing, because the state
is one place. Both run ahead of the clock until it catches up, so their time stops
being the creation time. The timeline bits did what they are for: without them, a
cut-off standby with a clock behind repeated 24% of the ids of its new
timeline. And twenty switchovers in a row under pg_auto_failover, the timeline
going from 1 to 21 so that the four bits wrap (`1 ... 15 0 1 ... 5`), produced no
repeat, no id below an earlier one and no lost row over 3,882,000 ids, because
with a current standby the state carries the order and the timeline is only the
second line of defence. Two standbys promoted independently from the same primary
pick the same timeline number, which pgsql-hackers is discussing in 2026 and
which fencing has to prevent.

A method changes two things that the other keys leave to the system around them,
and costs three. Whether the trade is worth it depends on the distributed setup,
not on the key.

**What it adds**

- **Resilience to a failover.** A sequence per region stays unique only if the
  standby that takes over is current, which a synchronous standby and a gated
  slot ensure. The clock keys stay unique if the clocks agree within the time of
  a takeover. `regionseq` stays unique whatever the standby had received, because
  the timeline bits change at every promotion, and it keeps increasing when the
  standby is current. Two standbys promoted independently from the same primary
  still pick the same timeline, so fencing stays necessary.
- **Resilience to clock skew.** When the clock steps back, or a takeover lands on
  a server whose clock is behind, the time-based `bigint` makes ids below earlier
  ones and some repeats, and `uuidv7()` makes ids below earlier ones from other
  sessions. `regionseq` keeps counting from the last id it made: no id was below an
  earlier one in the switchover runs. A consumer that reads one origin as a
  stream, remembering the last id it saw from that region, can rely on that.

**What it costs**

- **The time in the id stops being the creation time** while the clock catches up,
  by up to the size of the step: the ids run ahead of it.
- **State to keep correct.** The high-water mark goes through the WAL, the
  library is preloaded, and the region number comes from a server setting that has
  no default, because the same number on two servers makes the same ids on both.
  The expression has none of this, and sits in the column default.
- **A smaller key space.** 64 regions, 16 timeline values that wrap, 41 bits of
  milliseconds. And the method is an extension that no release provides today.

What the method doesn't change: the table, the index and the WAL, which are those
of the expression, and the need for synchronous standbys and watched clocks:
without them, a method fails the way the other keys do. Each setup
decides if these two resiliences matter. They do for a system that reads ids as a
stream per origin and can't ask its clocks to behave; they matter less where
`uuidv7()` or `(region, id)` runs with synchronous standbys and clocks that are
monitored. What would change this section is a use with state that an expression
can't hold, ranges handed out by consensus or gapless numbers, which we didn't
test.

## What we didn't test

- **Scale and disk.** The tables hold 5,000,000 rows on tmpfs, so the index pages
  that a random uuid missed in `shared_buffers` came from the operating system's
  cache, which is the case where a random uuid costs least. Nikolay Samokhvalov
  [measured Postgres 18.6](https://postgres.ai/blog/20261105-uuidv4-vs-uuidv7) on
  50 to 100 million rows on local SSD, and his sizes and WAL agree with ours. He
  also shows where the rate gap starts: random uuids and ordered keys insert at
  nearly the same rate while the index fits in memory, and at 100 million rows the
  random uuid inserted at 0.62 to 0.69 times the rate of an ordered key.
- **Standby replay.** We counted WAL volume, not the time a standby takes to
  replay it. Samokhvalov's measurements have a random uuid replaying 2.4 times
  slower per row than the ordered keys before the memory cap, and 5.5 times slower
  past it.
- **Updates and deletes.** Only inserts were measured. A companion index set as
  the replica identity also changes what an `UPDATE` writes to the WAL, and we
  didn't count it.
- **Real clocks.** Skew and steps are simulated, with `uuidv7(interval)` for the
  first test and libfaketime offsets on one machine everywhere else, and the
  takeover tables are one run per cell. Our skews are 2 seconds and less in the
  mesh. Samokhvalov put a skew of 60 seconds on `uuidv7()` generated by the client
  and got an index 69% full, with 45% of its leaves out of order, and recommends
  generating it on the server.
- **A larger formation, and the edges of the timeline bits.** One region of two
  nodes and one subscriber; three nodes, priorities and a partition instead of a
  kill are not run, nor the repeat that a stale standby, a clock behind and sixteen
  promotions would make, nor the shortest pause between switchovers that keeps a
  failover slot.
- **More than three regions in the mesh,** and keys made by the application, with
  an ORM or a library: they bring their own `uuid` version and their own clock.
- **Other uses of a sequence access method:** ranges handed out by consensus,
  gapless numbers, pseudo-random sequences.
- **Postgres 19 after release candidate 1.** The tools were pg_auto_failover's
  main branch and a Postgres 19 development build for the last section.

## Going further

If the SQL and schema design side of this is your daily work, [The Art of
PostgreSQL](https://theartofpostgresql.com) is the book for it, and every query
of the book runs in [the Lab](https://theartofpostgresql.com/lab/) against real
data. The [self-paced course](https://theartofpostgresql.com/course/) and the
[Masterclass](https://theartofpostgresql.com/masterclass/) are the guided
versions. [pg_auto_failover](https://oss.theartofpostgresql.com/pg_auto_failover/),
used in the takeover tests above, has its own page for support and the roadmap.

The scripts, the SQL and the output of every run quoted here are in this
article's directory of the [blog's source
repository](https://github.com/dimitri/tapoueh.org/tree/master/content/post/2026/11/distributed-primary-keys):
`compose/` has one script per experiment and a README that maps them to the
tables above, `results/` the raw output, and `extension/` the `regionseq`
extension, with its regression test.

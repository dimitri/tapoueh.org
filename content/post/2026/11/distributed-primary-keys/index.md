+++
title     = "Distributed primary keys"
date      = "2026-11-09T18:00:00+0100"
tags      = ["PostgreSQL", "Replication", "Data Modeling", "Architecture"]
categories = ["PostgreSQL", "Architecture"]
icon      = "🔑"
+++

<!--
Lab: compose/ has one script per experiment (README.md), sql/ the SQL scripts,
results/ the output of every run quoted below. Servers are postgres:19rc1
containers on tmpfs, except in the last section, which runs a build of the
sequence access method patches (compose/seqam/) with our extension/ built in.

Already on the site, cited and not repeated: Part 1 ("Minting ids on the
workers"), Part 4, the consolidation article, and the 2018 "Database
Normalization and Primary Keys" and "Database Modelization Anti-Patterns".
-->

A single Postgres server hands out primary keys without effort: a `bigint`
identity column asks one sequence for the next number, and no two rows ever get
the same one. Put writers in several regions, each accepting inserts while the
links between them are slow, down, or both, and that comfortable guarantee
needs rethinking. Two regions that each ask their own sequence for "the next
number" will hand out the same one, and the first time those rows meet on a
subscriber, one of them is rejected.

This article is about that choice, for the multi-writer system we built in [Part
4 of the logical replication
series](/blog/2026/11/postgres-logical-replication-beyond-one-writer/): which
primary key to give a table that several regions write at the same time. [Part
1](/blog/2026/09/ten-years-of-postgres-logical-replication/#minting-ids-on-the-workers)
listed the ways to mint ids that cannot collide, and showed that they work. It
did not measure what they cost, and that's what we do here, on the same
workload: a sequence per region with its own step, a composite `(region, id)`
key, random UUIDs, time-ordered
[`uuidv7()`](/blog/2026/07/sql-improvements-in-postgresql-1118-a-personal-selection/#uuidv7--time-sortable-uuids-pg-18),
and a `bigint` made of a timestamp, a region and a counter. We look at key size,
index locality, WAL and queries, and at what each choice allows the replication
links to do. What a primary key is, and why a surrogate key doesn't replace a
natural one, is [an older
story](/blog/2018/03/database-normalization-and-primary-keys/); we only use it.

What we found, in short:

- **On one server, the random UUID is the expensive key.** After a checkpoint it
  writes 487 bytes of WAL per row against 182 for `uuidv7()`, and its index is
  27% larger. A `uuidv7()` index behaves like a sequence's.
- **Across regions, every key we tried converged without a collision.** They
  differ in price. A composite `(region, id)` repeats 10 to 16 bytes in every
  table that points at it, and a uuid needs a second index to be filtered by
  region, which is about 85 bytes of WAL per row. A `bigint` that holds the
  region in its bits is filtered on the key alone.
- **A per-region sequence is state that a failover can rewind.** Promoted from
  a lagging standby, it hands out numbers that were already used. A UUID has no
  such state.
- **A key made from the clock repeats if the clock goes back.** A `bigint` of
  milliseconds and a counter survived every failover we ran, and failed 0.2% to
  0.3% of its transactions when the clock stepped back 2 seconds. A `uuidv7()`
  didn't.
- **`uuidv7()` stays cheap while the clocks of the regions agree to within a few
  milliseconds.** With 50 ms of skew its index was up to 55% bigger when one
  server took three streams, and 1 to 5 points less dense in a mesh.
- **A key made of the clock needs the new primary's clock not to be behind.**
  Promoted with a clock 2 seconds behind, the expression repeated ids and
  `uuidv7()` made ids below the old ones.
- **Whatever the key, 32 bits is a wall.** `serial` and `int` identity columns
  stop at 2,147,483,647, and a `serial` column made before Postgres 10 can hide
  a `bigint` sequence behind an `int` column.

The last sections are a table that says which key to use when, and whether
Postgres would need a sequence access method for it, with failovers and clocks
that disagree: we wrote one, and it's nice, not needed.

## What a key has to do here

- **Be unique everywhere**, without asking another region first. That is the
  one thing the single-server sequence doesn't survive; Part 1 shows the
  `insert_exists` error and the subscription that stops on it.
- **Say who owns the row**, because the topology of Part 4 filters on it. A
  row filter can only use columns covered by the replica identity, and Part 1
  shows what the first `UPDATE` does otherwise. For a composite key that's
  already satisfied; for a single-column key it needs a second index, which
  is a cost we count.
- **Stay cheap to index and to join on**, since every child table repeats it.

## Five candidates

Every table below has the same columns: the key, `region smallint` and
`payload text` (32 bytes). Region `r` of the lab is a number from 0 to 3.

~~~ sql
-- one sequence per region, interleaved: region r runs `increment by 10 start with r + 1`
create table t (id bigint primary key, region smallint not null, payload text not null);

-- the region in the key, one sequence per region counting by 1
create table t (region smallint not null, id bigint not null, payload text not null,
                primary key (region, id));

-- random uuid, and time-ordered uuid
create table t (id uuid primary key, region smallint not null, payload text not null);
-- default gen_random_uuid()    /    default uuidv7()

-- a bigint made of 41 bits of milliseconds, 4 of region and 18 of a per-region counter
create table t (id bigint primary key, region smallint not null, payload text not null);
-- default ((ms_since_2026 << 22) | (region << 18) | (nextval('s') & 262143))
~~~

[Part
1](/blog/2026/09/ten-years-of-postgres-logical-replication/#minting-ids-on-the-workers)
has the output of the interleaved sequences and the warning about headroom, and
shows the composite key and `uuidv7()` at work. It mentions the random uuid for
its reputation only. The fifth key is the shape of the `snowflakeid` kind it
mentions for EDB Postgres Distributed, written as an expression: time first so
that ids sort by creation, then the region, then 18 bits of a per-region
sequence, which only has to differ between the ids that one region makes within
the same millisecond.

| Key | How regions avoid each other | Size of one key |
|---|---|---|
| One sequence per region, with a step | the ranges interleave | 8 bytes |
| Composite `(region, id)` | the region is in the key | 2 + 8 bytes, 16 once aligned in a row or an index entry |
| `gen_random_uuid()` (v4) | the space is too large to collide | 16 bytes |
| `uuidv7()` | the same, with a time prefix | 16 bytes |
| Time, region, counter | the region is in the bits | 8 bytes |

## What each one costs on one server

We start on one server, so that the numbers of the next sections have something
to be compared with. Four writers, one per region number, insert 100 rows per
transaction until the table holds 5,000,000 rows, on a Postgres 19rc1 with the
default 128 MB of `shared_buffers`. Then we run a checkpoint and insert 500,000
more rows, which is what a server does in the minutes after each checkpoint.
That second run is where the cost of a random key shows, because the first
change to a page after a checkpoint writes the whole 8 kB page to the WAL, a
full-page image. Each figure is the median of three runs (`compose/run-single.sh`,
`results/30-single-*.out`):

| Key | Table | Primary key index | WAL per row | WAL per row after a checkpoint | Full-page images | Index pages missing from `shared_buffers` |
|---|---|---|---|---|---|---|
| Sequence with a step | 383 MB | 147 MB | 176 bytes | 176 bytes | 8 | 4 |
| `(region, id)` | 422 MB | 158 MB | 183 bytes | 183 bytes | 20 | 16 |
| `gen_random_uuid()` | 422 MB | 200 MB | 197 bytes | **487 bytes** | **24,396** | **198,663** |
| `uuidv7()` | 422 MB | 158 MB | 182 bytes | 182 bytes | 10 | 4 |
| Time, region, counter | 383 MB | 151 MB | 175 bytes | 174 bytes | 9 | 4 |

The last three columns are for the 500,000 rows inserted after the checkpoint.
The random uuid index has about 24,000 pages, and the 500,000 inserts wrote
24,396 full-page images: nearly every page of the index was touched and written
whole once. That's 487 bytes of WAL for each row, which carries about 183 bytes
of its own. The 198,663 index pages missing from `shared_buffers` say the same
from the buffer side: with an index larger than `shared_buffers`, four inserts
in ten found their page somewhere else. The other four keys put a new row into
the rightmost leaf of the index or near it, and read four pages in the same run.
`uuidv7()` behaves like the sequences from the point of view of the index, which
is the claim that Part 1 and the 11-18 selection made and that we hadn't
measured.

The size columns hold two more results. A random uuid's index is 27% larger than
`uuidv7()`'s for the same 16-byte keys (200 MB against 158 MB), because
random inserts split pages that then stay partly empty, where appending at the
right end leaves the pages behind it full. And the `(region, id)`
table is 39 MB, 10%, larger than the one with a single `bigint`, for ten bytes of
key. A `smallint` followed by a `bigint` is padded to 16 bytes, and the whole
row to the next multiple of 8, so a composite key of this shape costs what a
uuid costs in the heap.

Rows per second, from the same runs, were 1.9 million for the sequences, 1.8
million for `(region, id)`, 1.5 million for the time-based `bigint`, 1.35
million for `uuidv7()` and 1.2 million for `gen_random_uuid()`. It's a lab on
tmpfs with four writers on six cores, so we only read the order of magnitude: the
differences between the first four rows are the cost of building the key (a
regclass lookup for `nextval`, an expression, a function call), not of the
index, and only the random uuid is slower for the reason that matters.

### The price of being able to filter

The topology of Part 4 publishes the rows of a region with a filter on
`region`. A filter on `UPDATE` and `DELETE` needs the filter column in the
replica identity, so a table keyed on `id` alone needs a second unique index
that includes `region`, set as the replica identity. We checked it on the uuid
case: a publication `for table t where (region = 1)`, a first `UPDATE`:

~~~ results
ERROR:  cannot update table "t"
DETAIL:  Column used in the publication WHERE expression is not part of the replica identity.
~~~

With `create unique index t_id_region on t (id, region)` and `alter table t
replica identity using index t_id_region`, the `UPDATE` goes through and the
filtered row changes on the subscriber (`results/50-filter-uuid.out`). The
`(region, id)` key doesn't need any of this, since the key already holds the
column. We ran the single-server benchmark again with the second index on
each of the other four:

| Key, with the companion index | Second index | Rows per second | WAL per row | After a checkpoint | Full-page images |
|---|---|---|---|---|---|
| Sequence with a step | 204 MB | 1.36 M (was 1.91) | 263 bytes (was 176) | 263 bytes | 11 |
| `gen_random_uuid()` | 270 MB | **0.51 M** (was 1.17) | 301 bytes (was 197) | **954 bytes** | **57,657** |
| `uuidv7()` | 203 MB | 1.01 M (was 1.35) | 266 bytes (was 182) | 266 bytes | 11 |
| Time, region, counter | 211 MB | 1.10 M (was 1.51) | 260 bytes (was 175) | 259 bytes | 11 |

The second index is larger than the primary key it accompanies, at 203 to 211
MB for the keys that append, because it holds the key and the region. It adds
about 85 bytes of WAL to every row, half as much again as the row's own cost.
For a random uuid the two indexes both receive writes at random, and the 500,000
rows after a checkpoint cost 477 MB of WAL, 954 bytes each, five times the row
with a sequence.

The time, region and counter `bigint` doesn't have to pay it, though. Its region
is in its bits, and a row filter may use the operators that read them, which are
built in, on the key alone: `where ((id >> 18) & 15) = 1`. With the primary key
as the replica identity, one index on the table and no `region` column in the
filter, a publisher of region 1 that inserted 1,000 rows of its own and 1,000
of region 2, then updated and deleted 100 and 50 of each, sent the subscriber
the rows of region 1 only, the updates and the deletes included, with no apply
error (`results/111-region-filter-expression.out`). So the line "Time, region,
counter" of the table above is the price of filtering on a `region` column, and
the bits make it unnecessary; a `uuidv7()` has no such bits, and pays.

## What each one costs across regions

### Writers in three regions at once

Three servers on a network, each a region, each subscribed to the other two
with `origin = none` so that a row isn't sent back where it came from. Every
region runs 200 transactions of 100 rows a second for 10 seconds
(`compose/run-mesh.sh`, `results/80-mesh-*.out`), then we wait for the three
links to catch up and compare the row count and a digest of the keys on each
server:

| Key | Rows written | Rows on each of the three | Digests | Apply errors |
|---|---|---|---|---|
| Sequence with a step | 612,500 | 612,500 | equal | 0 |
| `(region, id)` | 600,300 | 600,300 | equal | 0 |
| `gen_random_uuid()` | 608,100 | 608,100 | equal | 0 |
| `uuidv7()` | 590,300 | 590,300 | equal | 0 |
| Time, region, counter | 597,000 | 597,000 | equal | 0 |
| Control: every region's sequence starts at 1 | 203,300 | 203,300 | equal | 0 |

Every key does what it's there for: no collision, no stopped subscription, and
the same rows everywhere at the first check after the writers stopped. The
control shows what the test can fail on, in a different place than Part 1's:
the rows of one region reached the other two before their first batch, and the
writers of those two stopped at the first insert on a duplicate key (`Key
(id)=(2) already exists`), which is why only 203,300 rows were written. With a
slower link the collision happens on the subscriber instead, as Part 1 shows.

### After a failover

A sequence is state, and a standby's copy of it is as old as the last piece of
WAL it received. We took a primary and a physical standby, a client taking
numbers from a sequence as fast as it could, killed the primary after three
seconds and promoted the standby by hand, so that we control how stale it is
(`compose/run-failover.sh`,
`results/70-failover-*.out`). The first number the new primary hands out, against
the last number the client received:

| Standby | Run 1 | Run 2 | Run 3 |
|---|---|---|---|
| Streaming | skips 13 numbers | skips 16 | skips 3 |
| Cut off from the primary three seconds before the kill | hands out 302,000 numbers a second time | 297,000 | 299,000 |

A sequence logs 32 numbers ahead of itself, so a promotion that follows an
up-to-date standby skips a few numbers and never repeats one. When the standby
hadn't received the last seconds of WAL, the new primary starts where the
standby's copy was and hands out numbers the old primary already used. The
second row is an extreme, since the standby here received nothing after the
sequence was created, but any lag at the moment of the kill gives the same
result for the numbers inside it.

What this means for the keys: the per-region sequence of the first two keys is
state that has to be at least as new as the rows other regions have already
received. [Part
4's](/blog/2026/11/postgres-logical-replication-beyond-one-writer/#a-region-is-a-group-of-three)
`synchronized_standby_slots` provides that, since a subscriber isn't sent a row
before the standby has it, and the `nextval` record comes before the row's
commit in the WAL: an argument from the order of the WAL, and the last section's
drill runs it under pg_auto_failover. Without it, a region whose standby lagged at the time of the
failover hands out ids that other regions hold, and their subscriptions stop on
`insert_exists`. A uuid has no state to rewind. The time-based `bigint` has a
counter, but the time in front of it has moved on by at least the time the
failover took, so a repeated counter value doesn't repeat the key as long as the clocks of the two servers agree: in six runs
(`results/106-failover-snowflake-*.out`) the first id after the promotion was
higher than the last one received by 1.3 to 1.5 billion, which is 310 to 350 ms
of the timestamp bits, from a streaming standby and from a cut-off one. With the
new primary's clock behind, it isn't so; the last section measures it.

After a promotion that may follow a lagging standby, we move each sequence past
the highest id the other regions hold for that region, with a `setval` built
from the catalog the way [the 2010
article](/blog/2010/02/resetting-sequences.-all-of-them-please/) builds it,
before the region accepts writes. Postgres 19's sequence synchronization, which
[Part
1](/blog/2026/09/ten-years-of-postgres-logical-replication/#what-about-sequences)
and [the consolidation
article](/blog/2026/10/consolidating-databases-with-postgres-logical-replication/#sequences-dont-replicate-either)
cover, copies a sequence's position to a subscriber; it doesn't make a writer's
own sequence safer in this case.

### Clocks that disagree

A `uuidv7()` is as ordered as the clocks that made it. We shifted the timestamp
of three regions with the `uuidv7(interval)` form of the built-in function, as
a skewed clock would, and inserted 180,000 rows at about 3,000 a second from the
three of them in turn (`compose/run-skew.sh`, `results/90-skew.out`). The region
shifts are the offsets of the three clocks; the index is the primary key on the
uuid:

| Region shifts | Rows with a key below one already inserted | Index size | Leaf page density |
|---|---|---|---|
| 0, 0, 0 | 0% | 5.7 MB | 90% |
| 0, +5 ms, -5 ms | 66.7% | 5.5 MB | 93% |
| 0, +50 ms, -50 ms | 66.7% | 8.8 MB | 58% |
| 0, +200 ms, -200 ms | 66.7% | 7.3 MB | 70% |
| 0, +2 s, -2 s | 66.7% | 7.6 MB | 68% |
| 0, +1 min, -1 min | 66.7% | 8.5 MB | 60% |
| one region 1 hour ahead | 66.7% | 8.7 MB | 59% |

The 5, 50 and 200 ms lines are the median of three runs, whose index sizes
differ by up to 5%. With a 5 ms skew two rows in three already arrive with a key
lower than one that's in the index, and the index doesn't care: the three
streams of keys land within the same few pages. At 50 ms and beyond they land on
separate parts of the tree. Only the rightmost page of a B-tree is split to
leave it full; any other page splits in the middle and leaves two half-full
pages, and with three streams of keys, each behind the next, two of them are
never at the right edge. So the index is 28% to 55% bigger from 50 ms of skew
up, which is what the random uuid cost (27%) in the one-server table, though we
didn't count the WAL of these runs. The size isn't monotonic in the skew, and 50
ms was the worst case we ran. Uniqueness and correctness don't depend on the
clocks; the order of keys across regions does, and so does the index. This is
three streams into one server; the last section runs three servers in a mesh,
where the index starts at 68% leaf density and the skew moves it by a few
points.

## What `(region, id)` costs

The composite key looked cheap in the one-server table, and its price is paid
somewhere else: every table that points at it repeats both columns, in the
column list and in the index on the foreign key. The
[2018 article](/blog/2018/03/database-normalization-and-primary-keys/) has the
example, a `comment` table that repeats three columns of `article` to reference
it, and stops at "quite a big table". We measured how big, with a `parent` table
of 1,000,000 rows and a `child` of 5,000,000 rows that has a foreign key to it
and an index on that foreign key (`compose/run-child.sh`,
`results/95-child.out`):

| Key of `parent` | Parent: heap + indexes | Child: heap + indexes | Both | Join of all children to their parents |
|---|---|---|---|---|
| `bigint` | 77 + 22 MB | 422 + 171 MB | 692 MB | 395 ms |
| `uuid` (v4 or v7: 16 bytes) | 84 + 40 MB | 465 + 180 MB | 770 MB | 433 ms |
| `(region smallint, id bigint)`, columns in key order | 84 + 32 MB | 465 + 180 MB | 761 MB | 450 ms |
| `(region smallint, id bigint)`, `bigint` first in the table | 77 + 32 MB | 422 + 180 MB | 710 MB | 519 ms |
| `(region text, id bigint)` | 89 + 52 MB | 482 + 184 MB | 807 MB | 487 ms |

The join times are the median of three runs of a hash join on a warm cache. They
vary by up to 20% from one run to the next (433 to 526 ms on the same table),
so the only thing we read in them is that the `bigint` was the fastest.

The sizes are the clearer result. A `(smallint, bigint)` key in the order we'd
naturally write it, region first, costs what a uuid costs, 16 bytes per row in
the heap and in every index entry, because of the padding between the two
columns. Declaring the table with the `bigint` first and the `smallint` after
it, with the primary key still on `(region, id)`, keeps the heap at the size of
the single `bigint` and saves 51 MB over the pair of tables, 7%. The index
entries are 16 bytes either way. A region kept as text (`ap-southeast` is
13 bytes) is the most expensive, 115 MB more than the `bigint` for the pair.
With one child table that's between 3% (`bigint` first) and 17% (text region)
more storage than the `bigint` for the pair, and every further table that points
at the key pays again.

Is there a way to keep the composite key and pay only once? The surrogate plus
unique natural key of the 2018 article puts a single-column key in the
children, and unique `(region, id)` on the parent. Here the surrogate has to be
unique across regions without asking another region, which is what the other
four candidates are for, so it replaces the composite key instead of completing
it, and the `region` column kept next to it for the filter brings back the
companion index of the previous section. We'd take the composite key where the
number of tables that point at it is small, and a single-column key where it
isn't.

## What changed since the UUID advice of 2018

[The 2018 advice](/blog/2018/03/database-modelization-anti-patterns/) was to
reach for UUIDs when a central sequence would be a single point of failure, with
random numbers in a 128-bit space in mind and a collision as unlikely as makes
no difference. Two things moved. `gen_random_uuid()` became built in with
Postgres 13 ([the 11-18
selection](/blog/2026/07/sql-improvements-in-postgresql-1118-a-personal-selection/#built-in-gen_random_uuid-pg-13)
has the details), and `uuidv7()` with 18, so the choice is no longer between a
random key and an extension. And the price of the random one is now measured:
the index is 27% larger and each insert after a checkpoint writes 487 bytes of
WAL against 182, which is the whole difference with `uuidv7()`, whose index
behaves like a sequence's.

Two things didn't move. A UUID is still not a natural key: it identifies the row
and says nothing about what the row is, so the unique constraint on the natural
columns stays. And it's 16 bytes in every table that points at it, 11% more
storage than a `bigint` for the pair of tables of the previous section. What the
2018 text called a single point of failure has a counterpart in a multi-writer
system: a sequence per region isn't one, but it's state that a failover can
rewind, as the failover test above measured.

## What others measured

The index behaviour of random and time-ordered UUIDs has been measured before,
on one writer. [A 2024
benchmark](https://ardentperf.com/2024/02/03/uuid-benchmark-war/) inserted a
million rows with 10 clients into a 20-million-row table, on a development build
with the `uuidv7()` patch: `uuidv7()` took as long as a `bigint` (290 seconds
each), a random UUID took about 30% longer, and the `bigint` table was 20%
smaller than the `uuidv7()` one. Its explanation is the one our buffer numbers
give: a random key spreads over the whole index and crowds the other tables out
of `shared_buffers`. [credativ's look at Postgres
18](https://www.credativ.de/en/blog/postgresql-en/a-deeper-look-at-old-uuidv4-vs-new-uuidv7-in-postgresql-18/)
measured the index itself at a million rows: 40.0 MB against 31.6 MB for the
random one, which is the 27% we found, and 71% leaf density against 90%. At 50
million rows, by dividing the times in its tables, the random UUID was 12 times
slower to insert into an empty table and 28 times into a full one. That's the
scale we didn't reach, and the direction we expected.

Neither of the two measures WAL or full-page images, and neither has a second
writer, which is the half of this article that is new: the WAL after a
checkpoint, the second index that a row filter needs, what a failover does to a
region's sequence, and what skewed clocks do to `uuidv7()`.

For minting ids on several writers at once, the closest published designs are
the ones [Part
1](/blog/2026/09/ten-years-of-postgres-logical-replication/#minting-ids-on-the-workers)
lists. [pgEdge's Snowflake
sequences](https://pgedge.com/blog/introducing-snowflake-sequences-in-a-postgres-extension)
are a `bigint` of 41 bits of milliseconds since 2023, 12 bits of counter and 10
bits of node number set in `postgresql.conf`, and the algorithm moves the
timestamp into the future when a node needs more than 4,096 ids in a
millisecond. EDB's [sequences for
PGD](https://enterprisedb.com/docs/pgd/5/sequences) offer both ranges handed out
by consensus and a snowflake kind. Our time, region and counter `bigint` has the
same shape, with 4 bits of region and 18 of counter, and it's an expression in
the column default instead of an extension to install. The pgEdge page gives no
measurements.

## Before choosing: the 32-bit wall

One failure has nothing to do with regions, and every candidate above can hit
it: a key stored on 32 bits. `serial` is the familiar way to get one, and the
width of the column and the width of the sequence behind it are two separate
things. We looked at what Postgres 19 release candidate 1 builds for each way of
writing an auto-numbered column:

~~~ sql
create table t_serial    (id serial primary key);
create table t_bigserial (id bigserial primary key);
create table t_ident     (id int    generated always as identity primary key);
create table t_identbig  (id bigint generated always as identity primary key);

create sequence s_plain;
create table t_plain     (id int primary key default nextval('s_plain'));
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
column can't hold:

~~~ results
ERROR:  nextval: reached maximum value of sequence "t_serial_id_seq" (2147483647)
~~~

The dangerous one is the last kind: a sequence created on its own, which is
`bigint` unless we say otherwise, used as the default of an `int` column. The
sequence goes on, and the column is the one that stops:

~~~ results
ERROR:  integer out of range
ERROR:  integer out of range
 last_value
------------
 2147483649
~~~

Every failed insert took a value from the sequence anyway, so the sequence
is already two past the wall after two attempts, and nothing in the error says
which side to look at. Sequences are not transactional, so a rollback gives
nothing back either: an application that retries a failing insert in a loop
moves the sequence faster than it adds rows.

In a system with several writers the wall comes closer than the table size
suggests. A sequence per region with a step of 10 spends 10 values for each row
it keeps, which leaves 214,748,364 rows per region before the wall, about 25
days of a writer doing 100 inserts a second. With `(region, id)` the wall is
per region and the step is 1, but it's the same wall. Widening a column later
rewrites the table, and every foreign key that points at it has to follow,
which is the pause of Part 4, for a mistake that's cheaper to avoid on the first
day: use `bigint`, and `generated always as identity` over `serial`.

### Widening the column under logical replication

DDL isn't replicated, so a link between two servers has two columns to
widen, and the order matters. We built the smallest case that shows it: an
`int` primary key on a publisher and on a subscriber, 3,000,000 rows copied, a
publication filtered on `region = 1`, and the widening done on each side in
turn. The rewrite of 3,000,000 rows took about a second on either side (a
lab on tmpfs, so this is the CPU part of the rewrite; the table is locked
against readers and writers for that long, and with it every foreign key that
needs the same change).

With the publisher first, the link carries on until the first value that
doesn't fit in 32 bits, and then the subscriber stops on it and retries:

~~~ results
publisher-first
rows on subscriber | 3000000
apply_error_count | 2
ERROR:  value "3000000000" is out of range for type integer
CONTEXT:  processing remote data for replication origin "pg_16394" during message type "INSERT" for replication target relation "public.t" column "id" in transaction 667
~~~

Widening the subscriber afterwards unblocks it, and the row arrives. With the
subscriber first, the same row goes through with no error:

~~~ results
subscriber-first
rows on subscriber has 3000000000 | 1
apply_error_count | 0
~~~

So the order is the one of any schema change on core logical replication: widen
the receiving side first, the publishing side after. The error only shows up
when the first value past 2,147,483,647 is written, which can be days after the
`ALTER TABLE` on the publisher. In a mesh where every region both publishes and
subscribes, every region is a receiving side, so all of them have to be widened
before any of them starts minting large ids, which is [the paused rollout of Part
4](/blog/2026/11/postgres-logical-replication-beyond-one-writer/#schema-changes-the-one-gap).

### Databases that are older than Postgres 10

The `AS` clause of `CREATE SEQUENCE` arrived in Postgres 10. Before it a
sequence was always a `bigint`, including the one behind a `serial` column.
We created a `serial` column on 9.6, then dumped it with the `pg_dump` of
19rc1, which is what `pg_upgrade` runs for the schema, and restored it. On 9.6
the column is `integer` and the sequence goes up to 9,223,372,036,854,775,807,
so the failure is the one of the standalone sequence above:

~~~ results
9.6:     select setval('t_id_seq', 2147483647);  insert into t default values;
ERROR:  integer out of range
~~~

The dump carries no `AS integer` for it, and on 19rc1 the restored column is
still `integer` with a `bigint` sequence behind it, and fails the same way:

~~~ results
 column_type | sequence_type |       seqmax
-------------+---------------+---------------------
 integer     | bigint        | 9223372036854775807
~~~

A `serial` column made before Postgres 10 and carried through every upgrade
since is the dangerous combination, found in the wild rather than built by
hand. This query lists the integer columns whose owned sequence is wider than
the column:

~~~ sql
select c.oid::regclass as "table", a.attname as "column", a.atttypid::regtype as column_type,
       q.oid::regclass as sequence, s.seqtypid::regtype as sequence_type, s.seqmax
  from pg_sequence s
  join pg_class q on q.oid = s.seqrelid
  join pg_depend d on d.objid = q.oid and d.classid = 'pg_class'::regclass
                  and d.refclassid = 'pg_class'::regclass and d.deptype in ('a', 'i')
  join pg_class c on c.oid = d.refobjid
  join pg_attribute a on a.attrelid = c.oid and a.attnum = d.refobjsubid
 where a.atttypid in ('int2'::regtype, 'int4'::regtype)
   and s.seqmax > case a.atttypid when 'int2'::regtype then 32767 else 2147483647 end;
~~~

On a `t_old` table we built by hand, an `int` column that owns a `bigint`
sequence, it returns one row and ignores the `serial` and `bigserial` columns
next to it:

~~~ results
 table | column | column_type |   sequence   | sequence_type |       seqmax
-------+--------+-------------+--------------+---------------+---------------------
 t_old | id     | integer     | t_old_id_seq | bigint        | 9223372036854775807
~~~

It finds sequences that are owned by the column, as `serial` makes them. A
sequence that is only the default of the column, with no `owned by`, has to be
found through `pg_attrdef`, which is what the 2010 query of
[resetting sequences](/blog/2010/02/resetting-sequences.-all-of-them-please/)
reads, and a sequence that no column owns at all is what
[finding orphaned sequences](/blog/2010/03/finding-orphaned-sequences/) looks for.

## Using `uuidv7()` before Postgres 18

The built-in `uuidv7()` arrived with Postgres 18, and the regions of a system
don't all upgrade on the same day. Part 1 sends readers of older versions to
the application or to an extension; there's a third way, a function that is
short enough to write in SQL. It takes a random v4 uuid, overwrites its first 48
bits with the Unix time in milliseconds, and sets the version nibble to 7. The
variant bits that `gen_random_uuid()` already put there are the right ones for
[RFC 9562](https://www.rfc-editor.org/rfc/rfc9562).

~~~ sql
create function public.uuidv7() returns uuid
    language sql volatile parallel safe
as $$
    select encode(
        set_bit(
            set_bit(
                overlay(uuid_send(gen_random_uuid())
                        placing substring(int8send((extract(epoch from clock_timestamp()) * 1000)::bigint) from 3)
                        from 1 for 6),
                52, 1),
            53, 1),
        'hex')::uuid
$$;
~~~

`gen_random_uuid()` is built in from Postgres 13; before that, `pgcrypto` has
it. We ran the script on 13, 17 and 19rc1. On every one the version nibble is 7,
the variant is `8`, `9`, `a` or `b`, and the 48 bits read back as the current
time:

~~~ results
 version | rfc_variant | clock_within_1s
---------+-------------+-----------------
 7       | t           | t
~~~

Two differences with the built-in one are worth knowing. We generated a million
ids in a single statement and counted how many are smaller than the one before:

| | ids smaller than the previous one | time for 1,000,000 ids (19rc1) |
|---|---|---|
| SQL function | about 500,000 on 13, 17 and 19rc1 | 810 ms |
| built-in `uuidv7()` | 0 | 472 ms |

The SQL function fills everything after the millisecond with random bits, so
two ids made in the same millisecond come out in random order. A million ids in
under a second is a thousand or so per millisecond, which is why half of them
are out of order. Across milliseconds the order holds, and that's what the
index cares about: new keys land in the pages of the last few milliseconds,
not anywhere in the tree. The built-in function spends 12 bits on the
sub-millisecond fraction and stays ordered inside a session; we'd only notice
the difference if something sorts on the key to recover the insert order of
one session.

The function lives in `public`, and an unqualified `uuidv7()` finds
`pg_catalog` first, so on 19rc1, with both defined, a call written without a
schema runs the built-in one:

~~~ results
 Result
   Output: uuidv7()
~~~

That covers the queries and the code that call `uuidv7()`. It doesn't cover a
column default, because `pg_dump` writes it with the schema. We created
`default uuidv7()` on 17, with the SQL function, and dumped it with the
`pg_dump` of 19rc1, which is what `pg_upgrade` does for the schema:

~~~ results
    id uuid DEFAULT public.uuidv7() NOT NULL,
~~~

After the restore the default still calls the SQL function. Once every region
runs 18 or later, we set the defaults again, without a schema (`alter table ...
alter column id set default uuidv7()`), which resolves to the built-in one,
and then drop `public.uuidv7()`.

Last, a `uuidv7()` value carries the time of its creation to the millisecond,
as Part 1 notes. When the key appears in a URL or an API, that's a public
timestamp for the row, which a random v4 doesn't give away.

<!-- Sources: sql/20-uuidv7-sql.sql, sql/21-uuidv7-native.sql, results/20-*.out,
     results/45-uuidv7-default.out (compose/run-default.sh).
     The function is adapted from the one in TAOP's download-tracking
     migration (internal/db/migrations/0001_init.sql). NB: the TAOP course
     text 2-1-uuid-gen.sql and the generated page say uuidv7() came with
     Postgres 17; it came with 18. -->

## Which one to pick

| Situation | Key | Why, and what it costs |
|---|---|---|
| One writer region, others only read | `bigint generated always as identity` | Nothing to coordinate; the 32-bit wall is the one trap, and the query above finds the columns that are on it |
| Several writer regions, each row owned by one, few tables point at the key | `(region, id)`, one identity column per region, `bigint` first in the table | The filter of Part 4 works without a second index, no clock is involved, no step to outgrow. It costs 3% to 10% more storage in a pair of tables, and each region's sequence has to survive a failover, which is [Part 4's gating](/blog/2026/11/postgres-logical-replication-beyond-one-writer/#a-region-is-a-group-of-three) |
| Several writer regions, each row owned by one, many tables point at the key | a single-column key: the time, region and counter `bigint`, filtered on its bits (`where ((id >> 18) & 15) = 1`, no second index), or `uuidv7()` with a `region` column and a unique `(id, region)` as replica identity | 8 or 16 bytes in every child instead of 10 or 16. The `bigint` needs nothing else, and didn't repeat after a failover in six runs, but it repeats when a clock is stepped back by 2 seconds (0.26% of the transactions of the test). It's an expression to write and keep, with 16 regions at most. `uuidv7()` is a built-in function, and its companion index adds about 85 bytes of WAL per row |
| Rows created anywhere, nobody filters on the owner | `uuidv7()` | 16 bytes, an index that behaves like a sequence's, nothing to coordinate. The clocks need to agree within a few milliseconds |
| The creation time must not leak | `gen_random_uuid()` | An index 27% larger, 2.7 times the WAL after a checkpoint (487 bytes a row against 182), and 5 times with a companion index (954) |

If we had to keep one rule: `uuidv7()` where nothing filters on the owner and
the clocks are kept in sync, `(region, id)` where the owner is the point, a
random uuid only when the creation time must stay hidden, and never 32 bits.

For a table that already has a sequence, the first steps come before the key:
the query above for its width, and Part 1's step and headroom arithmetic if
it's to become one of several writers. We haven't measured that migration, and
we'd keep the old tables single-writer and use the new key for new ones.

## Would Postgres need a sequence access method?

Two patch sets, in 2013 to 2016 and again from 2023 to 2026, tried to let `create
sequence ... using method` choose how a sequence computes its next value, so that
a `serial` or an identity column could be fed by a Snowflake-style id instead of
a counter. Both were returned with feedback; the reviews of the second asked for
the use case before the code, and in May 2026 its author dropped it from the
CommitFest (`compose/seqam/README.md` has the threads). Suppose that Postgres 20
ships the facility. A method is then an extension like any other, loaded with
`shared_preload_libraries`, and "a patched server" is not a cost of the answer.
The question left is whether it earns its place for a multi-region key, and the
cases that decide it are the ones a production deployment meets: a clock that
steps back, a failover to a server whose clock is behind, and regions whose
clocks differ.

We tested that with the v29 patches built into a Postgres master
(`compose/seqam/Dockerfile`) and a method of our own, `regionseq`
(`extension/`, a PGXS extension with its regression test). Its id is 41 bits of
milliseconds, 6 of region from a server setting that has no default, 4 of
timeline and 12 of counter. It keeps the last millisecond and counter it made
and never goes below them, takes the next millisecond when the counter runs out
instead of waiting, and logs a high-water mark one second ahead to the WAL, so
that ids stay above what was made after a crash and on a standby. The table, the
index and the WAL are those of the expression of the fifth key (1.48 M rows/s
for both, 112 MB of index and 165 bytes of WAL against 168, one region
writing), since a method makes the same `bigint`.

The timeline bits are there for the failover below. Postgres opens a new
timeline at every promotion and every restore to a point in time, and a
promoted standby with a stale copy of the state and a clock behind makes ids
that differ from the old primary's, because that number does. Four bits are 16
values, which we chose from what production reports: a Percona forum thread
reads TL 11 as ten switchovers, a [Patroni
issue](https://github.com/patroni/patroni/issues/1733) of a test cluster that is
killed repeatedly is on timelines 40 to 81, and one pgsql-general reporter had
[5,787 history files](https://postgresql.org/message-id/1521007012.2368.3.camel%40cybertec.at)
because every restart opened a timeline. What counts isn't the lifetime total
but how many promotions fall inside the time a clock or a standby was wrong,
seconds to minutes, and a Patroni cluster that flaps promotes once per leader
lock expiry. A coarser clock wouldn't buy bits: time and counter share a budget
of `log2(lifetime) + log2(ids per tick)`, 41 + 12 bits with milliseconds and
31 + 22 with seconds, so a tick of a second only moves bits from the time to the
counter, and coarsens the order between regions. The bits are bought by the peak
rate we plan for, and 4,096 ids a millisecond is what one sequence does under
its lock. Two standbys promoted independently from the same primary pick the
same timeline number, a case that pgsql-hackers is discussing in 2026 (with a
UUID per history entry as one answer), and that fencing has to prevent.

### What "fine" means

Three properties, from the weakest. **Unique:** an id is never made twice.
**Increasing per region:** what one region makes only goes up, which is what a
consumer that remembers "the last id I saw from this origin" needs, and with the
region in the key it can ask for one origin at a time. **Time-like:** the time in
the id is the creation time. Across regions, the last is the only one a clock
can give, and not exactly: replication lag already puts a row from another
region below ids that a reader has seen, so no key lets a reader keep one
cursor over all regions, and the second property is the one to ask for.

### A clock that steps back on one instance

Four clients insert 100 ids per transaction at 200 transactions a second, the
clock of the server steps back after 4 seconds (libfaketime), and we count the
ids that come out lower than one already made, by the same client or by any
client, and how far the time in the id is ahead of the server's clock
(`compose/run-clock-order.sh`, `results/121-clock-order.out`, about 200,000 ids
each, one run):

| Key | Lower than an earlier id of the same client, clock back 2 s / 10 s | Lower than an earlier id of any client | Time in the id ahead of the clock |
|---|---|---|---|
| One sequence | 0 / 0 | 0.1% to 0.2%, the jitter of four writers | none |
| `gen_random_uuid()` | all | all | none |
| `uuidv7()` | 0 / 0 | 16% / 47% | up to 2 s / 10 s |
| Expression, time and counter | 18% / 59% | 18% / 59% | none |
| `regionseq` | 0 / 0 | 0.1% to 0.2%, as a sequence | up to 2 s / 10 s |

The expression follows the clock down: its ids go lower for as long as the
clock is behind, and it can repeat one. At full speed, four writers, three runs
(`results/104-*`), 183 to 197 of about 71,000 transactions failed on a duplicate
key when the clock stepped back 2 s, none at 50 or 200 ms; `uuidv7()` and
`regionseq` had none. `uuidv7()` doesn't repeat, and keeps each
session increasing, because Postgres keeps the last timestamp of a backend and
adds to it; the sessions do it separately, so the region's ids interleave. A
`regionseq` sequence keeps the whole region increasing, because the state is one
place. Both of them pay the same price: the ids run ahead of the clock until
it catches up (still 4 s ahead at the end of the 10 s run), so their time stops
being the creation time. A sequence per region, the first key, has none of this
and no time either.

### A failover to a server whose clock is behind

The standby is promoted after the primary is killed, with its clock 0, 0.1, 2 or
10 seconds behind. The client took ids from the old primary as fast as it could
for 3 seconds; the new primary then makes 200,000. We count the new ids that
repeat one the old primary gave, and the new ids below the highest one it gave
(`compose/run-failover-skew.sh`, `results/120-failover-skew.out`, one run each):

| Standby, clock behind by | Expression: repeated / below | `uuidv7()`: repeated / below | `regionseq`: repeated / below |
|---|---|---|---|
| In sync, 0 or 0.1 s | 0 / 0 | 0 / 0 | 0 / 0 |
| In sync, 2 s | 98 / all | 0 / all | 0 / 0 |
| In sync, 10 s | 0 / all | 0 / all | 0 / 0 |
| Cut off 3 s before, 2 s | 782 / all | 0 / all | 0 / all |
| Cut off 3 s before, 10 s | 0 / all | 0 / all | 0 / all |

Up to 0.1 s every key is fine, since the promotion itself takes longer than
that. Past it, a key that is made of the clock goes below what the old primary
made, and the expression repeats ids when the new clock falls into the
milliseconds that the old primary used and the counters line up. (The 10 s lines
show no repeat only because the old primary had written for 3 seconds; with
writes all along, any skew larger than the promotion repeats whatever was made
in that window.) `regionseq` never repeats, and stays above when the standby is
current, because its WAL record holds a high-water mark. On a standby that was
cut off the mark is old, and with a clock behind the ids go below what the old
primary made. Our first version, without the timeline bits, repeated 48,510 ids
in that line, 24% of the new ones, which is why the bits are there: the ids of
the new timeline differ, so uniqueness no longer depends on the standby having
the WAL, and only the increasing order does. A plain sequence per region has the
same dependency, for the same reason, and no clock.

Those are physical standbys promoted by hand, which is how we made one that is
stale. A production region is managed, and its standby is not stale, so we ran
that too.

**With pg_auto_failover.** A region is a monitor and two nodes, the second a
synchronous standby (`synchronous_standby_names = ANY 1`), in the topology of
Part 4. A subscriber in another region follows it with a failover slot
(`failover = true`), and pg_auto_failover manages `synchronized_standby_slots`,
`sync_replication_slots` and the `dbname` of `primary_conninfo` itself (we built
its main branch against the server of this section: it needed three build fixes
for gcc 14 and the server's warning flags, none in its logic). A writer inserts
1,000 ids per statement through a two-host URI, the primary is killed after 6
seconds, and pg_auto_failover promotes node 2, whose clock is 0, 2 or 60 seconds
behind (`compose/run-pgaf-drill.sh`, `results/130-pgaf-drill.out`, one run each,
5 to 7 million ids a run):

| Node 2 clock behind by | Expression: refused batches / ids below | `uuidv7()` | `regionseq` |
|---|---|---|---|
| 0 s | 0 / 0 | 0 / 0 | 0 / 0 |
| 2 s | 0 / 0 | 0 / 0 | 0 / 0 |
| 60 s | 1 / 3,223,000 (61%) | 0 / 3,229,318 (61%) | 0 / 0 |

In all nine runs writes were back after 34 seconds, no acknowledged row was
missing on the new primary (0 of 6 to 7 million), and the subscriber ended with
exactly the rows of the new primary: the failover slot survived the promotion,
and the subscription reconnected by itself. The skew table has two readings.
First, 34 seconds of outage is longer than the 0 and 2 s of skew, so the new
primary's clock was already past the old one's last id when it started, and
nothing repeated or went below: a failover that takes tens of seconds hides a
small skew. Second, at 60 s the clock keys go wrong, as the manual tests said:
the expression had a batch refused on a repeated id, which the writer retried,
and both clock keys made 61% of their ids below ones already issued. `regionseq`
had neither, because the standby is current and its WAL holds the high-water
mark.

One thing about the tool, which we saw and didn't pursue. With Postgres's default
`checkpoint_timeout` the failover took 274 seconds in place of 34. The keeper
confirms a promotion by checking that the timeline advanced, and we infer from
the timing that it reads it from the checkpoint that follows the promotion,
which is spread over `checkpoint_completion_target` times `checkpoint_timeout`,
270 seconds by default: the events show the standby reported `stop_replication`
271 seconds after `prepare_promotion`. The
runs above set `checkpoint_completion_target = 0`. It was the main branch of
pg_auto_failover (7701bc9) against a Postgres 19 master, so it may not be what
a released pair does.

### Regions whose clocks differ

Three regions, region 0 on time, region 1 ahead and region 2 behind by 0, 50 ms
or 2 s, each writing 200 transactions of 100 rows a second and subscribed to
the other two (`compose/run-mesh-skew.sh`, `results/122-mesh-skew.out`, one run
each):

| Clocks | Expression: primary key, leaf density | `uuidv7()` | `regionseq` |
|---|---|---|---|
| 0, 0 | 17.7 MB, 68% | 25.0 MB, 68% | 16.7 MB, 72% |
| +50 ms, -50 ms | 18.2 MB, 67% | 26.9 MB, 64% | 18.4 MB, 67% |
| +2 s, -2 s | 18.2 MB, 66% | 26.7 MB, 63% | 17.8 MB, 67% |

All nine runs converged with equal digests and no apply error. Skew costs the
index 1 to 5 points of leaf density, against the 28% to 55% growth that the
three streams of the skew table above showed for `uuidv7()` inserted by one
server: in a mesh the replicated rows arrive in batches from apply workers and
the index is at 68% before any skew, so the skew has less to add. The
implications are about meaning, not correctness:

- **Uniqueness doesn't depend on the clocks,** since the region is in the bits
  (or the bits are random).
- **Order across regions is the order of the clocks,** so "created before" can't
  be read from two ids of two regions closer than the skew. It's a total order
  that every node agrees on, which is all `order by id` and a keyset
  pagination need; it isn't a causal order.
- **Per region, ids still increase** with `regionseq` and with a sequence per
  region, so a consumer per origin is correct, whatever the other regions' clocks
  say. That's the guarantee to design on, and `((id >> 16) & 63) = r` gives the
  origin from the key.

### Verdict: nice, not needed

On what a production deployment has to survive:

- **The failover problem is the same for every key that has state, except for
  uniqueness.** A sequence per region needs the standby to be current to stay
  unique; `regionseq` stays unique whatever the standby has, thanks to the
  timeline, and needs the standby only to keep increasing; the clock keys need
  clocks that agree within the promotion time. Part 4's gating gives the
  standby, and a method doesn't replace it.
- **A clock that steps back is a rare event that the expression turns into a
  burst of duplicate-key errors and into ids that are lower than earlier ones.**
  A method that keeps state turns it into ids that run ahead of the clock for a
  while, refuses a clock a minute behind, and keeps the region increasing. The
  per-region sequence does the same without a clock, at the price of a
  two-column key or a step. For a consumer that reads one origin as a stream,
  that guarantee is what a method adds, and the expression can't give it.
- **Everything else a method adds is a guard rail:** a region number that can't
  be left unset, a maximum that an `int` column must meet (`reached maximum
  value`, where the patches' demonstration silently kept 32 bits of the id), and
  the single-column `bigint` with region bits that the filter of Part 4 can read
  without a second index.

So, with the facility in Postgres 20 and the method an extension: nice, because
it gives a single-column, 8-byte key with a region in it and increasing per
region through a clock that steps back, which nothing built in does. Not needed,
because the choice that carries the load in production is the one this article
makes with `uuidv7()`, `(region, id)` or the expression, together with gated
standbys and clocks that are slewed and watched, and a method fails the same way
the others do when the second one is missing. What would make it needed is a
reader of ids as a stream in a system that can't ask the clocks to behave; and
what would change this section is a use that has state an expression can't hold,
ranges handed out by consensus or gapless numbers, which we didn't test.

## What we didn't test

- **Scale and disk.** The tables hold 5,000,000 rows on tmpfs, so the 198,663
  index pages that a random uuid missed in `shared_buffers` came from the
  operating system's cache, and we'd expect the gap to widen on storage that
  isn't memory.
- **Updates and deletes.** Only inserts were measured. A companion index set
  as the replica identity also changes what an `UPDATE` writes to the WAL, and
  we didn't count it.
- **Real clock drift.** Skew in the first test was simulated by shifting the
  timestamps with `uuidv7(interval)`, and the 5, 50 and 200 ms cases were run
  three times. Everywhere else the clocks are libfaketime offsets on one
  machine; the failover with a standby clock behind and the mesh with skewed
  regions are one run per cell, and in the first the old primary had written for
  3 seconds only.
- **Other uses of a sequence access method:** ranges handed out by consensus,
  gapless numbers, pseudo-random sequences.
- **A larger pg_auto_failover formation.** The drill is one region of two nodes
  and one subscriber, one run per cell, with the clock skew on the promoted node
  from libfaketime. Three nodes, priorities, a planned switchover, and a
  partition instead of a kill are not run.
- **More than three regions in the mesh.** The mesh is three plain servers,
  without a standby.
- **Keys made by the application,** with an ORM or a library: they bring their
  own `uuid` version and their own clock.
- **Postgres 19 after RC1.** Everything here ran on 19rc1, RC2 is planned for
  2026-11-12 and the release for 2026-11-19.

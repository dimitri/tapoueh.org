+++
title     = "Distributed primary keys"
date      = "2026-11-09T18:00:00+0100"
tags      = ["PostgreSQL", "Replication", "Data Modeling", "Architecture"]
categories = ["PostgreSQL", "Architecture"]
icon      = "🔑"
+++

A single Postgres server hands out primary keys without effort: a `bigint`
identity column asks one sequence for the next number, and no two rows ever get
the same one. Now put writers in several regions, each of them accepting inserts
while the links between them are slow or down. Two regions that each ask
their own sequence for "the next number" will hand out the same one, and the
first time those two rows meet on a subscriber, one of them is rejected.

So which primary key should we give a table that several regions write at the
same time? That's the question of this article. It continues the logical
replication series: [Part
1](/blog/2026/09/ten-years-of-postgres-logical-replication/#minting-ids-on-the-workers)
listed the ways to make ids that can't collide, and [Part
4](/blog/2026/11/postgres-logical-replication-beyond-one-writer/) built the
system that uses them, with a group of nodes in each region and every region
subscribed to the others. Here we measure what each choice costs on the same
workload: in size, in WAL, in joins, during failovers, and when clocks disagree.

We take the basics for granted. A primary key is a unique and non-null
constraint. A generated id is a surrogate key, and it doesn't replace the
natural key, because it can't stop two rows that describe the same thing.
[Database Normalization and Primary
Keys](/blog/2018/03/database-normalization-and-primary-keys/) covers both, and
this article is about where the generated id comes from.

**TL;DR** We compared five primary keys for a table that several regions write
at once. A sequence per region is the trap: when a region's standby is promoted,
the sequence can hand out numbers that were already used. `uuidv7()` is cheap and
needs no coordination. A composite `(region, id)` key lets a publication filter
on the region without a second index. A `bigint` made of a timestamp, the region
and a counter does the same with a single column, as long as the clocks agree.
And whatever the key, 32 bits is a wall.

<!--more-->

{{< lab >}}
All the measurements come from throwaway containers on Postgres 19 release
candidate 1, on one laptop, with four writers unless stated otherwise. The sizes
and WAL volumes are exact; the rates are only good for comparing the keys with
each other. The scripts and the output of every run are in the [lab
directory](#going-further) of this article.
{{< /lab >}}

<!--toc-->

## The short version

- **On one server, the random uuid is the expensive key.** In the minutes after a
  checkpoint, `gen_random_uuid()` writes 487 bytes of WAL per row against 182 for
  `uuidv7()`, and its index is 27% larger. Both use the same native 16-byte
  `uuid` type: the difference comes from where the new keys land in the index, as
  [the single-server section](#what-each-one-costs-on-one-server) shows. A
  `uuidv7()` index behaves like a sequence's.
- **A composite `(region, id)` key makes the child tables bigger, and a
  single-column key needs a second index to be filtered by region.** Every table
  that points at a composite key repeats 10 to 16 bytes per row. A uuid key needs
  a second index before a publication can filter it by region, and that index
  adds about 85 bytes of WAL per row. A time-based `bigint` holds the region in
  its bits, so the filter reads the key alone.
- **When a region's standby is promoted, a sequence can hand out numbers that
  were already used.** A sequence only knows what its physical standby had
  received. We promoted a standby that was a few seconds behind, and the new
  primary handed out 300,000 numbers a second time. A key built from the clock
  doesn't have this problem, because the clock moved on during the failover. It
  does when the new primary's clock is behind the old one's: its ids are then
  lower than the old ones, and with the time-based `bigint` a few of them can
  repeat ids already given out.
- **A clock that steps back has the same effect** on the time-based `bigint`:
  some ids repeat. `uuidv7()` ids go out of order across sessions, and never
  repeat.
- **A sequence access method could keep a region's ids increasing through a
  failover and through a clock that steps back,** at the cost of state to
  maintain. No release has the facility. We built one on top of a patch set to see
  what it would change, and [the section](#what-a-sequence-access-method-could-change)
  lays out the trade-offs.
- **Whatever the key, 32 bits is a wall.**

## What a key has to do here

Before measuring anything, here's what we expect from a primary key when several
regions write:

- **Be unique everywhere**, without asking another region first. That's the one
  thing a single sequence can't do once there are several writers.
- **Say which region owns the row.** Each region publishes only its own rows,
  with a row filter on the owner. For updates and deletes, a row filter can only
  use columns of the *replica identity*: the columns that logical replication
  sends to find the row on the subscriber, the primary key by default. A
  composite key includes the region and a single-column key doesn't, so we count
  the extra index that the single-column key needs.
- **Stay cheap to index and to join on**, since every child table repeats it.
- **Survive a failover**: a promotion, planned or not, and a clock that isn't
  exactly right.

## Five candidates

Every table in this article has the same three columns: the key, `region
smallint`, and a 32-byte `payload text`. Region numbers go from 0 to 3, one per
writer. Here's the DDL of the five keys:

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
| Sequence per region | each region has its own numbers | 8 bytes |
| Composite `(region, id)` | the region is in the key | 2 + 8 bytes, 16 once aligned in a row or an index entry |
| `gen_random_uuid()` (v4) | the space is too large to collide | 16 bytes |
| `uuidv7()` | the same, with a time prefix | 16 bytes |
| Time-based `bigint` | the region is in the bits | 8 bytes |

The sequence per region gives each region its own numbers: with an increment of
10, region 0 makes 1, 11, 21 and so on, and region 1 makes 2, 12, 22. The
composite key puts the region next to a plain sequence. The two uuids count on a
space so large that two random values don't collide, and `uuidv7()` puts the time
in front of the random part.

The last one, the time-based `bigint`, is the Snowflake layout, written as an
expression in the column default. The time comes first, so that ids sort by
creation. The region comes next. Last comes a counter, which only has to differ
between the ids that one region makes within the same millisecond.

Each region sets its number once, for its database, with `alter database
postgres set app.region = 1`. A custom setting needs a dotted name, which is why
it's `app.region` and not just `region`. The origin is spelled in UTC: a bare
`'2026-01-01'` would move by hours with the `TimeZone` of the session that
inserts.

{{< image src="fig-bits.svg" title="The 64 bits of the time-based bigint, and the expressions that read them." >}}

In [2018](/blog/2018/03/database-modelization-anti-patterns/), the advice was to
reach for uuids when a central sequence would be a single point of failure, and
random uuids were the kind we had in mind. Since then, `gen_random_uuid()` has
become built in with Postgres 13, and `uuidv7()` with Postgres 18 ([the 11-18
selection](/blog/2026/07/sql-improvements-in-postgresql-1118-a-personal-selection/#uuidv7--time-sortable-uuids-pg-18)
has the details). The choice is no longer between a random key and an extension,
and we can measure what the random one costs.

## What each one costs on one server

We start with a single server, so that the numbers of the following sections
have something to be compared with.

Four writers, one per region number, insert 100 rows per transaction until the
table holds 5,000,000 rows, with the default 128 MB of `shared_buffers`. Then we
run a checkpoint and insert 500,000 more rows. That second run matters because
the first change to a page after a checkpoint writes the whole 8 kB page to the
WAL, a *full-page image*. A server is in that situation for some minutes after
every checkpoint, and that's where the cost of a random key shows. Each figure
is the median of three runs:

| Key | Table | Pkey | WAL/row | After checkpoint | FPIs | Index misses |
|---|---|---|---|---|---|---|
| Sequence per region | 383 MB | 147 MB | 176 bytes | 176 bytes | 8 | 4 |
| `(region, id)` | 422 MB | 158 MB | 183 bytes | 183 bytes | 20 | 16 |
| `gen_random_uuid()` | 422 MB | 200 MB | 197 bytes | **487 bytes** | **24,396** | **198,663** |
| `uuidv7()` | 422 MB | 158 MB | 182 bytes | 182 bytes | 10 | 4 |
| Time-based `bigint` | 383 MB | 151 MB | 175 bytes | 174 bytes | 9 | 4 |

Pkey is the primary key index. The last three columns are for the 500,000 rows
inserted after the checkpoint: WAL per row, full-page images (FPIs), and index
pages missing from `shared_buffers` (index misses).

{{< image src="fig-wal.svg" title="WAL per row in the 500,000 inserts after a checkpoint." >}}

Here's how to read the `gen_random_uuid()` row. Its index has about 24,000
pages, and the 500,000 inserts after the checkpoint wrote 24,396 full-page
images: nearly every page of the index was modified, and written whole to the WAL
once. That's 487 bytes of WAL for each row, when the insert itself needs about 183
bytes, as the other keys show. The 198,663 index misses tell the same story from the buffer side. The
index is larger than `shared_buffers`, so four inserts in ten had to fetch their
page from outside of it.

The other four keys add each new row to the rightmost page of the index, or near
it. They wrote 8 to 20 full-page images and missed 4 to 16 pages in the same
run. From the point of view of the index, `uuidv7()` behaves like a sequence.

{{< image src="fig-btree.svg" title="Where the new rows land in the index, and how full the pages stay: random pages cost a full-page image each, the last page costs one." >}}

The size columns hold two more results.

The random uuid index is 27% larger than the `uuidv7()` one, for the same
16-byte keys. When a new key goes into a full page in the middle of the index, the
page splits into two half-full pages, and random inserts leave them partly empty. Appending at the right
end leaves the pages behind it full.

The `(region, id)` table is 10% larger than the tables with a single `bigint`
key. A `bigint` has to start on a multiple of 8 bytes, so a `smallint` followed
by a `bigint` takes 16 bytes, and the whole row is then padded to the next
multiple of 8. In the heap, a composite key of this shape costs what a uuid
costs.

Others measured the same index behaviour with one writer. [A 2024
benchmark](https://ardentperf.com/2024/02/03/uuid-benchmark-war/) found
`uuidv7()` as fast as a `bigint`, and a random uuid about 30% slower. The
[credativ look at Postgres
18](https://www.credativ.de/en/blog/postgresql-en/a-deeper-look-at-old-uuidv4-vs-new-uuidv7-in-postgresql-18/)
measured the same 27% on the index, with index pages 71% full against 90%, and
inserts 12 to 28 times slower for the random key at 50 million rows, a scale we
didn't reach. Neither of them measures WAL or full-page images, and neither has
a second writer.

### What filtering by region adds

Each region publishes its own rows with a row filter on the region, as in
`create publication region_1 for table t where (region = 1)`. For an `INSERT`,
that's all it takes. For an `UPDATE` or a `DELETE`, Postgres requires the
columns of the filter to be part of the replica identity, so a table keyed on
`id` alone refuses the first `UPDATE`:

~~~ results
ERROR:  cannot update table "t"
DETAIL:  Column used in the publication WHERE expression is not part of the replica identity.
~~~

The fix is a second unique index that holds both columns, set as the replica
identity:

~~~ sql
create unique index t_id_region on t (id, region);

alter table t replica identity using index t_id_region;
~~~

With it, the `UPDATE` goes through. The `(region, id)` key doesn't need any of
this, since its primary key already holds the region. Here's the single-server
benchmark again, with the second index added to the other four keys:

| Key | 2nd index | Rows/s | WAL/row | After<br />checkpoint | FPIs |
|---|---|---|---|---|---|
| Sequence per region | 204 MB | 1.36 M (was 1.91) | 263 bytes (was 176) | 263 bytes | 11 |
| `gen_random_uuid()` | 270 MB | **0.51 M** (was 1.17) | 301 bytes (was 197) | **954 bytes** | **57,657** |
| `uuidv7()` | 203 MB | 1.01 M (was 1.35) | 266 bytes (was 182) | 266 bytes | 11 |
| Time-based `bigint` | 211 MB | 1.10 M (was 1.51) | 260 bytes (was 175) | 259 bytes | 11 |

The second index is larger than the primary key it goes with, since it holds the
key and the region, and it adds about 85 bytes of WAL to every row. With a random
uuid, both indexes receive their writes at random places. After a checkpoint,
each row then costs 954 bytes of WAL, five times what a row costs with a sequence
and no second index.

The time-based `bigint` doesn't need a second index. Its region is in its bits,
and a row filter may use the built-in operators that read them, so the filter
reads the key and nothing else. Here's the publisher of region 1, with the
time-based `bigint` table of the five candidates, its primary key as the replica identity (the default),
and no other index:

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

Then we count the rows by `payload` on the publisher, the server where the rows
were made, and on the subscriber, the server that receives the publication:

| `payload` | rows on the publisher | rows on the subscriber |
|---|---|---|
| `mine` | 850 | 850 |
| `mine, updated` | 100 | 100 |
| `theirs` | 850 | 0 |
| `theirs, updated` | 100 | 0 |

The subscriber has the rows of region 1, with their 100 updates and 50 deletes
applied, and no row of region 2: the filter kept them from being sent at all.
The subscription reported no error.

A `uuidv7()` has no such bits, so a uuid key needs the second index.

{{< image src="fig-replica-identity.svg" title="What a row filter on the region can read, for updates and deletes, with each key." >}}

## What the composite key costs in the child tables

The composite key looked cheap on one server. Its overhead shows up in the other
tables: a table that points at the key repeats both columns, in its own rows and
in the index on the foreign key. The [2018
article](/blog/2018/03/database-normalization-and-primary-keys/) shows it with an
`article` table keyed on three columns. Its `comment` table repeats all three,
and the article stops at "quite a big table".

Here we measure how big. A `parent` table holds 1,000,000 rows, and a `child`
table holds 5,000,000 rows, with a foreign key to the parent and an index on
that foreign key:

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
| `bigint` | 77 + 22 MB | 422 + 171 MB | 692 MB | 395 ms |
| `uuid`<br />(v4 or v7, 16 bytes) | 84 + 40 MB | 465 + 180 MB | 770 MB | 433 ms |
| `(region smallint, id bigint)`<br />columns in key order | 84 + 32 MB | 465 + 180 MB | 761 MB | 450 ms |
| `(region smallint, id bigint)`<br />`bigint` first in the table | 77 + 32 MB | 422 + 180 MB | 710 MB | 519 ms |
| `(region text, id bigint)` | 89 + 52 MB | 482 + 184 MB | 807 MB | 487 ms |

Parent and Child are the heap plus the indexes of each table, and Join is the
time to join all the children to their parents. The join times vary by up to
20% from one run to the next, so the only thing we read in them is that the
`bigint` was the fastest. The sizes are the clearer result:

- A `(smallint, bigint)` key in the order we'd naturally write it, region first,
  costs what a uuid costs: 16 bytes per row in the heap and in every index
  entry, because of the padding between the two columns.
- Declaring the `bigint` column before the `smallint` one in the table, with the
  primary key still on `(region, id)`, keeps the heap at the size of the single
  `bigint` and saves 7% over the pair of tables.
- A region kept as text (`ap-southeast` is 13 bytes) is the most expensive: 17%
  more than the `bigint` for the pair, and every further table that points at
  the key repeats it.

{{< image src="fig-padding.svg" title="The same two columns in a row: region first pads the bigint to the next multiple of 8 bytes, bigint first doesn't." >}}

So we'd take the composite key where few tables point at it, and a single-column
key where many do.

## Three regions that write to each other

Between regions, the only link is logical replication. Each region publishes its
own rows, and every other region subscribes to them. A subscriber receives rows,
never the state of a sequence. So here a key has two jobs: arrive without
colliding with the keys of the other regions, and keep the subscriber's index in
good shape.

### Writers in three regions at once

We run three servers on one network, one per region. Each server subscribes to
the other two with `origin = none`, so that a row it receives isn't sent back
where it came from. Every region runs 200 transactions of 100 rows a second for
10 seconds. Then we wait for the three links to catch up, and compare the row
count and a digest of the keys on each server.

{{< image src="fig-mesh.svg" title="Three regions that write to each other: every row crosses each link once, and origin = none stops it from going round." >}}

| Key | Written | Per server | Digests | Apply errors |
|---|---|---|---|---|
| Sequence per region | 612,500 | 612,500 | equal | 0 |
| `(region, id)` | 600,300 | 600,300 | equal | 0 |
| `gen_random_uuid()` | 608,100 | 608,100 | equal | 0 |
| `uuidv7()` | 590,300 | 590,300 | equal | 0 |
| Time-based `bigint` | 597,000 | 597,000 | equal | 0 |
| Control: every region's sequence starts at 1 | 203,300 | 203,300 | equal | 0 |

Every key does its job: no collision, no stopped subscription, and the same rows
on every server. The control row shows that the test can fail. When every
region's sequence starts at 1, the rows of one region reached the other two
before their own first batch, and their writers stopped at the first insert, on
a duplicate key. That's why only 203,300 rows were written.

### Clocks that disagree between regions

A `uuidv7()` is only as ordered as the clocks that made it. To see what that
does to an index, we shift the clocks of three regions with the
`uuidv7(interval)` form of the built-in function, which adds the interval to the
time it puts in the id, as a skewed clock would. The three regions then take
turns inserting into one server, 180,000 rows at about 3,000 a second. Region
shifts are the offsets of the three clocks. Out of order counts the rows whose
key is below one already in the index, and leaf density is how full the index
pages are:

| Region shifts | Out of order | Index size | Leaf density |
|---|---|---|---|
| 0, 0, 0 | 0% | 5.7 MB | 90% |
| 0, +5 ms, -5 ms | 66.7% | 5.5 MB | 93% |
| 0, +50 ms, -50 ms | 66.7% | 8.8 MB | 58% |
| 0, +2 s, -2 s | 66.7% | 7.6 MB | 68% |
| one region 1 hour ahead | 66.7% | 8.7 MB | 59% |

With a skew of only 5 ms, two rows in three already arrive with a key lower than
one in the index, and the index doesn't care: the three streams of keys land in
the same few pages. From 50 ms on, the three streams land in separate parts of
the tree, and that costs space. Postgres splits the rightmost page of a B-tree so
as to leave it full, but it splits any other page in the middle, which leaves
two half-full pages. With three streams of keys, each one behind the next, two
of them are never at the right edge. So from 50 ms of skew up, the index is 28%
to 55% bigger: as much as a random uuid costs on one server (27%), or more.

{{< image src="fig-skew-streams.svg" title="Three regions whose clocks disagree, inserting into one index: from 50 ms of skew, two streams of keys split pages in the middle." >}}

That was three streams into one server. In the three-server mesh, a subscriber's
index starts at 68% leaf density, because the rows arrive in batches from the
apply workers, and a skew of 50 ms or 2 s moved it by only 1 to 5 points, for
`uuidv7()` and for the time-based `bigint`.

Uniqueness doesn't depend on the clocks: the region is in the bits of the
`bigint`, and the rest of a uuid is random. The order of the keys across regions
does depend on the clocks, and so does the size of the index, a little.

## When a region's standby is promoted

Inside a region, high availability comes from a physical standby: a streaming
copy of the primary, which is promoted when the primary goes away. In this
section, "standby" always means that physical standby, and "subscriber" means
another region, which receives rows through logical replication.

{{< image src="fig-region.svg" title="A region: the standby gets all the WAL, the state of the sequence included; the other regions get rows only." >}}

This is where the state of a sequence matters. A standby's copy of a sequence can
be older than the rows that the subscribers have already received. If the
promoted standby hands out those numbers again, the subscribers stop on a
duplicate key.

### A stale standby

A sequence is state, and a standby's copy of it is only as recent as the last
WAL the standby received. To see what that does, we take a primary, a standby,
and a client that takes numbers from a sequence as fast as it can. After three
seconds we kill the primary and promote the standby by hand, which lets us choose
how stale it is: either streaming up to the kill, or cut off from the primary
three seconds before. Then we compare the first number the new primary hands out
with the last number the client received. Three runs of each:

| | Streaming standby | Standby cut off three seconds before the kill |
|---|---|---|
| Numbers skipped, which is harmless | 13, 16, 3 | 0, 0, 0 |
| Numbers handed out twice, which are duplicates | 0, 0, 0 | 302,000, 297,000, 299,000 |

{{< image src="fig-stale.svg" title="A streaming standby carries on after the last number; a standby cut off before the kill hands out numbers the old primary already used." >}}

A sequence writes to the WAL 32 numbers ahead of the one it hands out. So a
standby that was streaming up to the kill starts a little ahead of the client,
skips a few numbers, and never repeats one. A standby that missed the last
seconds of WAL starts where its copy was, and hands out again some 300,000
numbers that the old primary already used.

That's how the sequence per region, and the composite key built on it, fail. The
other regions may already have received rows with those ids, and their
subscriptions then stop on `insert_exists`, a duplicate key. Two things prevent
this: a synchronous standby, which has the WAL of every committed transaction,
and a logical slot that sends nothing to the subscribers before the standby has
the WAL, which is `failover = true` with `synchronized_standby_slots` (the next
subsection uses both). After any promotion
that may have followed a lagging standby, we also move each sequence past the
highest id that the other regions hold for that region, with a `setval` built
from the catalog, [the way the 2010
article](/blog/2010/02/resetting-sequences.-all-of-them-please/) does it.

A uuid has no state to go back to. The time-based `bigint` has a counter, but the
time in front of it moved on during the failover, so even a repeated counter
value makes a new key, as long as the new primary's clock agrees with the old
one's. In six runs, from a streaming standby and from a cut-off one, the first id
after the promotion was 310 to 350 ms of timestamp above the last id the client
received.

### Failover and switchover with pg_auto_failover, onto a clock that is behind

The stale standby above is a situation we forced by hand. In production, a
region runs under a tool that manages its failovers and keeps a synchronous
standby, so the standby has all the WAL when it is promoted. We ran that setup
too, with [pg_auto_failover](https://oss.theartofpostgresql.com/pg_auto_failover/).

The region is a formation of a monitor and two nodes, the second one a
synchronous physical standby. A logical subscriber in another region follows the
region with a slot created with `failover = true`, which pg_auto_failover copies
to the standby and holds back with `synchronized_standby_slots` until the
standby has the WAL. A writer inserts 1,000 ids per statement through a
two-host connection string. Then we either kill the primary (a failover) or run
`pg_autoctl perform switchover` (a planned switchover), and node 2 becomes the
primary with its clock 0 to 60 seconds behind. We count two things: the ids
refused because the old primary had already made them, and the ids made after
the promotion that are below the highest id made before. Each run makes 5 to 7
million ids, and there is one run per cell:

| What happened | Clock behind | Time-based `bigint`: refused / below | `uuidv7()` |
|---|---|---|---|
| Primary killed, writes back after 34 s | 0 s or 2 s | 0 / 0 | 0 / 0 |
| Primary killed | 60 s | 1 / 3,223,000 (61%) | 0 / 3,229,318 (61%) |
| `pg_autoctl perform switchover`, writes back after 2 to 4 s | 5 s | 0 / 289,000 (11%) | 0 / 527,474 (20%) |
| Switchover | 10 s | 2 / 1,376,000 (55%) | 0 / 1,296,000 (53%) |
| Switchover | 60 s | 0 / 1,892,000 (all) | 0 / 2,346,000 (all) |

Whatever the key, no acknowledged row was missing on the new primary, and the
subscriber ended with exactly the rows of the new primary. The failover slot
survived the promotion, and the subscription reconnected by itself.

The clocks are another matter. The time-based `bigint` and `uuidv7()` need the
clocks of the region's nodes to agree. A failover takes tens of seconds, so by
the time the new primary accepts writes, a clock 2 seconds behind has no effect
on the ids. A switchover takes only a few seconds, so even a clock 5 seconds
behind shows up. When the new primary's clock is behind, its ids go below the
ones the old primary made. The time-based `bigint` can also repeat a few of them:
those are the refused batches, which the writer retried. `uuidv7()` never
repeats, but its ids go out of order just the same.

{{< image src="fig-clock-behind.svg" title="After a promotion onto a clock that is behind, time-based ids go below the highest one made before; regionseq doesn't." >}}

{{< lab >}}
Two things we learnt about pg_auto_failover on the way. First, with the default
`checkpoint_timeout`, the promotion took 274 seconds instead of 34. The keeper
confirms a promotion once the timeline has advanced, which it appears to read
from the checkpoint that follows, and that checkpoint is spread over 270 seconds
by default; the runs set `checkpoint_completion_target = 0`. Second, with 2
seconds between two switchovers, the logical slot was gone from both nodes and
the subscription had to be created again: the demoted node hadn't received the
slot yet when it was promoted again. With 30 seconds between switchovers, the
slot was on both nodes after each of twenty. A rolling maintenance should check
that the slot has reached the standby before the next switchover.
{{< /lab >}}

### A clock that steps back inside a region

A clock that steps back is a different matter from a skew between regions: it
makes the same millisecond come round twice on one server. It's also what a
promotion onto a server whose clock is behind looks like.

We ran four writers at full speed and stepped the clock of the server back while
they inserted. With a step of 2 seconds, 183 to 197 transactions of the
time-based `bigint`, out of about 71,000, failed on a duplicate key. With a step
of 50 or 200 ms none failed, and the `uuidv7()` transactions never failed.

To repeat an id, the clock has to come back to a millisecond it already used,
and the counter has to reach the value it had then. A short step back didn't line
the two up, and a step of 2 seconds did. A narrower counter lines up more often.
Either way the failure is loud, limited to the transactions inside the window,
and the application retries them.

## Two traps on the way

### The 32-bit wall

One failure has nothing to do with regions, and every candidate above can hit
it: a key stored on 32 bits. `serial` is the familiar way to get one. The trap is
that the width of the column and the width of the sequence behind it are two
separate things. Here's what Postgres 19 builds for each way of writing an
auto-numbered column:

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

A `serial` or an `int` identity column gets a sequence of its own width. That
sequence stops at 2,147,483,647 with a clear message, before it hands out a value
the column can't hold.

The dangerous one is the last kind: a sequence created on its own, which is a
`bigint` unless we say otherwise, used as the default of an `int` column. When
the values reach the wall, the sequence goes on, and the column is the one that
stops, with `ERROR: integer out of range`. Every failed insert took a value from
the sequence anyway, so after two attempts the sequence is already two past the
wall, and nothing in the error says which side to look at. And since sequences
are not transactional, an application that retries a failing insert in a loop
moves the sequence faster than it adds rows.

A `serial` column made before Postgres 10 is the same combination, found in the
wild rather than built by hand. Before the `AS` clause of `CREATE SEQUENCE`,
every sequence was a `bigint`, including the one behind a `serial`. A dump
carries no `AS integer` for it, so it stays a `bigint` through every upgrade.
This query lists the integer columns whose sequence is wider than the column:

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

In a system with several writers, the wall comes closer than the table size
suggests. A sequence per region with an increment of 10 spends 10 values for
each row it keeps, which leaves 214,748,364 rows per region before the wall:
about 25 days of a writer doing 100 inserts a second.

Widening a column later rewrites the table, and every foreign key that points at
it has to follow. DDL isn't replicated either, so a link has two columns to
widen, and the order matters: widen the subscriber first, then the publisher.
Otherwise the subscriber stops at the first value past 2,147,483,647, which can
be days after the `ALTER TABLE`. In a mesh, where every region both publishes and
subscribes, every region has to be widened before any of them starts making
large ids. The cheaper way is to get it right on the first day: `bigint`,
and `generated always as identity` rather than `serial`.

### `uuidv7()` before Postgres 18

The built-in `uuidv7()` arrived with Postgres 18, and the regions of a system
don't all upgrade on the same day. Meanwhile, a function short enough to write in
SQL fills the gap. It takes a random v4 uuid, overwrites its first 48 bits with
the Unix time in milliseconds, and sets the version to 7. The variant bits that
`gen_random_uuid()` already set are the right ones for [RFC
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

`gen_random_uuid()` is built in from Postgres 13, and before that the `pgcrypto`
extension provides it. On 13, 17 and 19, the function gives the version nibble 7,
a variant of `8`, `9`, `a` or `b`, and 48 bits that read back as the current
time. There are two differences from the built-in function. For the first one,
we generated a million ids in a single statement and counted how many are
smaller than the one before:

| | ids smaller than the previous one | time for 1,000,000 ids |
|---|---|---|
| SQL function | about 500,000 | 810 ms |
| built-in `uuidv7()` | 0 | 472 ms |

The SQL function fills everything after the millisecond with random bits, so two
ids made in the same millisecond come out in random order. Across milliseconds
the order holds, and that's what the index cares about: new keys land in the
pages of the last few milliseconds, not anywhere in the tree. The built-in
function spends 12 bits on the fraction of the millisecond, and stays ordered
within a session.

The second difference is name resolution. An unqualified `uuidv7()` finds
`pg_catalog` first, so on 18 and later a call written without a schema runs the
built-in function. But a column default is dumped with its schema, `DEFAULT
public.uuidv7()`, and keeps calling the SQL one. Once every region runs 18 or
later, `alter table ... alter column id set default uuidv7()` switches the
column to the built-in function, and `public.uuidv7()` can go.

One last thing about any `uuidv7()`: it carries the time of its creation, to the
millisecond. When the key appears in a URL or an API, that's a public timestamp
for the row, which a random v4 uuid doesn't give away.

## Which key to pick

Here's how we'd choose, depending on who writes and who reads:

- **One region writes, the others only read:** `bigint generated always as
  identity`. There's nothing to coordinate. The 32-bit wall is the one trap, and
  the query above finds the columns that are on it.
- **Several regions write, each row is owned by one of them, and few tables
  point at the key:** `(region, id)`, with one identity column per region and the
  `bigint` declared first in the table. The publication filters on the key
  without a second index, no clock is involved, and there's no increment to
  outgrow. It takes 3% to 10% more storage over a parent and child pair, and each
  region's sequence has to survive a failover: a synchronous standby and a
  failover slot take care of that.
- **The same, with many tables pointing at the key:** a single-column key, which
  takes 8 or 16 bytes in every child row instead of 10 or 16. There are two
  options:
  - The time-based `bigint`, filtered on its bits (`where ((id >> 18) & 15) = 1`)
    without a second index. It needs nothing else and survives a promotion. But
    it repeats ids when a clock steps back by seconds, it's an expression to
    write and maintain, and it allows 16 regions at most.
  - `uuidv7()`, with a `region` column and a unique index on `(id, region)` as
    the replica identity. It's a built-in function and never repeats ids, and its
    second index adds about 85 bytes of WAL per row.
- **Rows are created anywhere, and nobody filters on the owner:** `uuidv7()`. It
  takes 16 bytes, its index behaves like a sequence's, and it needs nothing to
  coordinate, as long as the clocks agree to within the duration of a failover.
- **The creation time must not leak:** `gen_random_uuid()`. Its index is 27%
  larger, it writes 2.7 times the WAL after a checkpoint (487 bytes a row against
  182), and 5 times with a second index (954 bytes).

If we had to keep one rule: `uuidv7()` where nothing filters on the owner and the
clocks are kept in sync, `(region, id)` where the owner matters, a random uuid
only when the creation time must stay hidden, and never 32 bits.

For a table that already has a sequence, two checks come before changing the
key: the query above, for the width of the column, and the increment and
headroom arithmetic of [Part
1](/blog/2026/09/ten-years-of-postgres-logical-replication/#minting-ids-on-the-workers)
if the table is to have several writers. We'd keep the old tables with a single
writer, and use the new key for new tables.

## What a sequence access method could change

The time-based `bigint` is an expression in a column default because a sequence
can't compute its own next value: it can only count. Two patch sets tried to
change that, from 2013 to 2016 and from 2023 to 2026, with `create sequence ...
using method`, so that a `serial` or an identity column could be fed with a
Snowflake-style id instead of a counter. Both were returned with feedback. The
reviews of the second one asked for a use case before the code, and in May 2026
its author dropped it from the CommitFest.

This article is a use case, so we asked what a method would change. Nothing says
that the facility will land in Postgres 20, and it may well not, so what follows
is a thought experiment, not a feature to wait for. If it did land, a method
would be an extension like any other, loaded with `shared_preload_libraries`.
The question is what it would do for a multi-region key that the other keys
can't. To find out, we built the second patch set into a Postgres development
build, and wrote a method as an extension: `regionseq`.

A `regionseq` id is made of 41 bits of milliseconds, 6 bits of region, 4 bits of
timeline and 12 bits of counter. The region comes from a server setting, which
has no default. The timeline is the number that Postgres increments at every
promotion, to tell the history of the new primary from the old one's.

The sequence remembers the last millisecond and counter it used, and never goes
below them. When the clock is behind, it keeps counting in the last millisecond.
When the counter runs out, it moves to the next millisecond instead of waiting
for the clock.

That memory survives a crash and a promotion. The sequence logs a high-water
mark, one second ahead, to the WAL, so after a crash and on a standby the ids
stay above what was already made. And if a standby's copy of that state is
stale, the timeline bits still make the promoted standby's ids differ from the
old primary's.

Production reports suggest that 16 timeline values are enough. Timelines are in
the tens: 11 after about ten switchovers in a [forum
thread](https://forums.percona.com/t/patronictl-tl-mean/27989), and 40 to 81 on a
[Patroni test cluster](https://github.com/patroni/patroni/issues/1733) that is
killed repeatedly. What counts is how many promotions happen within the seconds
or minutes that a standby or a clock is wrong. Since a method makes the same kind
of `bigint` as the expression, the table, the index and the WAL are the same too.

{{< image src="fig-bits-regionseq.svg" title="The 64 bits of a regionseq id, and the expressions that read them." >}}

What a key can promise comes in three levels:

- **Unique:** an id is never made twice.
- **Increasing per region:** the ids of one region only go up. That's what a
  consumer needs when it remembers "the last id I saw from this region", and
  with the region in the key, it can ask for one region at a time.
- **Time-like:** the time in the id is the time the row was created.

Across regions, only the third level holds, and only roughly: replication lag
already puts a row from another region below ids that a reader has seen, so no
key lets a reader keep a single cursor over all regions. The second level, per
region, is the one to design on.

Here are the same tests as before, on the three keys that carry a clock. There
is one run per cell, with about 200,000 ids for the first three lines:

| | Time-based `bigint` | `uuidv7()` | `regionseq` |
|---|---|---|---|
| Clock steps back 2 s / 10 s: ids lower than one the same client already got | 18% / 59% | 0 / 0 | 0 / 0 |
| Same, lower than one any client got | 18% / 59% | 16% / 47% | 0.1% |
| Time in the id ahead of the clock | none | up to 2 s / 10 s | up to 2 s / 10 s |
| Failover onto a clock 60 s behind, standby current: repeats / ids below | 1 / 61% | 0 / 61% | 0 / 0 |
| Switchover onto a clock 5 s, 10 s, 60 s behind: ids below | 11%, 55%, all | 20%, 53%, all | 0, 0, 0 |
| Failover from a cut-off standby with a clock 2 s behind: repeated ids | 782 of 200,000 | 0 | 0 |
| `int` column | `integer out of range` | not applicable | `reached maximum value`, with a hint |

{{< image src="fig-ids-below.svg" title="The same switchover measurement, for the three keys that carry a clock." >}}

`uuidv7()` keeps each session increasing: a backend remembers its last timestamp
and adds to it. The sessions do that separately, so the ids of a region
interleave. `regionseq` keeps the whole region increasing, because its state is
in one place. Both run ahead of the clock until it catches up, and meanwhile the
time in their ids isn't the creation time.

The timeline bits did their job. Without them, a cut-off standby with a clock
behind repeated 24% of the ids of its new timeline. And twenty switchovers in a
row under pg_auto_failover, with the timeline going from 1 to 21 so that the four
bits wrap (`1 ... 15 0 1 ... 5`), made 3,882,000 ids with no repeat, no id below
an earlier one, and no lost row. With a current standby, the state carries the
order, and the timeline is only the second line of defence. One case remains:
two standbys promoted independently from the same primary pick the same timeline
number. It's being discussed on pgsql-hackers, and until then fencing has to
prevent it.

So a method changes two things that the other keys leave to the system around
them, and it costs three. Whether the trade is worth it depends on the
distributed setup, not on the key.

**What it adds**

- **Resilience to a failover.** A sequence per region stays unique only if the
  standby that is promoted is current, which a synchronous standby and a
  failover slot ensure. The time-based `bigint` and `uuidv7()` stay unique if the
  clocks agree within the duration of a failover. `regionseq` stays unique
  whatever the standby had received, because the timeline bits change at every
  promotion, and it keeps increasing when the standby is current. Two standbys
  promoted independently from the same primary still pick the same timeline, so
  fencing stays necessary.
- **Resilience to clock skew.** When the clock steps back, or a promotion lands
  on a server whose clock is behind, the time-based `bigint` makes ids below
  earlier ones and some repeats, and `uuidv7()` makes ids below earlier ones from
  other sessions. `regionseq` keeps counting from the last id it made: no id was
  below an earlier one in the switchover runs. A consumer that reads one region
  as a stream, remembering the last id it saw from that region, can rely on that.

**What it costs**

- **The time in the id stops being the creation time** while the clock catches
  up, by up to the size of the step: the ids run ahead of the clock.
- **State to keep correct.** The high-water mark goes through the WAL, the
  library is preloaded, and the region number comes from a server setting that
  has no default, because the same number on two servers makes the same ids on
  both. The expression has none of this, and sits in the column default.
- **A smaller key space.** 64 regions, 16 timeline values that wrap, 41 bits of
  milliseconds. And the method is an extension that no release provides today.

What the method doesn't change: the table, the index and the WAL are those of the
expression. And it still needs synchronous standbys and watched clocks, because
without them a method fails the way the other keys do.

Each setup decides whether these two properties matter. They do for a system
that reads ids as a stream per region and can't count on its clocks. They matter
less where `uuidv7()` or `(region, id)` runs with synchronous standbys and
monitored clocks. What would change this section is a use with state that an
expression can't hold, such as ranges handed out by consensus or gapless
numbers, which we didn't test.

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
- **Updates and deletes.** Only inserts were measured. A second index set as the
  replica identity also changes what an `UPDATE` writes to the WAL, and we didn't
  count it.
- **Real clocks.** Skews and steps are simulated, with `uuidv7(interval)` for the
  first test and libfaketime offsets on one machine everywhere else, and the
  failover tables are one run per cell. Our skews are 2 seconds and less in the
  mesh. Samokhvalov put a skew of 60 seconds on `uuidv7()` generated by the client
  and got an index 69% full, with 45% of its leaves out of order, and recommends
  generating it on the server.
- **A larger formation, and the edges of the timeline bits.** We ran one region
  of two nodes, and one subscriber. We didn't run three nodes, priorities, or a
  network partition instead of a kill. Nor did we run the case that would make
  `regionseq` repeat (a stale standby, a clock behind and sixteen promotions
  together), or look for the shortest pause between switchovers that keeps a
  failover slot.
- **More than three regions in the mesh,** and keys made by the application,
  with an ORM or a library: they bring their own `uuid` version and their own
  clock.
- **Other uses of a sequence access method:** ranges handed out by consensus,
  gapless numbers, pseudo-random sequences.
- **Postgres 19 after release candidate 1.** The tools were pg_auto_failover's
  main branch, and a Postgres 19 development build for the last section.

## Going further

If the SQL and schema design side of this is your daily work, [The Art of
PostgreSQL](https://theartofpostgresql.com) is the book for it, and every query
of the book runs in [the Lab](https://theartofpostgresql.com/lab/) against real
data. The [self-paced course](https://theartofpostgresql.com/course/) and the
[Masterclass](https://theartofpostgresql.com/masterclass/) are the guided
versions. [pg_auto_failover](https://oss.theartofpostgresql.com/pg_auto_failover/),
used in the failover tests above, has its own page for support and the roadmap.

The scripts, the SQL and the output of every run quoted here are in this
article's directory of the [blog's source
repository](https://github.com/dimitri/tapoueh.org/tree/master/content/post/2026/11/distributed-primary-keys):
`compose/` has one script per experiment and a README that maps them to the
tables above, `results/` the raw output, and `extension/` the `regionseq`
extension, with its regression test.

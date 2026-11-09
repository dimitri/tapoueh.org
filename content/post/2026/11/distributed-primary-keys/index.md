+++
title     = "Distributed primary keys"
date      = "2026-11-04T12:30:00+0100"
tags      = ["PostgreSQL", "Replication", "Data Modeling", "Architecture"]
categories = ["PostgreSQL", "Architecture"]
icon      = "🔑"
+++

<!--
STUB. Nothing below is measured yet: every TODO is a run to do in the lab, and
no number in this file comes from one.

What is already on the site, to cite and not to repeat:
  - Part 1, "Minting ids on the workers" and the two sections before it
    (/blog/2026/09/ten-years-of-postgres-logical-replication/): the collision
    of a naive key (insert_exists, one subscription stuck), modulo and offset
    with its output and the headroom warning, uuidv7 ordering across workers,
    16 bytes against 8, BDR's galloc and snowflakeid, and that Postgres 19's
    sequence replication doesn't help a writer mint ids. Also the row filter
    trap: a filter column outside the replica identity makes the first UPDATE
    fail, with the error message, and the unique index fix.
  - Part 4: the (region, id) key and why no global sequence is needed.
  - Consolidation article: sequences that don't follow, replica identity.
  - 2018, "Database Normalization and Primary Keys": natural against surrogate
    keys, and the surrogate plus unique natural key middle path. It's the
    argument of the book's chapter, don't redo it here.
  - 2018, "Database Modelization Anti-Patterns", UUID section: random UUIDs
    only, argued from the single-sequence SPOF. The part about v4 predates
    uuidv7 and the new article should say what changed and what didn't (a
    UUID is still not a natural key).
  - 11-18 selection: two paragraphs on uuidv7, with "better B-tree locality"
    as an assertion that no article here measures.

What is not on the site yet, and is the reason for this article:
  - the 32-bit wall of serial and int identity columns (measured on 19rc1,
    sql/10-serial-limits.sql and its results/ file);
  - index size, insert rate, WAL volume, full-page images and buffers touched,
    per candidate key, on one server and in the five-region lab;
  - the cost of a wide key in a child table, measured and not just described;
  - a uuid key under a row filter (the filter column has to be in the replica
    identity, so the key needs a companion index, and what does that cost);
  - what a region's sequence returns after a failover;
  - uuidv7 with clocks that disagree between regions.

Book and training material worth adapting (app.taop.xyz), small pieces only:
  - 04-data-types, uuid section: pg_column_size of a uuid against its text
    form (16 and 37 bytes).
  - 04-data-types, sequences section: GENERATED ... AS IDENTITY, ALWAYS against
    BY DEFAULT, for the per-region identity columns.
  - courses/taop-course/03-data-modeling, natural against surrogate: the
    repeated columns in every foreign key, as the example for the wide-key
    measurement.
-->

A single Postgres server hands out primary keys without effort: a `bigint`
identity column asks one sequence for the next number, and no two rows ever get
the same one. Put writers in several regions, each accepting inserts while the
links between them are slow, down, or both, and that comfortable guarantee
needs rethinking. Two regions that each ask their own sequence for "the next
number" will hand out the same one, and the first time those rows meet on a
subscriber, one of them is rejected.

This article is about that choice, for the multi-writer system we built in
[Part 4 of the logical replication
series](/blog/2026/11/postgres-logical-replication-beyond-one-writer/): which
primary key to give a table that several regions write at the same time.
[Part 1](/blog/2026/09/ten-years-of-postgres-logical-replication/#minting-ids-on-the-workers)
listed the ways to mint ids that cannot collide, and showed that they work. It
did not measure what they cost, and that's what we do here, on the same
workload: a sequence per region with its own step, a composite `(region, id)`
key, random UUIDs and time-ordered `uuidv7()`. We look at key size, index
locality, WAL and queries, and at what each choice allows the replication links
to do. What a primary key is, and why a surrogate key doesn't replace a natural
one, is [an older story](/blog/2018/03/database-normalization-and-primary-keys/);
we only use it.

## What a key has to do here

<!-- TODO: three jobs, in this order, kept to a few lines. -->

- **Be unique everywhere**, without asking another region first. That is the
  one thing the single-server sequence doesn't survive; Part 1 shows the
  `insert_exists` error and the subscription that stops on it.
- **Say who owns the row**, because the topology of Part 4 filters on it. A
  row filter can only use columns covered by the replica identity, and Part 1
  shows what the first `UPDATE` does otherwise. For a composite key that's
  already satisfied; for a single-column key it needs a second index, which
  is a cost we count.
- **Stay cheap to index and to join on**, since every child table repeats it.

## The 32-bit wall

Before comparing keys, the one failure that has nothing to do with regions and
that every candidate below can hit: a key stored on 32 bits. `serial` is the
familiar way to get one, and the width of the column and the width of the
sequence behind it are two separate things. We looked at what Postgres 19 release
candidate 1 builds for each way of writing an auto-numbered column:

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

<!-- TODO: run the widening in the lab, with a row-filtered link in both
     directions. DDL isn't replicated, so the order matters: which side first,
     what a subscriber with an int column does when a bigint arrives that
     doesn't fit, and how long the rewrite blocks writers on a table of N
     million rows. Part 1 and Part 4 have the order rule for DDL on core. -->

<!-- TODO: check the older databases. The AS clause of CREATE SEQUENCE is
     from Postgres 10; the claim to confirm, on a cluster that went through
     pg_upgrade, is that a serial column created before it still has a bigint
     sequence behind it, which is the dangerous combination above, found in
     the wild instead of built by hand. -->

## Four candidates

<!-- TODO: one schema block per candidate, same table. Don't re-explain the
     sequence with a step, link Part 1 for its output and the headroom
     warning, and show only what is new: the table, the workload, the
     measurements. -->

| Key | How regions avoid each other | Size of one key |
|---|---|---|
| One sequence per region, with a step: `increment by 10 start with <n>` | the ranges interleave | TODO bytes |
| Composite `(region, id)`, one identity column per region | the region is in the key | TODO bytes |
| `gen_random_uuid()` (v4) | the space is too large to collide | 16 bytes |
| `uuidv7()` | the same, with a time prefix | 16 bytes |

<!-- TODO: optional fifth, a bigint built from time, region and a per-region
     counter, as an SQL function: the shape of the snowflakeid kind that Part 1
     mentions for EDB Postgres Distributed. Say whether it earns its place. -->

### `uuidv7()` before Postgres 18

The built-in `uuidv7()` arrived with Postgres 18, and the regions of a system
don't all upgrade on the same day. The function is short enough to write in SQL:
take a random v4 uuid, overwrite its first 48 bits with the Unix time in
milliseconds, and set the version nibble to 7. The variant bits that
`gen_random_uuid()` already put there are the right ones for
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

The name needs no care at the upgrade. The function lives in `public`, and an
unqualified `uuidv7()` finds `pg_catalog` first, so on 19rc1, with both defined,
the plan shows the built-in one:

~~~ results
 Result
   Output: uuidv7()
~~~

A call written as `public.uuidv7()` keeps running the SQL one. We drop the
function once every region runs 18 or later.

<!-- TODO: a column default is stored against the function that was resolved
     when it was created. Check what a dump and restore, a pg_upgrade and a
     logical copy each do with `default uuidv7()` created on 17, so the
     article can say whether the defaults switch to the built-in by
     themselves or need an ALTER ... SET DEFAULT. -->

<!-- Sources: sql/20-uuidv7-sql.sql, sql/21-uuidv7-native.sql, results/20-*.out.
     The function is adapted from the one in TAOP's download-tracking
     migration (internal/db/migrations/0001_init.sql). NB: the TAOP course
     text 2-1-uuid-gen.sql and the generated page say uuidv7() came with
     Postgres 17; it came with 18. -->

<!-- TODO: a uuidv7 value reveals when a row was created, to the millisecond.
     One sentence, when the key is exposed in URLs. -->

## What each one costs on one server

<!-- TODO: single-node run first, so it can be compared with the multi-region
     numbers. Same table, N million inserts, per candidate:
     - table and primary key index size (pg_relation_size, pg_indexes_size)
     - insert rate and WAL volume (pg_stat_wal: wal_bytes, wal_fpi)
     - buffers touched by the index while inserting: random UUIDs spread over
       the whole index, a time prefix and a sequence append at the right end.
       Part 1 and the 11-18 selection both say "right edge of the index" and
       "better locality"; this is where the number comes from.
     - the cost in a child table: one foreign key column, or two -->

## What each one costs across regions

<!-- TODO: the five-region lab of Part 4.
     - ids produced by writers in two regions at once, and any duplicate-key
       error on the subscribers
     - the row filter: with a uuid key, the companion index the replica
       identity needs and what it adds to size and WAL (Part 1 has the error
       and the fix, not the cost)
     - after a failover of a region: what the new primary's sequence returns
       compared with what the old one had handed out
     - uuidv7 with a few hundred milliseconds of clock skew between regions:
       what ordering is left, since Part 1 only says it's as good as the clocks
     Postgres 19's sequence replication stays out: Part 1 and the
     consolidation article say what it does for a cutover and that it doesn't
     help here. -->

## What `(region, id)` costs

<!-- TODO: adapt the training example of the wide composite key. Every child
     table repeats both columns in its foreign key and in its indexes: measure
     it on a comment-like child table, and say when a single-column surrogate
     on top, with a unique `(region, id)`, is the better trade (the 2018
     article's middle path, in one sentence and a link). -->

## What changed since the UUID advice of 2018

<!-- TODO: short. The old advice was to reach for UUIDs when a central
     sequence is a SPOF, with random UUIDs in mind and an index cost to pay.
     uuidv7() changes the index part, and the measurements say by how much.
     It doesn't change the other half: a UUID is still not a natural key.
     Link the 2018 article and say so, no more. -->

## Which one to pick

<!-- TODO: a decision table, short, from the measurements:
     a single writer region with replicas -> a plain identity column
     several writer regions, rows owned by a region -> ...
     rows created anywhere, no owner -> ...
     and what to do with an existing table that already has a sequence. -->

## What we didn't test

<!-- TODO: only the real list. Candidates: more than five regions, key
     generation in the application, Postgres 19 final release behaviour after
     RC2. -->

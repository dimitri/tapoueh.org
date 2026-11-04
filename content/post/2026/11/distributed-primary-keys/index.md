+++
title     = "Distributed primary keys"
date      = "2026-11-04T12:30:00+0100"
tags      = ["PostgreSQL", "Replication", "Data Modeling", "Architecture"]
categories = ["PostgreSQL", "Architecture"]
icon      = "🐘"
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

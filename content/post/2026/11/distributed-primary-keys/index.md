+++
title     = "Distributed primary keys"
date      = "2026-11-04T12:30:00+0100"
tags      = ["PostgreSQL", "Replication", "Data Modeling", "Architecture"]
categories = ["PostgreSQL", "Architecture"]
icon      = "🐘"
+++

<!--
STUB. Nothing below is measured yet: every TODO is a run to do in the lab, and
no number in this file comes from one. Sources to reuse from the book and the
training (app.taop.xyz), adapt, don't paste:

  - book, 05-data-modeling/generated/normalisation.surrogate-keys.md:
    natural key vs surrogate key, and the middle path (surrogate primary key +
    unique constraint on the natural key) for "Why a key is more than an id".
  - book, 05-data-modeling/generated/anti-patterns.uuids.md: sequences are
    non-transactional, holes, and "UUIDs are useful when you can't synchronize
    against a central sequence (a SPOF)" as the starting point of the argument.
  - book, 04-data-types/generated/pg-data-types-101.sequences-and-the-serial-pseudo-data-type.md:
    serial vs bigserial range failure, GENERATED ... AS IDENTITY (ALWAYS vs BY
    DEFAULT), for the per-region identity columns.
  - book, 04-data-types/generated/pg-data-types-101.universally-unique-identifier-uuid.md:
    gen_random_uuid(), uuidv7(), pg_column_size() of a uuid vs its text form
    (16 vs 37 bytes) for the size table.
  - training, courses/taop-course/03-data-modeling/generated/
    primary-keys-and-constraints.natural-vs-surrogate-primary-keys.md: the
    cost of a wide composite key in every child table (repeated columns in each
    foreign key), for "What (region, id) costs".
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
primary key to give a table that several regions write at the same time. We
compare four ways to build it, a sequence per region with its own step, a
composite `(region, id)` key, random UUIDs and time-ordered `uuidv7()`, on the
same workload, and look at what each one costs in key size, index locality, WAL,
and queries, and what each one allows the replication links to do.

## What a key has to do here

<!-- TODO: three jobs, in this order. -->

- **Be unique everywhere**, without asking another region first. That is the
  one thing the single-server sequence doesn't survive.
- **Say who owns the row**, because the topology of Part 4 filters on it: a
  publication row filter can only look at columns covered by the replica
  identity, so for updates and deletes the owner must be part of the key.
  <!-- TODO: verify in the lab, with a row filter on a column outside the key, and show the error. -->
- **Stay cheap to index and to join on**, since every child table repeats it.

## Four candidates

<!-- TODO: one schema block per candidate, same table, same workload. Use
     GENERATED ... AS IDENTITY for the sequence variants (book, sequences
     section). -->

| Key | How regions avoid each other | Size of one key |
|---|---|---|
| One sequence per region, with a step: `increment by 5 start with <n>` | the ranges interleave | TODO bytes |
| Composite `(region, id)`, one identity column per region | the region is in the key | TODO bytes |
| `gen_random_uuid()` (v4) | the space is too large to collide | 16 bytes |
| `uuidv7()` | the same, with a time prefix | 16 bytes |

<!-- TODO: add a fifth, optional: a bigint built from time, region and a
     per-region counter, written as an SQL function. Say whether it earns its
     place or just adds one more way to get the clock wrong. -->

## What each one costs on one server

<!-- TODO: single-node run first, so the numbers can be compared with the
     multi-region ones. Same table, N million inserts, per candidate:
     - table and primary key index size (pg_relation_size, pg_indexes_size)
     - insert rate and WAL volume (pg_stat_wal: wal_bytes, wal_fpi)
     - buffers touched by the index while inserting (random UUIDs spread across
       the whole index, a time prefix and a sequence append at the right end)
     - the cost in a child table: one foreign key column, or two
     Book hook: "UUIDs are useful in distributed computing", then measure
     whether the index pays for it. -->

## What each one costs across regions

<!-- TODO: use the five-region lab of Part 4.
     - ids actually produced by writers in two regions at once; any
       duplicate-key error on the subscribers
     - the row filter: which candidates allow `where (region = ...)` on the
       publication, and which need a separate region column in the key
     - Postgres 19 sequence synchronization (`for all sequences`,
       `refresh sequences`): what it does for a cutover, and that it doesn't
       let two writers share a sequence (Part 4, last section)
     - after a failover of a region: what the new primary's sequence returns
       compared with what the old one had handed out -->

## What `(region, id)` costs

<!-- TODO: adapt the training example of the wide composite key. Every child
     table repeats both columns in its foreign key and in its indexes: measure
     it on a comment-like child table with a few hundred million rows
     extrapolated from a smaller one, and say when a surrogate on top (a
     single-column `id` plus a unique `(region, id)`) is the better trade. -->

## Which one to pick

<!-- TODO: a decision table, short, from the measurements:
     a single writer region with replicas -> a plain identity column
     several writer regions, rows owned by a region -> ...
     rows created anywhere, no owner -> ...
     and what to do with an existing table that already has a sequence. -->

## What we didn't test

<!-- TODO: only the real list. Candidates: more than five regions, key
     generation in the application, `uuidv7()` clock skew between regions,
     Postgres 19 final release behaviour after RC2. -->

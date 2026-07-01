+++
title = "pgcopydb v0.18"
date = "2026-07-01T16:05:56+0200"
tags = ["PostgreSQL", "pgcopydb", "Migration", "CDC", "Logical Replication"]
categories = ["PostgreSQL", "Projects", "pgcopydb"]
icon = "🗞️"
+++

Hot off the press: [pgcopydb
v0.18](https://github.com/dimitri/pgcopydb/releases/tag/v0.18) is out!

It's the biggest release the project has had — 88 commits since v0.17, which
shipped in August 2024. I took a break from my Open Source responsibilities
for a while, because I was lacking employer support to make it happen.

## What is pgcopydb

pgcopydb copies a PostgreSQL database to another PostgreSQL server, as fast
as possible when physical file copy isn't available. It parallelises the
COPY across all tables simultaneously, builds indexes in parallel after data
is loaded, and supports Change Data Capture via logical replication for
minimal-downtime migrations. It is designed to be restartable: state is
tracked in a local SQLite catalog so an interrupted run can resume where it
left off.

## Headline Features of pgcopydb v0.18

v0.18 brings compatibility with PostgreSQL 16, 17, and 18; a pgoutput-default
CDC engine with significant reliability and performance improvements;
regular-expression-based filtering; Citus-to-Citus migration support; and
24 bug fixes.

<!--more-->
<!--toc-->

### PostgreSQL 17 and 18

pgcopydb now fully builds and is tested against PostgreSQL 16, 17, and 18. This
includes CI coverage for PostgreSQL 18's breaking changes (build system, large
object API, `pg_dump` metacommand syntax). The `pg_dump \restrict` / `\unrestrict`
metacommand fix (#947) also covers CVE-2025-8714.

### pgoutput is now the default decoding plugin

pgcopydb previously defaulted to `test_decoding`.

Now, `pgoutput` is the default logical decoding plugin. This matters for two
reasons: pgoutput is the plugin used by standard PostgreSQL logical
replication, which means its output is more structured and more widely
tested; and it is the plugin required for filtered publications.

**Filter-aware publications**: when a filter INI file is active, pgcopydb now
creates a PostgreSQL publication that matches the filter set, rather than
publishing all tables. This limits WAL decode overhead on the source to exactly
the tables being migrated. The same filter is passed to `wal2json` when that
plugin is used instead.

### CDC engine rewrite: SQLite instead of JSON

The CDC streaming engine has been rewritten to use SQLite as its storage format.
Previously, WAL changes captured from logical decoding were written as JSON files;
they are now stored in two structured SQLite databases — `output.db` (decoded
changes) and `replay.db` (applied changes). This brings several concrete
improvements:

- **Restart safety**: because SQLite is transactional, a process crash during
  streaming no longer risks partially-written change records. Processing resumes
  from a clean state.
- **Size-based file rotation**: when an output database grows past a configurable
  threshold, a new database file is opened automatically. This bounds unbounded
  growth in long-running CDC sessions.
- **Stream cleanup**: a new `pgcopydb stream cleanup` command reclaims disk
  space by removing processed CDC database files, and the cleanup happens
  automatically every five minutes in the background when the follow (sub-)
  command is running.
- **Performance**: catalog inserts into the SQLite internal catalog are now batched,
  and index creation is deferred. Attribute lookups in the transform hot path are
  cached.

### Clone an entire Postgres instance with --all-databases

A new `--all-databases` flag clones every database in a PostgreSQL instance in a
single command:

```
pgcopydb clone --all-databases \
  --source "postgres://user@source/" \
  --target "postgres://user@target/"
```

This feature is not just about convenience, it is also about performance
characteristics and run-time resource sharing. When using the
`--all-databases` option pgcopydb fetches the catalog metadata for all the
databases found in the source database, each in their own SQLite catalog,
and then merges them to find the optimal ordering across all databases,
multiplexing COPY and CREATE INDEX operations to ensure lower migration time
overall.

### Citus-to-Citus migration

pgcopydb now supports cloning a Citus cluster to another Citus cluster. This
covers the Citus-specific metadata: distribution columns, colocation groups,
reference tables, and the shard placement map. Documentation for TimescaleDB
extension-aware migration was also added in this release.

### Richer filtering

v0.18 extends the filter INI file in two directions:

**Regex patterns** — table names in filter sections now accept `~/pattern/`
syntax for regular expression matching:

```ini
[exclude-table-data]
~/^log_/
~/^audit_/
```

**Extension filtering** — two new sections control which extensions are copied:

```ini
[include-only-extension]
timescaledb
postgis

[exclude-extension]
pg_stat_statements
```

This new filtering with regular expressions was also the opportunity to
implement a new approach: rather than creating TEMP tables on the source
database to use SQL anti-JOIN operators, we now use text, regexp, and OID
arrays as SQL query parameters.

This means pgcopydb is fully compatible with read-only source servers now,
including standby servers, and even when using filtering.

## Bug fixes

24 bug fixes shipped in v0.18. A selection of the more significant ones:

- **SQL_ASCII source encoding**: `client_encoding` is now forced to `SQL_ASCII`
  when the source database uses that encoding, preventing garbled data in
  databases that deliberately use non-UTF-8 byte sequences.
- **LOCK TABLE failure**: a `SAVEPOINT` now wraps the `LOCK TABLE` check in
  worker transactions, so a lock failure no longer aborts the whole COPY
  transaction.
- **Binary-incompatible columns**: pgcopydb detects when a column type is not
  binary-compatible between source and target and falls back to text COPY
  automatically, rather than failing.
- **GENERATED ALWAYS AS IDENTITY in CDC**: identity columns were incorrectly
  included in CDC `UPDATE SET` clauses, causing replay errors. They are now
  skipped.
- **Materialized views in CDC**: `REFRESH MATERIALIZED VIEW` operations are now
  handled correctly during CDC apply.
- **Double precision in CDC**: floating-point values are now preserved at full
  precision through the CDC pipeline rather than being rounded in string
  serialization.

The full list is in the
[CHANGELOG](https://github.com/dimitri/pgcopydb/blob/main/CHANGELOG.md).

## By the numbers

<table class="stats-table">
  <thead>
    <tr><th colspan="2">pgcopydb v0.18</th></tr>
  </thead>
  <tbody>
    <tr><td>Commits since v0.17</td><td>88</td></tr>
    <tr><td>Development period</td><td>Aug 2024 → Jun 2026 (23 months)</td></tr>
    <tr><td>New features</td><td>13</td></tr>
    <tr><td>Changed / improved</td><td>8</td></tr>
    <tr><td>Bug fixes</td><td>24+</td></tr>
    <tr><td>CVEs fixed</td><td>CVE-2025-8714</td></tr>
    <tr><td>PostgreSQL versions tested in CI</td><td>16, 17, 18</td></tr>
  </tbody>
</table>

## Getting pgcopydb v0.18

```
docker pull dimitri/pgcopydb:v0.18
```

Packages for Debian, Ubuntu, and RPM-based distributions are available at
the [pgcopydb releases page](https://github.com/dimitri/pgcopydb/releases/tag/v0.18).

## Supporting the project

pgcopydb is maintained as an open-source project. Development is funded
through the professional support program at
[oss.theartofpostgresql.com](https://oss.theartofpostgresql.com).

If your organization runs pgcopydb for production migrations, a subscription
helps ensure the project continues to receive prompt attention when
PostgreSQL releases or your infrastructure changes introduce new edge cases.
Community support remains free, as always.

If pgcopydb has earned its place in your infrastructure, making it
sustainable is a conversation worth having with your team.

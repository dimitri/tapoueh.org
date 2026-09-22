# Zero-downtime major upgrade with logical replication (demo)

Everything runs in Docker Compose (project `lrupg`, host ports 5730-5739; only
5730-5734 are used). Nothing outside this directory is touched.

| server | image | host port | role |
| --- | --- | --- | --- |
| `old` | `postgres:16` (16.15) | 5730 | legacy primary, publisher |
| `new` | `postgres:18` (18.6) | 5731 | upgrade target, subscriber |
| `traffic` | `postgres:18` | - | the "application": a psql loop, and the ops scripts |
| `upg` | `postgres:18` + PGDG `postgresql-17` (17.11) | - | part B, pg_upgrade helper |
| `pg19a`, `pg19b`, `pg19c` | `postgres:19beta3-bookworm` | 5732-5734 | part C, **beta** |

## Reproduce in three commands

```
make all        # clean, then part A, part B, part C (about 8 minutes)
less results/22-cutover.out
make clean      # remove containers and volumes (make distclean also deletes results/)
```

Parts can be run alone: `make a`, `make b`, `make c` (each `up` + `run`).
`make verify` runs everything twice from clean and diffs the normalised
outputs (about 20 minutes, see "Reproducibility").

## How it works

`run.sh FROM TO` executes `sql/NN-*` in order, for `FROM <= NN <= TO`.
The first line of each step names where it runs (`-- @service: old` or
`# @service: traffic`; `host` means on the host). `.sql` steps go through
`psql -X -a` (no prompts, the statements are echoed), `.sh` steps through
`bash -s` in the container. Raw stdout+stderr goes to `results/NN-name.out`.
Errors are not hidden: several steps provoke errors on purpose.

The application is `traffic/traffic.sh`: one transaction per loop turn (7 writes:
customer, order, invoice number from a standalone sequence, no-PK audit row,
partitioned measurement, counter update), about 50 transactions per second, connecting
to whichever host `/tmp/target` names. Switching the file is our stand-in for
"switch the app". Every attempt is logged with its end time, so the write stall
is measured from the client side: last commit acknowledged on the source to first
commit acknowledged on the target.

## Part A - 16 to 18

| step | what it shows |
| --- | --- |
| 10 | roles, schema (identity, serial, standalone sequence, FK, generated column, no-PK table, partitioned table, matview, large object) and about 1M rows on 16 |
| 11 | `CREATE PUBLICATION ... FOR ALL TABLES`; UPDATE/DELETE on the no-PK table then fail on the publisher until `REPLICA IDENTITY FULL` |
| 12 | traffic starts on `old` |
| 13 | roles with `pg_dumpall --roles-only` (one error: `postgres` exists), then `pg_dump --schema-only` (18's pg_dump against 16) |
| 14-15 | does `CREATE SUBSCRIPTION` need a superuser? (`pg_create_subscription`, scratch database) and the log lines |
| 16-17 | `CREATE SUBSCRIPTION ... copy_data = true`; watching `pg_subscription_rel` (`i`, `d`, `r`); the whole initial copy takes about a second |
| 18 | what is missing after the sync: sequences, matview content, large objects |
| 19 | DDL on the old side during the migration (ADD COLUMN, CREATE TABLE) |
| 20 | `WAIT FOR LSN` on the subscriber, and the LSN comparisons available instead |
| 21 | rollback path prepared before cutover: reverse subscription, `origin = none`, `copy_data = false` |
| 22 | the timed cutover |
| 23 | post-cutover chores: matview refresh, large object copy |
| 24 | rollback demo, traffic back on 16 |
| 25 | stop traffic; per-table row counts and content hashes on both servers; acknowledged writes versus rows; both stalls |
| 26 | error/warning log lines of both servers |

### Cutover procedure (step 22, `traffic/lib.sh`)

1. **Freeze writers** on `old`: `ALTER DATABASE app SET default_transaction_read_only = on`,
   then `pg_terminate_backend()` on the `app` role. Chosen over REVOKE CONNECT / NOLOGIN
   because reads keep working, writers get a precise error (SQLSTATE 25006) instead
   of a connection failure, there is no lock queue (unlike `LOCK TABLE`, which makes
   writers hang instead of fail) and it is undone by one command. Costs: it is advisory
   (a client can `SET default_transaction_read_only = off` itself) and it also applies to
   your own admin sessions: the scripts export `PGOPTIONS='-c default_transaction_read_only=off'`.
   A pooler `PAUSE` is the better tool where you have one (not tested here).
2. **Catch up.** Write a marker row on `old`, wait until it is visible on `new`;
   the two LSN methods are measured next to it, see the surprises below.
3. **Sequences**: `setval()` statements generated from `old.pg_sequences`, run on `new`.
4. **Switch the app.**

Steps that need to be done by hand (not replicated): sequences, materialized
view content, large objects, DDL, roles, `REPLICA IDENTITY` for tables without PK.

### Rollback path (steps 21 and 24)

Before the cutover: `ALTER SUBSCRIPTION app_sub SET (origin = none)` on `new`, and on `old`
`CREATE SUBSCRIPTION app_rev ... WITH (copy_data = false, origin = none)` pulling from
`new` (whose `app_pub` came along in the schema dump). With `origin = none` a
subscription ignores changes that were themselves applied by a subscription, so nothing
loops. The forward subscription needs `origin = none` too: otherwise the rows that
`app_rev` applies on `old` would be sent back to `new`. Rolling back is the cutover
mirrored: freeze `new`, marker, wait on `app_rev`, `setval()` from `new` to `old`,
unfreeze `old`, switch.

## Part B - pg_upgrade 17 to 18 with subscriptions and slots

Steps 30-32 in the `upg` container (built from `upg/Dockerfile`): two 17 clusters
(publisher 5441, subscriber 5442 inside the container), `pg_upgrade --link` of the
subscriber then of the publisher. See the outputs for what is kept and what is not.
This was done completely, not degraded.

## Part C - PostgreSQL 19 beta3 (BETA: everything here may change or be reverted before GA)

Steps 40-47 on `pg19a` (publisher), `pg19b` (subscriber, `track_commit_timestamp = on`),
`pg19c` (default `wal_level = replica`): `FOR ALL SEQUENCES` and `REFRESH SEQUENCES`,
`FOR ALL TABLES EXCEPT (TABLE ...)`, `retain_dead_tuples` and `update_deleted`,
`effective_wal_level`, `CREATE SUBSCRIPTION ... SERVER`, `WAIT FOR LSN`.

## Findings worth quoting (all from the outputs in `results/`)

Versions: PostgreSQL 16.15, 17.11, 18.6 (official images), 19beta3 (`postgres:19beta3-bookworm`).

* **Cutover write stall** (client side, last commit on the source to first commit on the target):
  about 180-200 ms in both directions (steps 22 and 24), 4 failed attempts inside the window
  (`cannot execute INSERT in a read-only transaction`), no acknowledged write lost, every table
  identical by content hash on both servers (step 25). The procedure itself takes about 160 ms
  (freeze 35 ms, catch-up about 65 ms, sequences 50 ms); the rest is the loop's own retry cadence.
  The initial copy of about 100 MB took about one second.
* **`WAIT FOR LSN`** does not exist in 16 or 18 (`ERROR:  syntax error at or near "WAIT"`, step 20).
  It exists in 19beta3 but is a physical-standby feature: on a logical subscriber the
  `standby_*` modes fail with `recovery is not in progress`, and the `primary_flush`
  mode answers `success` as soon as the subscriber's OWN wal position passed the number, which says
  nothing about whether the publisher's changes were applied (step 46). Do not use it for this.
* **Comparing `pg_current_wal_lsn()` (old) with `remote_lsn` (new)** only works if the last WAL record
  of the old server is a replicated commit. The freeze itself (`ALTER DATABASE ... SET`) writes a
  commit that logical replication never sends, so with the LSN taken right after the freeze
  `remote_lsn` never got there in a development run (waited 7.5 s, gave up), while
  `pg_stat_subscription.latest_end_lsn` reached it after 68 ms. The latter is updated when a message
  is RECEIVED, before it is applied, so it is not proof either. What the scripts do: write a marker
  row (`cutover_markers`) after the freeze and wait for it to show up on the new server; with the marker
  the three tests agree within 1-2 ms (the `[time]` lines of steps 22 and 24).
* The read-only freeze also freezes the DBA: `setval()` and `ALTER DATABASE ... RESET` on the frozen
  server failed with `cannot execute setval() in a read-only transaction` until the admin sessions
  were given `PGOPTIONS='-c default_transaction_read_only=off'` (a development run). The app's
  sessions were not terminated at all in the demo (0 sessions): the loop reconnects for each
  transaction; a real pool would show a number there.
* `pg_dumpall --roles-only` into the new server: `ERROR:  role "postgres" already exists`, the rest
  applies. `pg_dump --schema-only` also carries the publication, creates the materialized view
  `WITH NO DATA` (`materialized view "sales_by_day" has not been populated`), and the
  `REPLICA IDENTITY FULL` setting.
* After the sync: sequences on the new server have `last_value` NULL (an INSERT there fails with
  `duplicate key value violates unique constraint "orders_pkey"`), the matview is empty, the large object
  is missing (`large object N does not exist`) although `documents.body_oid` was replicated, the
  stored generated column is recomputed by the subscriber.
* `CREATE SUBSCRIPTION` as a non-superuser (step 14): with `pg_create_subscription` alone
  `ERROR:  permission denied for database scratch` (needs `CREATE` on the database); a conninfo
  without a password gives `ERROR:  password is required` / `DETAIL:  Non-superusers must provide a password in the connection string.`;
  then the table sync worker fails with `ERROR:  role "migrator" cannot SET ROLE to "postgres"` until the
  subscription owner owns the table (or `run_as_owner`); the publisher side needs a `REPLICATION` role
  that can `SELECT`. `CREATE PUBLICATION ... FOR ALL TABLES` needs a superuser on the publisher (not tested as non-superuser).
* DDL on the old side while `FOR ALL TABLES` is in force (step 19): nothing fails on the old server;
  the new server's apply worker fails and retries, and replication is stuck behind it:
  `logical replication target relation "public.customers" is missing replicated column: "note"` and
  `logical replication target relation "public.late_table" does not exist`. Creating the object on the new
  server unblocks the first; for a new table `ALTER SUBSCRIPTION ... REFRESH PUBLICATION` is needed as well
  (without it the table was not in `pg_subscription_rel`, the row did not arrive).
* Reverse subscription: `copy_data = false` is required; with `copy_data = true` on tables that already
  have the rows the tablesync worker fails with `duplicate key value violates unique constraint "t_pkey"`
  and sits in state `d` (scratch database, step 21). The forward subscription must be switched to
  `origin = none` first. Not tested: what happens if it is not (reasoning: the rows applied by `app_rev` would
  be sent back). After a rollback the old server's sequences are stale, hence the `setval()` back.
* Part B: `pg_upgrade` 18 refuses a 17 cluster without checksums
  (`old cluster does not use data checksums but the new one does`): `initdb --no-data-checksums`.
  Subscriber: the subscription, its table states (`r`) and its origin position survive, and it stays
  ENABLED (nothing to enable by hand; it resumes on start). Publisher: the slot survives
  (`Restoring logical replication slots in the new cluster  ok`), provided it consumed all WAL
  (`The slot "sub" has not consumed the WAL yet` otherwise) and the new cluster has `wal_level = logical`
  (`"wal_level" must be "logical" but is set to "replica"`). Not carried over: `postgresql.conf` /
  `pg_hba.conf` (you configure the new cluster), and statistics counters (`Some statistics are not transferred by pg_upgrade.`).
* Part C (19beta3, beta): see the outputs. Only `FOR ALL SEQUENCES` exists (`FOR SEQUENCE s1` is a syntax
  error); sequence values reach the subscriber at the initial sync and at `ALTER SUBSCRIPTION ... REFRESH SEQUENCES`,
  not periodically (still stale after 20 s); a sequence created later needs `REFRESH PUBLICATION` first.
  `FOR ALL TABLES EXCEPT (TABLE ...)` works, also `ALTER PUBLICATION ... SET ALL TABLES EXCEPT (...)`;
  excluding a partitioned table excludes its partitions. `retain_dead_tuples` produces
  `conflict=update_deleted` where the same scenario without it gives `update_missing`
  (`pg_stat_subscription_stats.confl_update_deleted`), using a physical slot `pg_conflict_detection` on the
  SUBSCRIBER; without `track_commit_timestamp` it only warns. `effective_wal_level` follows the first logical slot
  (`logical decoding is enabled upon creating a new logical replication slot`) and goes back
  (`logical decoding is disabled because there are no valid logical replication slots`), no restart.
  `CREATE SUBSCRIPTION ... SERVER` works with `postgres_fdw` (whose `fdwconnection` is `postgres_fdw_connection`).
  Gotcha: with the default `max_logical_replication_workers = 4` a fifth subscription silently never starts.

## Reproducibility

`make verify` compares two clean runs after `normalize.sh`, which drops the `[poll]` lines
(timing samples), and masks LSNs, timestamps, pids, md5 hashes, every integer of three or
more digits (row counts, milliseconds, oids, xids, sequence values), the column padding
psql applies, and a few small timing dependent counters. So it verifies the
structure, errors, states and messages, not the timings. The raw results in `results/`
are those of the last run.

## Files

```
docker-compose.yml   Makefile   run.sh   verify.sh   normalize.sh   README.md
traffic/  traffic.sh traffic.sql lib.sh        the app loop and ops helpers
upg/      Dockerfile                          PG 17 + 18 helper image
sql/      NN-*.sql, NN-*.sh                   the steps (first line: -- @service: x)
results/  NN-*.out                            raw output, one per step
```

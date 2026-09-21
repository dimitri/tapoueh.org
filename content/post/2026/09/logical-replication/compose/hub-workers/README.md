# Hub and workers: write scalability with logical replication

A metering/invoicing application. One `hub` database owns the reference data (customers,
plans, prices) and does the aggregation and invoicing. N `worker` databases each take a share of
the traffic and write usage events locally for the customers assigned to them.

- reference data flows **hub -> workers** (each worker gets only its slice of the customers),
- usage events flow **workers -> hub**, into ONE table partitioned by `worker_id`.

This is the topology Londiste/PgQ used to be used for. Everything here is core logical
replication on PostgreSQL 18 (`postgres:18`, tested on 18.6), plus a variant with pglogical on
PostgreSQL 14 (14.24, pglogical 2.4.8).

## Reproduce

```
make clean && make up      # 4 containers (hub, worker1..3), waits for pg_isready over TCP
make run                   # runs sql/01..20 in order, writes results/*.out (about 20 s)
make down                  # (or `make clean` to also drop the data)
```

Variant on PG14 + pglogical: `make pglogical-up && make pglogical-run` (about 5 s, plus the image build).
Optional crash reproduction (1 to 2 minutes): `make pglogical-crash` (see step 7 below).

Requirements: Docker with Compose v2, `bash`, `perl`, `bc`. Compose project name `lrhub`,
host ports 5710-5717. Data lives on tmpfs, so nothing survives `down`. Never use
`--remove-orphans` here: both compose files belong to the same project.

`./run.sh core [prefix]` runs the steps; a prefix such as `10` runs only that step (development aid).
The steps assume a fresh `make clean && make up`; running `make run` twice on the same containers
fails on the first `CREATE TABLE`.

## How it works

- `sql/NN-name.sql`: first line `-- @service: <container(s)>`. A second line `-- @start: worker4`
  starts that compose service (profile `worker4`) and waits until healthy first.
- `results/NN-name.out`: exactly what `psql -X -a` printed (stdout and stderr merged, so NOTICE and
  ERROR lines are in place; `-a` echoes the statements). When a file runs on several services,
  `run.sh` writes a `== worker1 ==` line before each service's output: that is the only line not
  printed by psql. Timings per step: `results/timings.txt` (not compared).
- `lib/lr-helpers.sql`: scaffolding loaded silently on every node before each step: the `lr` schema
  with `lr.wait_for(cond)` (polls a boolean expression, no fixed sleeps; a bounded timeout raises an
  error), `lr.wait_caught_up()` (publisher: every walsender confirmed the current WAL position),
  `lr.log_lines()` / `lr.log_last_lsn()` (read the server log with `pg_read_file`, the servers log to
  `log/pg.log`), `lr.norm()` (normalises LSNs, xids and timestamps in log text) and `lr.on(node, sql)`
  (run a statement on another node through dblink, so that a cross-node timeline stays in one file).
- Reproducibility: the SQL only prints stable things. LSNs, xids and timestamps in log lines are
  normalised by `lr.norm` (`X/X`, `transaction N`, `TS`, `pg_OID`); counters that depend on how long a
  retry loop ran are printed as booleans; the few measured durations are printed on lines containing
  `unstable`. `./compare.sh dirA dirB` diffs two results directories ignoring those lines.
  Four full runs from `make clean` were compared: identical, except the intentionally crashing
  step 79 (see below).
- Server settings that are not defaults (see `docker-compose.yml`): `wal_level=logical`,
  `track_commit_timestamp=on`, `max_logical_replication_workers=16` (default 4 is too few for a hub
  with 4 subscriptions plus parallel apply workers), `wal_retrieve_retry_interval=1s` (default 5s; it
  only speeds up the retry loops shown in the stall demos), logging to a file, `trust` authentication
  (demo only). Step 3 additionally sets `logical_decoding_work_mem=64kB` on worker1 (the minimum),
  otherwise 300000 narrow rows would not exceed the 64MB default and nothing would be streamed.

## Steps and what they demonstrate

| Step | Files | Demonstrates |
|---|---|---|
| 1 Reference data down | 01-06 | one publication and one subscription per worker; then per-worker publications with a row filter (`WHERE worker_id = N`) and a column list (billing-only column not published); a worker sees only its slice; UPDATE that moves a row between filters |
| 2 Usage events up | 07-09 | key `(worker_id, event_id)` cannot collide; hub table `PARTITION BY LIST (worker_id)` as subscription target; cross-worker invoicing query; a per-node-only key collides, the apply worker stops and retries, log line and counters |
| 3 Big transaction | 10 | 300000-row transaction with `streaming = off` and `= parallel` |
| 4 Operating | 11-17 | worker 4 added live (partition attach + subscription); duplicate-key recovery with `ALTER SUBSCRIPTION ... SKIP`; PG18 `confl_*` counters; lag from both sides |
| 5 Direction rule | 18-19 | reference and usage tables travel in opposite directions; two-node `origin = none` test |
| 6 Sequences | 20 | a sequence is not replicated; consequence for ids minted on workers |
| 7 pglogical on PG14 | 70-74, 79 | same flow with `row_filter` + replication sets; `last_update_wins` |

### Observed facts (from `results/*.out`)

Step 1
- The row filter column must be in the replica identity **for UPDATE and DELETE on the publisher**.
  With the primary key as replica identity, *every* UPDATE on `customers` (even of an unrelated column)
  fails at UPDATE time, not at `CREATE PUBLICATION` time:
  `ERROR:  cannot update table "customers"` / `DETAIL:  Column used in the publication WHERE expression is not part of the replica identity.`
  Fix used: `CREATE UNIQUE INDEX customers_rid ON customers (customer_id, worker_id)` +
  `ALTER TABLE customers REPLICA IDENTITY USING INDEX customers_rid` (no `REPLICA IDENTITY FULL` needed).
- Filtered UPDATE moving a row from worker 1's filter to worker 2's: worker 1 receives a DELETE,
  worker 2 receives an INSERT (verified: `has_customer_3` is 0 on worker1, 1 on worker2).
- Switching a subscription to a filtered publication (`ALTER SUBSCRIPTION ... SET PUBLICATION ... WITH (refresh = false)`)
  is **not retroactive**: rows and column values already copied stay on the worker (9 customers and
  9 billing notes still there); cleaning up is the operator's job.
- The subscription name is the slot name on the hub: with the same name on every worker the second
  `CREATE SUBSCRIPTION` fails with `replication slot "sub_ref" already exists`. Use `sub_ref_w<N>`.

Step 2
- Subscribing into a partitioned table works (initial copy and streaming); rows are routed to `usage_events_w1..w3`.
- Collision (worker1 and worker2 both mint `event_id` 1..3 in a table keyed on `event_id` alone), PG18 log:
  ```
  ERROR:  conflict detected on relation "public.usage_naive": conflict=insert_exists
  DETAIL:  Key already exists in unique index "usage_naive_pkey", modified by origin pg_OID in transaction N at TS.
          Key (event_id)=(1); existing local row (1, 1, 1); remote row (1, 4, 1).
  CONTEXT:  processing remote data for replication origin pg_OID during message type "INSERT" for replication target relation "public.usage_naive" in transaction N, finished at X/X
  ```
  The apply worker exits and is restarted every `wal_retrieve_retry_interval` to fail again;
  `pg_stat_subscription_stats.apply_error_count` and `confl_insert_exists` grow together, `pid` in
  `pg_stat_subscription` flaps to NULL. The other subscriptions keep flowing.

Step 3
- `pg_subscription.substream` for a subscription created without the option is `p` (parallel) on PG18.
- `streaming = off`: no parallel apply worker for this subscription; publisher-side slot shows
  `spill_txns > 0`, `stream_txns = 0`; the hub sees nothing before COMMIT; measured commit-to-visible on the hub
  about 0.8-1.0 s for 300000 rows.
- `streaming = parallel`: a parallel apply worker exists for the subscription before COMMIT, still 0 rows
  visible on the hub before COMMIT (uncommitted), publisher `stream_txns > 0`; commit-to-visible about
  0.08 s. So the apply work moved before the commit; no claim is made about total wall time.
- `sub_naive_w1`, which publishes nothing of that transaction, went from 0 to 1 parallel apply workers
  during the big transaction (observed; the likely reason is that the worker's walsender streams the in-progress
  transaction on every slot, even where none of its changes are published: not verified further).

Step 4
- Lock on the partitioned parent: `ATTACH PARTITION` takes `ShareUpdateExclusiveLock`,
  `CREATE TABLE ... PARTITION OF` takes `AccessExclusiveLock`. Create the table standalone with a matching
  CHECK constraint, then attach it. Adding worker 4 left the apply worker processes of the three existing
  subscriptions untouched (same pids).
- SKIP: the LSN comes from the `finished at X/X` of the CONTEXT line; log lines
  `logical replication starts skipping transaction at LSN X/X` and `... completed skipping transaction at LSN X/X`.
  The skipped transaction is lost for good (worker2's 3 rows never reach the hub): divergence to repair by hand.
- PG18 columns of `pg_stat_subscription_stats`: `subid, subname, apply_error_count, sync_error_count,
  confl_insert_exists, confl_update_origin_differs, confl_update_exists, confl_update_missing,
  confl_delete_origin_differs, confl_delete_missing, confl_multiple_unique_conflicts, stats_reset`.
  `update_missing`, `delete_missing` and `update_origin_differs` do not stop the apply worker (log level LOG):
  `conflict detected on relation "public.usage_events_w1": conflict=update_missing` with
  `DETAIL: Could not find the row to be updated.` (and `... to be deleted.`, and
  `Updating the row that was modified locally in transaction N at TS.` for origin_differs, where the remote change is applied).
  `insert_exists` is an ERROR.
- Lag: on the publisher `pg_stat_replication` (one row per subscription, `application_name` = subscription name)
  and `pg_replication_slots` (`wal_status`, retained WAL); on the subscriber `pg_stat_subscription`.
  With `sub_usage_w3` disabled, worker3's slot goes inactive and keeps retaining WAL.

Step 5
- Each node's published and subscribed table sets are disjoint. Round 1 of the origin test: same table both ways,
  default `origin = any`, no primary key: one inserted row is bounced back and forth and multiplied without end.
  Round 2, with a primary key: the boomerang row hits `insert_exists` on its own key
  (`Key already exists in unique index "settings_pk_pkey", modified locally in transaction N at TS.`) and the subscription stalls.
  Round 3, `origin = none` on both subscriptions (PG16+): exactly one copy of each row, 0 apply errors.
  `pg_subscription.suborigin` is `any` by default and `none` when set.

Step 6
- `CREATE PUBLICATION ... FOR ALL SEQUENCES` is a syntax error on PG18 and
  `FOR TABLE <sequence>` fails with `This operation is not supported for sequences.`
  `nextval` 5 times on the hub leaves the worker's same-named sequence at 1. The identity sequence of a
  replicated table on a worker never moves: `INSERT` with a default id on worker1 fails with
  `duplicate key value violates unique constraint "customers_pkey"` (`Key (customer_id)=(1)`).

## Step 7: the same flow with pglogical (PG14)

`docker-compose.pglogical.yml`, `pglogical/Dockerfile`. The package `postgresql-14-pglogical`
(2.4.8) installs from the PGDG repository already configured in the official Debian image: no extra repository.

Extra configuration required: `shared_preload_libraries=pglogical`, `wal_level=logical`,
`track_commit_timestamp=on`, `pglogical.conflict_resolution=last_update_wins` (this is the default) **and**,
on this PostgreSQL 14.24 minor release, `output_plugin_libraries=pglogical_output`; without it the subscription
never starts: `could not create replication slot on provider: ERROR:  library "pglogical_output" may not be used as an output plugin`.

Moving parts, counted per flow (statement types run to get it working):

| | core PG18 | pglogical PG14 |
|---|---|---|
| server settings | `wal_level` (plus tuning) | `shared_preload_libraries`, `wal_level`, `track_commit_timestamp`, `output_plugin_libraries` on this minor |
| extension | none | `CREATE EXTENSION pglogical` on every node |
| node registration | none | `pglogical.create_node` on every node |
| hub -> worker | `CREATE PUBLICATION` (filter + columns in the DDL) + `CREATE SUBSCRIPTION` | `create_replication_set`, `replication_set_add_table` (per table, per set, filter + columns as arguments), `create_subscription` |
| worker -> hub | publication + subscription per worker | replication set + `create_subscription` per worker |
| conflict handling | error + `SKIP` by hand; `confl_*` counters | automatic policy, log lines only |
| into a partitioned target | works | see below |

pglogical observations (`results/70..74`):
- Row filter and column list work (`pglogical.replication_set_table.set_row_filter`, `set_att_list`).
  A worker only sees its slice. **Moving a row out of a filter is different from core**: the UPDATE of customer 3
  from worker 1 to worker 2 was not applied on either side: worker 1 still has customer 3 with `worker_id = 1`
  (stale), worker 2 never got it. Core logical replication turned the same UPDATE into a DELETE plus an INSERT.
- **Partitioned parent as target crashes**: applying replicated changes into a partitioned parent table made the
  apply worker die with `terminated by signal 11: Segmentation fault`; the postmaster then restarts all backends and the
  subscription crash-loops until dropped (initial copy into the parent worked). `make pglogical-crash` reproduces it
  (`sql/79-...`). The hub keeps crash-looping until the subscription is dropped, which can only succeed between
  two crashes and is attempted for 30 s only: run `make clean` afterwards. The session that runs the step is
  killed by the crash, so this one `.out` is not byte-for-byte reproducible (which statement prints the
  connection-lost messages varies); it is excluded from the identical-runs claim.
  The variant therefore names each worker's table `usage_events_w<N>` and replicates straight into the matching
  partition of the hub's `usage_events`; queries on the parent work.
- Conflicts with `last_update_wins` (commit timestamps decide), key inserted on both sides:
  local row older -> `CONFLICT: remote INSERT on relation public.usage_events_w1 (local index usage_events_w1_pkey). Resolution: apply_remote.`
  local row newer -> same line with `Resolution: keep_local.` The second case leaves the two nodes with different
  values for the same key (hub 2, worker 1) and no error anywhere: convergence needs the policy to run on both sides.
- Object name uniqueness: `create_subscription` names are part of the slot name on the provider, so the same
  subscription name on two workers against one hub collides here too (`replication slot ... already exists`).

## Files

```
docker-compose.yml             hub + worker1..3 (+ worker4, profile), postgres:18
docker-compose.pglogical.yml   pghub, pgw1, pgw2 on postgres:14 + pglogical
pglogical/Dockerfile
Makefile  run.sh  compare.sh
lib/lr-helpers.sql             test scaffolding (see above)
sql/01..22-*.sql               core steps      sql/70..74,79-*.sql   pglogical steps
results/*.out                  psql output      results/timings.txt   measured durations
```

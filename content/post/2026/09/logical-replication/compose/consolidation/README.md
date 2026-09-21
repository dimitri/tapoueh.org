# Consolidation: many databases into one, then re-export as CDC

Three source apps (`shop`, `crm`, `billing`, one PostgreSQL container each) are
consolidated into one `warehouse` database with logical replication, and the
warehouse re-exports the consolidated changes as a CDC stream for a
Debezium-like consumer (`pg_recvlogical`, `test_decoding`, `pgoutput`), also from
a physical standby.

Tested with the `postgres:18` image: PostgreSQL 18.6 (Debian 18.6-1.pgdg13+2,
aarch64), `pg_recvlogical` 18.6. Compose project `lrcons`, host ports 5720-5724.

## Reproduce (three commands)

```
make clean     # remove containers, volumes and results/*.out
make up        # start shop, crm, billing, warehouse and wait until pg_isready answers over TCP
make run       # execute sql/NN-*.sql and sql/NN-*.sh in order, write results/NN-*.out
```

`make down` stops the containers, `./run.sh 6` runs only the steps whose name
starts with `6` (steps expect the state left by the previous ones).
A full run takes about 6 minutes.

## How it works

* `run.sh` runs `sql/*.sql` and `sql/*.sh` in lexical order. A `.sql` file starts
  with `-- @service: <container>` (optional `-- @db: <database>`,
  `-- @nosync`); it is piped to `psql -X -a` inside that container. A `.sh` file
  is sourced with `lib.sh` loaded (`sq`, `qt`, `wait_until`, `wait_file`, `sync_all`).
* No fixed sleeps. Before each step, `sync_all` polls until every subscription has
  finished its initial copy and reached the publisher's current WAL position.
  Other waits poll a condition (`pg_stat_subscription_stats`, `pg_locks`, a file
  content, `pg_replication_slots`).
* `results/NN-*.out` is the raw `psql -X -a` / tool output, with volatile values
  rewritten by `normalize` in `lib.sh`: LSNs to `<LSN>`, timestamps, pids, xids,
  subscription oids in origin/slot names. Queries were written to avoid printing
  counters that depend on timing (booleans instead of `apply_error_count`).
* Reproducibility check: two runs from `make clean` produced byte-identical
  `results/` (`diff -r` clean).
* Auth: sources use a `repl` role (REPLICATION, `pg_read_all_data`) with a password;
  each subscription on the warehouse is owned by a regular role `sub_<source>`
  (member of `pg_create_subscription`), not by a superuser.
* `docker-compose.yml`: `wal_level=logical` everywhere, `logical_decoding_work_mem=64kB`
  (so a 20000-row transaction streams), `warehouse_standby` is a sleeping container in
  profile `standby`: step 67 runs `pg_basebackup` into it and starts postgres with `pg_ctl`.

## Steps and what they show

| Step | Shows |
|---|---|
| 01-05 | Naming constraint: same `schema.table` on two sources into one target table. Initial copy fails with `duplicate key value violates unique constraint "customers_pkey"` (`DETAIL: Key (id)=(1) already exists.`), a different shape fails with `logical replication target relation "public.contacts" is missing replicated column: "company"`. The table stays in state `d`, retried every 5 s, the first source's data is untouched. |
| 10-12 | The three apps, each with its own schema (`shop`, `crm`, `billing`). |
| 20-21 | Layout (a): one schema per source, one database, one subscription per source. Cross-source joins work. |
| 22-24 | Layout (b): one database per source. No cross-database queries (`cross-database references are not implemented`), one slot and one apply worker per source database, and slots are per database (CDC needs one slot per database). |
| 25-35 | Layout (c): shared table with an extra `source` column. 27-29 trigger and identity probe, 30 overlapping keys, 31-35 disjoint keys (what works). |
| 40-47 | Filtering: column list (no PII on the subscriber), row filter, `FOR TABLES IN SCHEMA` limits, what UPDATE of filtered / filter columns produces. |
| 50-52 | Consolidated read model (materialized view: revenue per account), stale until refreshed after replication. |
| 60-67 | CDC re-export: publication, slots, visibility of applied rows, origins, transaction boundaries, streaming of large transactions, decoding on a standby. |
| 70-72 | What breaks: DDL on the source, fix order, sequences. |

## Findings, with the evidence file

### Naming and layouts (01-24)
* Subscriptions map `schema.table` to the same name on the target, no renaming.
  Two sources with the same table name cannot share a target table unless the
  data is disjoint and the shape identical (`04-naming-errors.out`).
* (a) schema-per-source: works, one query joins everything, the recommended
  layout. It needs distinct schema names at the source (or a schema rename at
  the source; there is no way to rename on the subscriber side).
* (b) database-per-source: no renaming problem at all (same names everywhere),
  but nothing can be joined without postgres_fdw/dblink; 3 apply workers, 3 CDC
  slots needed. Every source also streams once per subscription (`24-layout-b-slots.out`
  shows two slots per source while (a) and (b) coexist).

### Layout (c), shared table stamped with the source (25-35)
* Extra subscriber columns with a DEFAULT work: the apply worker (and the
  initial copy) evaluate the default for the column the publisher does not send.
* Default trigger state does NOT fire in apply workers (`session_replication_role = replica`):
  `pg_trigger.tgenabled = 'O'`, nothing stamped (`28-c-trigger-state.out`).
  `ENABLE ALWAYS` fires, also during the initial copy (tablesync worker).
* Identity seen inside an `ENABLE ALWAYS` trigger (`29-c-identity-matrix.out`):
  `session_user` is always the subscription owner; `current_user` is the table
  owner with `run_as_owner = false` (the default) and the subscription owner
  with `run_as_owner = true`, also when the owner is a superuser.
  `pg_replication_origin_session_is_setup()` is `true`. `application_name` is
  empty. A role-level GUC (`ALTER ROLE sub_shop SET lr.source = 'shop'`) is
  visible. `pg_stat_subscription` joined on `pg_backend_pid()` gives the
  subscription name and oid (origin name is `pg_<subid>`, `pg_<subid>_<relid>`
  in a tablesync worker). `pg_stat_activity` shows the worker only when
  `current_user` is the subscription owner.
* Origin NAME: no function returns it for the current session.
  `pg_replication_origin_status` has only `local_id, external_id, remote_lsn, local_lsn`
  (no pid). Matching `pg_replication_origin_session_progress(false)` with
  `remote_lsn` was unreliable in exploratory runs (matched for some rows, not others);
  it is not part of the recorded output.
* Privileges: `pg_replication_origin_session_is_setup`, `_session_progress` and
  `pg_show_replication_origin_status` are superuser-only by default. A trigger
  calling them makes the apply worker fail with `permission denied for function ...`
  until EXECUTE is granted to the table owner.
* Recommended stamping: `source text NOT NULL DEFAULT regexp_replace(session_user, '^sub_', '')`
  with one owner role per subscription (`34-c-disjoint-target.out`,
  `35-c-disjoint-dml.out`). No trigger needed. Weakness: a superuser-owned
  subscription stamps `postgres`; a local write stamps the local user.
* The blocker is UPDATE and DELETE, not stamping. With overlapping ids the
  subscriber key must be `(source, id)`, but the publisher does not send `source`:
  `ERROR: publisher did not send replica identity column expected by the logical replication target relation "public.customers"`
  (apply retries forever). `REPLICA IDENTITY FULL` on the subscriber does not help.
  With `REPLICA IDENTITY FULL` on the publishers and a plain unique index on
  `(id, source)`, apply does not fail: it silently skips the change,
  `LOG: conflict detected on relation "public.customers": conflict=update_missing`
  (`DETAIL: Could not find the row to be updated.`), because the local row has
  `source` set and the remote tuple has NULL there. `pg_stat_subscription_stats.confl_update_missing`
  counts it (`30-c-overlap.out`). A partitioned target list-partitioned on `source` failed too
  (`no partition of relation "customers" found for row`, exploratory run, not in the recorded steps).
* So: the shared table works only when keys are disjoint across sources
  (`PRIMARY KEY (id)`, tested in 32-35). With overlapping keys nothing on the
  subscriber side is reliable; either use layout (a) plus a `UNION ALL` view that adds a constant
  `source` column, or add a real key-namespace column at the source (not tested here).

### Filtering (40-47)
* The subscriber table has no `email`/`phone`; the publisher's `pgoutput` messages
  contain no PII bytes (`44-filter-dml.out`, with a positive control that shows the
  same check detects the phone without a column list). The initial copy honours the list and the row filter.
* A row filter on a column that is not in the replica identity makes UPDATE
  fail at the SOURCE: `ERROR: cannot update table "orders"`, `DETAIL: Column used in the publication WHERE expression is not part of the replica identity.`
  Fix used: unique index `(id, tenant)` as `REPLICA IDENTITY USING INDEX`.
* UPDATE of a column outside the column list: still sends an UPDATE message (`BRUC`),
  no PII inside, the subscriber row is unchanged. UPDATE of the filter column
  eu->us arrives as DELETE (`BRDC`), us->eu as INSERT (`BRIC`); changes on rows
  outside the filter are not sent.
* Filters do not clean up: the US order copied before the filter was added stays on the
  subscriber until deleted by hand (43, 50). Same for emails already copied
  when `crm` moved from schema publication to column list (47).
* `FOR TABLES IN SCHEMA` picks up new tables automatically but refuses column lists:
  `Column lists cannot be specified in publications containing FOR TABLES IN SCHEMA elements.`
  The subscriber still needs the table and `REFRESH PUBLICATION`.

### CDC re-export (60-67)
* (i) Rows applied by subscription workers ARE decoded from the warehouse slot.
  The PII columns are absent end to end (`63-cdc-visible.out`).
* (ii) Applied transactions carry the subscription's replication origin.
  `test_decoding` `only-local=1` and `pgoutput` `origin 'none'` drop them: only the
  local write on the warehouse remains. `origin 'any'` (the default) emits an `O`
  message with `pg_<subid>` before each applied transaction (`64-cdc-origin.out`,
  `65-cdc-recvlogical.out`). A consumer that sets `origin=none` sees nothing of the consolidated data.
* (iii) A source transaction touching two tables arrives as ONE downstream transaction
  (`63-cdc-visible.out`, folded table). Separate source transactions stay separate.
* (iv) With 64kB `logical_decoding_work_mem` and a 20000-row source transaction, the
  warehouse subscription applies in a parallel worker (`substream = p`) and the downstream slot with
  `stream-changes=1` receives `opening a streamed block` / `streaming change for transaction`
  BEFORE the source commits, while the rows are still invisible on the warehouse. Commit
  ends with `committing streamed transaction`; a source rollback ends with
  `aborting streamed (sub)transaction` after thousands of changes were already delivered.
  A consumer must handle stream abort (`66-cdc-streaming.out`). Only test_decoding was checked for streaming.
* (v) Standby decoding works (`67-cdc-standby.out`). Requirements actually hit:
  1. The standby refuses to start with default sizing:
     `FATAL: recovery aborted because of insufficient parameter settings` /
     `DETAIL: max_worker_processes = 8 is a lower setting than on the primary server, where its value was 24.`
  2. wal_level is not inherited: `ERROR: logical decoding requires "wal_level" >= "logical"`
     when creating the slot on the standby, until the standby itself runs `wal_level=logical`.
  3. Creating the slot blocks while the primary is idle; `SELECT pg_log_standby_snapshot()` on the primary unblocks it.
  4. `hot_standby_feedback = off`: after catalog cleanup on the primary the slot is invalidated:
     `ERROR: can no longer access replication slot "cdc_sb"` / `DETAIL: This replication slot has been invalidated due to "rows_removed".`
     (`conflicting = t`). The primary's own logical slots hide this, so they are dropped first in the demo.
  5. With `hot_standby_feedback = on` the primary's physical slot `standby1` holds a `catalog_xmin`
     and the slot survives the same catalog vacuum.
  `primary_slot_name = 'standby1'` came from `pg_basebackup -R -S standby1 -C`; running without it was not tested.

### What breaks (70-72)
* Publisher-first `ALTER TABLE ... ADD COLUMN` (publication without a column list):
  `ERROR: logical replication target relation "shop.orders" is missing replicated column: "note"`;
  the worker restarts every 5 s, later transactions queue behind it and the slot
  keeps unconfirmed WAL. Adding the column on the subscriber fixes it without
  intervention. Order: additive changes subscriber first, then publisher; the reverse
  for drops (publisher first; the subscriber column stays NULL) (`71-ddl-error.out`).
* Sequence values are not replicated: the publisher sequence is at 40013, the
  warehouse sequence is `1, is_called = f`; a local insert fails with
  `duplicate key value violates unique constraint "orders_pkey"` (`72-ddl-sequences.out`).
* `ALTER SUBSCRIPTION ... SKIP (lsn = ...)` (used in step 30) is the way out of a
  transaction that can never apply.

## Files

`docker-compose.yml`, `Makefile`, `run.sh`, `lib.sh`, `init/warehouse-hba.sh`
(allows `pg_basebackup`), `sql/` (steps), `results/` (recorded output).

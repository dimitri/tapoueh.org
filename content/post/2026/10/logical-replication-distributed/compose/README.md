# Part 4 demo — compose/

Pinned to real commits, matching the pattern parts 1 through 3 already use
(`docker-compose.yml` + profile-gated containers + `run-*.sh` + `results/`).
Profiles **a**, **c**, **d**, **e** and **f** have been built and run; **b** isn't
scaffolded yet.

## Images build from source, not from a release

- **pg_auto_failover**, pinned to `8ed16056dfe13be3d912cc582c055cf8ecf9e803`
  on `main` — the commit that merges
  [PR #1197](https://github.com/hapostgres/pg_auto_failover/pull/1197),
  `synchronized_standby_slots()` and the rest of the CDC-continuity work
  this post's first section is about. Last tagged release is `v2.2`, which predates it. Image builds clean
  (`pgaf:demo`).
- **pgcopydb**, pinned to `84bce26e38d09801fb845ba22830e830e55490b1` on
  `main`, which includes
  [DDL replication over logical decoding](https://github.com/dimitri/pgcopydb/pull/1064)
  and its apply fix. Not in a tagged release yet.

Re-pin before running this again — `main` moves.

## Profiles

- **a** — pg_auto_failover failover-slot continuity. Monitor, primary and
  standby on Postgres 17, a `pg_recvlogical` consumer on a `failover = true`
  slot, a writer inserting numbered rows. `./run-a.sh <scenario>` with
  `planned`, `kill`, `kill-lag-gated` or `kill-lag-ungated`; it prints
  duplicates, holes, rows the consumer received that the new primary lacks,
  and how long writer and consumer were silent. `reset-a.sh` tears
  everything down. Summaries of three runs of each scenario are in
  `results/a-*-run*.out`.
- **b** — `synchronous_commit` modes under a simulated cross-region RTT.
  **Not scaffolded.** `[TODO 2]` in the post.
- **c** — four primaries (`us-east`, `us-west`, `in`, `au`), row-filtered
  cascades, one warehouse. **Run, three times.** The first two runs found
  and fixed a real echo-loop bug (the warehouse's own mirror riding back
  out through `us-east`'s unfiltered publication) — see
  `results/c-*.out`. The third run is clean: 8000 rows on every node,
  exactly 2000 per region, zero apply/sync errors anywhere. `[TODO 3]`
  in the post is closed.
- **d** — pgcopydb DDL-as-a-message. `run-d.sh` starts `clone --follow`,
  installs the event trigger mid-stream, runs an `ALTER TABLE` on the
  source and polls the target for the new column. Success is
  `got_column=1`.
  `reset-d.sh` must run between runs: it also drops the `pgcopydb`
  publication on the target, which clone's schema copy leaves behind.

- **f** — the pg_auto_failover group of profile `a` plus a plain Postgres `y`
  standing for a second region, linked in both directions by native logical
  replication (`sub_y_from_x` with `failover = true` and a multi-host
  connection string, `sub_x_from_y` on the group's primary). `./run-f.sh`
  kills the primary under load on both sides and compares what both regions
  hold afterwards. Three runs in `results/f-failover-run*.out`.
  `run-a.sh` also captures `pg_autoctl show state`, `show settings`,
  `show synchronized-standby-slots` and a `pg_autoctl watch` frame
  (`results/a-*-captures.out`); on the pinned pg_auto_failover build `watch`
  needs `--group 0`, without it it loops on "no nodes found in group -1".
- **e** — pglogical 2.4.8 on Postgres 18.6, three nodes. `./run-e.sh findings`
  re-runs Part 1's partitioned-parent crash and filter-move findings;
  `./run-e.sh concurrent` has two nodes update the same 1,000 keys under
  each conflict-resolution policy (`SECS` sets the duration). `reset-e.sh`
  recreates the containers. Results in `results/e-*.out`.

## Running it

```
docker compose --profile a --profile d --profile e build   # monitor, pgcopydb, e-hub
COMPOSE_PROFILES=c docker compose up -d --wait
./run-c.sh                                          # four-primary demo
./reset-d.sh && COMPOSE_PROFILES=d docker compose up -d --wait
./run-d.sh                                          # DDL-message demo
./run-a.sh planned                                  # failover drill (also: kill, kill-lag-gated, kill-lag-ungated)
./run-f.sh                                          # two regions, failover in one
./reset-e.sh && ./run-e.sh findings && ./run-e.sh concurrent
```

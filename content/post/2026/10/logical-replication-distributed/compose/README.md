# Part 4 demo — compose/

Pinned to real commits, matching the pattern parts 1 through 3 already use
(`docker-compose.yml` + profile-gated containers + `run-*.sh` + `results/`).
Profiles **c** and **d** have been built and run; **a** is built
but not yet run as a full failover drill; **b** isn't scaffolded yet.

## Images build from source, not from a release

- **pg_auto_failover**, pinned to `8ed16056dfe13be3d912cc582c055cf8ecf9e803`
  on `main` — the commit that merges
  [PR #1197](https://github.com/hapostgres/pg_auto_failover/pull/1197),
  `synchronized_standby_slots()` and the rest of the CDC-continuity work
  this post's first section is about. **Merged** 2026-10-14. Last tagged
  release is `v2.2`, which predates it. Image builds clean
  (`pgaf:demo`).
- **pgcopydb**, pinned to `84bce26e38d09801fb845ba22830e830e55490b1` on
  `main` — the merge of [PR #1069](https://github.com/dimitri/pgcopydb/pull/1069),
  which fixes the apply stall found in
  [PR #1064](https://github.com/dimitri/pgcopydb/pull/1064) ("replicate
  DDL over logical decoding as a typed message", merged 2026-10-16,
  `ffb41bd`). The stall reproduced on `febf16b`, `251a03a` and `ffb41bd`.

Re-pin before running this again — `main` moves.

## Profiles

- **a** — pg_auto_failover failover-slot continuity. **Built** (`pgaf:demo`),
  **not yet run** as a full monitor+primary+standby+failover drill — the
  service definitions are in `docker-compose.yml`, the actual
  `pg_autoctl perform failover` timing/duplicate/hole measurement is still
  `[TODO 1]` in the post.
- **b** — `synchronous_commit` modes under a simulated cross-region RTT.
  **Not scaffolded.** `[TODO 2]` in the post.
- **c** — four primaries (`us-east`, `us-west`, `in`, `au`), row-filtered
  cascades, one warehouse. **Run, three times.** The first two runs found
  and fixed a real echo-loop bug (the warehouse's own mirror riding back
  out through `us-east`'s unfiltered publication) — see
  `results/c-*.out`. The third run is clean: 8000 rows on every node,
  exactly 2000 per region, zero apply/sync errors anywhere. `[TODO 3]`
  in the post is closed.
- **d** — pgcopydb DDL-as-a-message, against the real code of PR #1064.
  **Run on five commits.** On `febf16b`, `251a03a` and `ffb41bd` the DDL
  never reached the target: receive, storage and transform were correct
  (`results/d-ddl-message-trace*.out`), but the apply connection went idle
  with `pg_replication_origin_session_setup()` as its last query, and
  fifty later `INSERT`s never landed either
  (`results/d-pipeline-stall-evidence.out`). `patch/ld_apply.c` is the
  diagnostic patch that showed `pgsql_execute()` returning success first.
  Root cause: the apply connection is in libpq pipeline mode, where
  `pgsql_execute()` only queues, and libpq does not write to the socket
  below 64kB without a sync, so a small transaction committed mid-stream
  never left the client.
  [PR #1069](https://github.com/dimitri/pgcopydb/pull/1069) flushes the
  pipeline after every `COMMIT`. On `84bce26`, `run-d.sh` ends with
  `got_column=1` after 2s, and 50 further `INSERT`s land (70 rows on both
  sides, `results/d-fix-verified-84bce26.out`).
  Cleanup note: `reset-d.sh` must also drop the `pgcopydb` publication on
  the target, which clone's schema copy leaves behind.

TODO 4 (re-running part 1's three pglogical findings against PG18) isn't
a new profile — it reuses part 1's own `compose/`, pointed at a current
PG18 build, which is why it isn't scaffolded again here.

## Running it

```
docker compose build pgaf pgcopydb                 # profiles a, d
COMPOSE_PROFILES=c docker compose up -d --wait
./run-c.sh                                          # four-primary demo
./reset-d.sh && COMPOSE_PROFILES=d docker compose up -d --wait
./run-d.sh                                          # DDL-message demo (got_column=1 on 84bce26)

# the diagnostic build that found the root cause above:
docker build --build-arg PGVERSION=18 -t pgcopydb:debug -f Dockerfile.pgcopydb-debug .
```

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
  this post's first section is about. Last tagged release is `v2.2`, which predates it. Image builds clean
  (`pgaf:demo`).
- **pgcopydb**, pinned to `84bce26e38d09801fb845ba22830e830e55490b1` on
  `main`, which includes
  [DDL replication over logical decoding](https://github.com/dimitri/pgcopydb/pull/1064)
  and its apply fix. Not in a tagged release yet.

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
- **d** — pgcopydb DDL-as-a-message. `run-d.sh` starts `clone --follow`,
  installs the event trigger mid-stream, runs an `ALTER TABLE` on the
  source and polls the target for the new column. Success is
  `got_column=1`.
  `reset-d.sh` must run between runs: it also drops the `pgcopydb`
  publication on the target, which clone's schema copy leaves behind.

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
```

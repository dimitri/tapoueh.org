# Part 4 demo — compose/

Pinned to real commits, matching the pattern parts 1 through 3 already use
(`docker-compose.yml` + profile-gated containers + `run-*.sh` + `results/`).
Profiles **c** and **d** have actually been built and run; **a** is built
but not yet run as a full failover drill; **b** isn't scaffolded yet.

## Images build from source, not from a release

- **pg_auto_failover**, pinned to `8ed16056dfe13be3d912cc582c055cf8ecf9e803`
  on `main` — the commit that merges
  [PR #1197](https://github.com/hapostgres/pg_auto_failover/pull/1197),
  `synchronized_standby_slots()` and the rest of the CDC-continuity work
  this post's first section is about. **Merged** 2026-10-14. Last tagged
  release is `v2.2`, which predates it. Image builds clean
  (`pgaf:demo`).
- **pgcopydb**, pinned to `ffb41bdd95c1e79b9ec208d8e910ee205bb8a59d` on
  `main` — the merge of [PR #1064](https://github.com/dimitri/pgcopydb/pull/1064),
  "replicate DDL over logical decoding as a typed message." **Merged**
  2026-10-16. Tested against three commits on the way to this one
  (`febf16b`, the force-pushed `251a03a`, now the merge) and found the
  same apply-side bug against all three.

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
- **d** — pgcopydb DDL-as-a-message, against PR #1064's real code, not
  the design doc. **Run three times**, against three different commits
  (`febf16b`, `251a03a`, and now the merge, `ffb41bd`). Same symptom
  all three times: receive, storage, and transform are verified correct
  by querying pgcopydb's own SQLite state directly (`results/
  d-ddl-message-trace*.out`) — the message lands with the right prefix
  and envelope, resolves to a hashed, storable DDL statement, correctly
  tagged with the real transaction xid — and apply never executes it.
  The retest against `251a03a` also caught a cleanup bug in this post's
  *own* `reset-d.sh` (a leftover `pgcopydb` publication on the target
  from a prior run's own schema dump, now fixed) that briefly looked
  like the real bug and wasn't.

  **Root-caused**, not just reproduced, on the merge commit:
  `compose/patch/ld_apply.c` is a one-line diagnostic patch (kept, not
  deleted, as the actual evidence) confirming `pgsql_execute()` for the
  DDL statement returns success — and the apply connection then never
  does anything else. `pg_stat_activity` on the target shows it idle,
  `pg_replication_origin_session_setup()` as the last query it ever
  ran, through fifty unrelated `INSERT`s issued on the source
  afterward that never land either (`results/d-pipeline-stall-evidence.out`).
  The apply connection runs in libpq pipeline mode, where "queued" and
  "server-confirmed" are different things; `pgsql_execute()` returning
  true here means the former, not the latter. `[TODO]` in the post asks
  for a fix against this specific finding, not a re-description of the
  symptom.

TODO 4 (re-running part 1's three pglogical findings against PG18) isn't
a new profile — it reuses part 1's own `compose/`, pointed at a current
PG18 build, which is why it isn't scaffolded again here.

## Running it

```
docker compose build pgaf pgcopydb                 # profiles a, d
COMPOSE_PROFILES=c docker compose up -d --wait
./run-c.sh                                          # four-primary demo
./reset-d.sh && COMPOSE_PROFILES=d docker compose up -d --wait
./run-d.sh                                          # DDL-message demo (currently fails at apply)

# the diagnostic build that found the root cause above:
docker build --build-arg PGVERSION=18 -t pgcopydb:debug -f Dockerfile.pgcopydb-debug .
```

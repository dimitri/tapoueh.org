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
- **pgcopydb**, two separate pins:
  - `main` at `140977231d667035a13a59c58c17a958e0c68ff5`, used by profile
    **c** (doesn't need the DDL-message work).
  - [PR #1064](https://github.com/dimitri/pgcopydb/pull/1064)'s branch,
    `251a03a79db2c9b42648f6b949becff2493ab8bb` (force-pushed over the
    original `febf16b...`), used by profile **d** — **open, not merged**,
    the real implementation of the `case 'M':` dispatch this post's
    last section designs, not a stand-in for it. The force-push fixed
    a real, different bug (apply not flushing its libpq pipeline
    before reporting `replay_lsn` at cutover); re-verified against it
    on fresh volumes and the DDL-apply bug below is unchanged.

Re-pin both before running this again — `main` moves, and #1064 may have
landed by the time anyone reads this.

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
- **d** — pgcopydb DDL-as-a-message, against PR #1064's real branch, not
  the design doc. **Run twice**, against two different commits on the
  PR. Receive, storage, and transform are verified correct both times
  by querying pgcopydb's own SQLite state directly (`results/
  d-ddl-message-trace.out`, `results/d-ddl-message-trace-retest.out`):
  the message lands with the right prefix and envelope, resolves to a
  hashed, storable DDL statement, correctly tagged with the surrounding
  transaction's real xid. Apply does not execute it, either time —
  `replay_lsn` advances past the message with no error logged, and the
  target never gets the column. The retest, on fully fresh volumes,
  also caught a cleanup bug in this post's *own* `reset-d.sh` (a
  leftover `pgcopydb` publication on the target, from a prior run's own
  schema dump, now fixed) that briefly looked like it might be the real
  bug and wasn't. Real, reproducible, still not root-caused to an exact
  line. `[TODO]` in the post now asks for that line, not for the demo.

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
```

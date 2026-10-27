# Part 4 demo — compose/

Same pattern as parts 1 through 3: `docker-compose.yml` with profile-gated
containers, `run-*.sh` scripts, `results/`. Profiles **a**, **c**, **d**,
**e** and **f** have been built and run; **b** isn't scaffolded yet.

## One group per region

Every region is one pg_auto_failover group of its own: a monitor and three
Postgres nodes, `<region>-monitor`, `<region>-a`, `<region>-b`, `<region>-c`.
The groups share no failover; they are connected by logical replication only.
The nodes of a region carry its name as their `region` label (informational
in pg_auto_failover: it affects no failover or quorum decision). The
regions are `us-east`, `us-west`, `ap-south`, `ap-southeast` and `eu-west`
(the Amsterdam warehouse). `gen-regions.py` generates their services in
`docker-compose.yml`; `lib-regions.sh` has the helpers the scripts share.
The nodes keep their data on tmpfs, so removing the containers is the reset.

## Images build from source, not from a release

- **pg_auto_failover**, pinned to `8ed16056dfe13be3d912cc582c055cf8ecf9e803`
  on `main` — the commit that merges
  [PR #1197](https://github.com/hapostgres/pg_auto_failover/pull/1197),
  `synchronized_standby_slots()` and the rest of the CDC-continuity work the
  post's first section is about. The last tagged release is `v2.2`, which
  predates it. The image is built by the `us-east-monitor` service and
  tagged `lrdist-pgaf:17`.
- **pgcopydb**, pinned to `84bce26e38d09801fb845ba22830e830e55490b1` on
  `main`, which includes
  [DDL replication over logical decoding](https://github.com/dimitri/pgcopydb/pull/1064)
  and its apply fix. Not in a tagged release yet.

Re-pin before running this again — `main` moves.

## Profiles

- **a** — failover-slot continuity in one region, `us-east`. A
  `pg_recvlogical` consumer on a `failover = true` slot, a writer inserting
  numbered rows. `./run-a.sh <scenario>` with `planned`, `kill`,
  `kill-lag-gated` (both standbys are cut off from the primary's WAL before
  the kill) or `kill-lag-ungated` (the same, with
  `synchronized_standby_slots` emptied). It prints duplicates, holes, rows the
  consumer received that the new primary lacks, how long writer and consumer
  were silent, and what `pg_autoctl show state`, `show settings`,
  `show synchronized-standby-slots` and `watch` show before, during and
  after. Three runs of each scenario are in `results/a-*-run*.out`
  (`a-*-captures.out` are the first runs). On the pinned build `watch` needs
  `--group 0`; without it it loops on "no nodes found in group -1".
- **b** — `synchronous_commit` modes under a simulated cross-region RTT.
  **Not scaffolded.** `[TODO]` in the post.
- **c** — five regions: four writers with equal primaries, row-filtered
  cascades, and the `eu-west` warehouse. `./run-c.sh` creates the
  publications and the failover-enabled subscriptions (each connection
  string lists every node of the publishing group), writes 2,000 rows in each
  of the four regions at once, and counts: 8,000 rows on every primary and
  standby, 2,000 per region on the warehouse, zero apply or sync errors. Three
  runs in `results/c-run*.out`.
- **d** — pgcopydb DDL-as-a-message. `run-d.sh` starts `clone --follow`,
  installs the event trigger mid-stream, runs an `ALTER TABLE` on the source
  and polls the target for the new column. Success is `got_column=1`.
  `reset-d.sh` must run between runs: it also drops the `pgcopydb`
  publication on the target, which clone's schema copy leaves behind.
- **e** — pglogical 2.4.8 on Postgres 18.6, three nodes. `./run-e.sh findings`
  re-runs Part 1's partitioned-parent crash and filter-move findings;
  `./run-e.sh concurrent` has two nodes update the same 1,000 keys under each
  conflict-resolution policy (`SECS` sets the duration). `reset-e.sh`
  recreates the containers. Results in `results/e-*.out`.
- **f** — two regions, `us-east` and `us-west`, each a group of its own,
  replicating to each other with native logical replication
  (`sub_us_west_from_us_east` and `sub_us_east_from_us_west`, both
  `failover = true`, both through every node of the other group), a writer
  on each side. `./run-f.sh east` kills `us-east-a` under load,
  `./run-f.sh west` kills `us-west-a`, and the script compares what both
  regions hold afterwards. Three runs of each in
  `results/f-failover-*-run*.out`.

## Running it

```
docker compose --profile c build us-east-monitor    # lrdist-pgaf:17, profiles a, c, f
docker compose --profile d --profile e build        # pgcopydb, e-hub
./run-c.sh                                          # five regions, 20 containers
./run-a.sh planned                                  # also: kill, kill-lag-gated, kill-lag-ungated
./run-f.sh east                                     # also: west
./run-d.sh                                          # after ./reset-d.sh && COMPOSE_PROFILES=d docker compose up -d --wait
./reset-e.sh && ./run-e.sh findings && ./run-e.sh concurrent
```

Profile `c` runs 20 containers and wants a few GB of memory. A group's
standbys joining at the same moment can leave the primary with a short
`synchronized_standby_slots` (seen on the pinned build); `region_wait` in
`lib-regions.sh` detects it and makes the keeper re-read the monitor's value.

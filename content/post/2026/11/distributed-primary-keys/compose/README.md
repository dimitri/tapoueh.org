# Distributed primary keys: lab

The scripts of the sequence access method section use the `pk-seqam` image
(`docker build -t pk-seqam compose/seqam`, and `pk-seqam-ft` from
`Dockerfile.faketime`). Every other script starts throwaway `postgres:19rc1` containers on tmpfs (and one
`postgres:9.6`, one `postgres:17`, and `postgres:13` and `postgres:17` for the
SQL `uuidv7()`), and removes them at the end. `lib.sh` has the helpers.
Results are in `../results/`, named after the script that made them.

| Script | What it runs | Results |
|---|---|---|
| `run-single.sh <variant> [run]`, `run-single-all.sh` | five keys, with and without the companion index (`+`): 4 writers, 5,000,000 rows, then 500,000 rows after a checkpoint | `30-single-*.out`, `summarize-single.py` makes `30-single-summary.txt` |
| `run-child.sh` | 1,000,000 parents and 5,000,000 children under each key | `95-child.out` |
| `run-mesh.sh <variant>` | three regions writing at once, each subscribed to the other two | `80-mesh-*.out` |
| `run-failover.sh <insync\|lagging> [run]` | a standby promoted after the primary is killed, and what its sequence hands out | `70-failover-*.out` |
| `run-skew.sh`, `skew.sql` | `uuidv7(interval)` as a skewed clock, three regions, six situations | `90-skew.out` |
| `run-filter.sh` | a uuid key under a row-filtered publication, with and without the companion index | `50-filter-uuid.out` |
| `run-widen.sh <publisher-first\|subscriber-first>` | `int` to `bigint` under logical replication | `60-widen-*.out` |
| `run-serial-old.sh` | a `serial` column made on 9.6, dumped and restored on 19rc1 | `40-serial-old.out` |
| `run-default.sh` | the default `uuidv7()` of a column, dumped from 17 and restored on 19rc1 | `45-uuidv7-default.out` |
| `run-seqam-single-all.sh` (`run-single.sh` with `snowone`, `snowam`) | the one-server workload on the build with the sequence access method patches (`seqam/Dockerfile`, v29 on the master of 2026-05-14): the keys again, the expression on one shared sequence, the `snowflake` method | `31-seqam-*.out`, `summarize-single.py ../results 31-seqam` makes `31-seqam-summary.txt` |
| `extension/` (at the article's root) | `regionseq`, our own sequence access method as a PGXS extension: C, control and SQL files, a regression test (`make installcheck`) and its README. `compose/seqam/Dockerfile` builds it out of tree into the patched server: `docker build -t pk-seqam -f compose/seqam/Dockerfile .` from the article's directory | `100-seqam-regress.out` (the patches' suites) |
| `run-seqam-func.sh` | the `snowflake` method: ids, `int` columns, `MAXVALUE`, dump and restore, a crash | `101-seqam-func.out` (regression suites: `100-seqam-regress.out`) |
| `run-seqam-scale.sh` | `nextval()` alone, 1, 4 and 16 clients on one sequence | `105-seqam-scale.out` |
| `run-failover.sh <insync\|lagging> [run] snowam\|snowflake` | the failover test for the method and for the time-based `bigint` | `102-failover-snowam-*.out`, `106-failover-snowflake-*.out` |
| `run-mesh.sh snowam\|snowam-same` | the mesh with a machine number per region, and with the default everywhere | `103-mesh-*.out` |
| `run-clock.sh <variant> <step> <rate>` | a key made from the clock, with the clock stepped back (`seqam/Dockerfile.faketime`) | `104-clock-step*.out` |
| `run-regionseq-func.sh` | `regionseq`: library not preloaded, region number unset, a crash with the clock set back 10 s, dump from region 2 and restore into region 3 | `110-regionseq-func.out` |
| `run-region-filter.sh <regionseq\|expression>` | a publication filtered on the bits of the key, replica identity the primary key, with inserts, updates and deletes of two regions | `111-region-filter-*.out` |
| `run-region-filter-sql.sh` | the same filter with the SQL the article prints: region 1 in the bits of the key, 1,000 rows of region 1 and of region 2, 100 updates and 50 deletes of each | `141-region-filter-sql.out` |
| `run-snowflake-default.sh` | the article's time, region, counter `bigint` as a column default (region from `app.region`, origin in UTC), against the same bits inlined in each INSERT as the other scripts do: same ids, same sizes, same speed | `140-snowflake-default.out` |
| `run-single.sh regionam\|snowone1\|snowam1`, `run-failover.sh <s> [run] regionam`, `run-mesh.sh regionam\|regionam-same`, `run-clock.sh regionam ...`, `run-seqam-scale.sh` | `regionseq` and its like-for-like references (one region for all four writers) on the same tests | `31-seqam-*.out`, `112-failover-regionam-*`, `113-mesh-*`, `104-clock-step*.out`, `114-*`, `105-seqam-scale.out` |
| `run-pgaf-drill.sh <key> <skew> [run]` | a region under pg_auto_failover (monitor, two nodes, synchronous standby), a subscriber with a failover slot, a writer on a two-host URI, the primary killed, node 2's clock `skew` s behind; `CKPT=default` keeps the default checkpoint settings (a 270 s promotion), `KEEP=1` keeps the containers; `SWITCHES=N` does N switchovers in a row (`SWPAUSE` seconds apart, `PACE` seconds between two batches of the writer; `132-pgaf-20-switchovers.out`); `MODE=switchover` does a `pg_autoctl perform switchover` instead of killing the primary (`131-pgaf-switchover.out`). Image: `compose/seqam/Dockerfile.pgaf` | `130-pgaf-drill.out` |
| `run-failover-skew.sh <key> <insync\|lagging> <skew> [run]` | a promoted standby whose clock is behind by `skew` seconds: ids repeated and ids below the old ones, for the expression, `uuidv7()` and `regionseq` | `120-failover-skew.out` |
| `run-clock-order.sh <key> <step> [run]` | the clock stepped back while four clients insert: ids lower than an earlier one (same client, any client), time in the id ahead of the clock | `121-clock-order.out` |
| `run-failover-skew.sh regionseq ...` after the timeline bits | the same failover, `regionseq` only: no repeat in any line | `123-failover-skew-timeline.out`, `124-clock-order-regionseq.out` |
| `run-mesh-skew.sh <key> <skew>` | the three-region mesh with clocks 0, +skew, -skew: convergence, index size and leaf density | `122-mesh-skew.out` |
| `run-rest.sh`, `run-rest2.sh` | the order in which the results above were produced | |

The 32-bit wall (`../sql/10-serial-limits.sql`), the detection query
(`../sql/11-narrow-columns.sql`) and the SQL `uuidv7()` (`../sql/20-*.sql`,
`../sql/21-*.sql`) run in a plain `psql` session.

The machine is a laptop with six cores and an 8 GB Docker VM. Rates and times
are for comparing the keys with each other, not absolute figures.

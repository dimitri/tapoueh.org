# Distributed primary keys: lab

Every script starts throwaway `postgres:19rc1` containers on tmpfs (and one
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
| `run-rest.sh`, `run-rest2.sh` | the order in which the results above were produced | |

The 32-bit wall (`../sql/10-serial-limits.sql`), the detection query
(`../sql/11-narrow-columns.sql`) and the SQL `uuidv7()` (`../sql/20-*.sql`,
`../sql/21-*.sql`) run in a plain `psql` session.

The machine is a laptop with six cores and an 8 GB Docker VM. Rates and times
are for comparing the keys with each other, not absolute figures.

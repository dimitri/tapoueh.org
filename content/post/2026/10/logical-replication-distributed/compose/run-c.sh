#!/usr/bin/env bash
# Profile c: four regions with equal primaries, row-filtered cascades, one warehouse.
# Every region is a pg_auto_failover group of its own (a monitor and three nodes).
# Builds the topology "Cascading, one hop downstream" and "The architecture that
# makes the question moot" describe, end to end, and checks the result with
# concurrent writers in all four regions.
set -uo pipefail
cd "$(dirname "$0")"
. ./lib-regions.sh
. ./lib-topology.sh
export COMPOSE_PROFILES=c

REGIONS="us-east us-west ap-south ap-southeast eu-west"
mkdir -p results
OUT=results/c-$(date +%Y%m%d-%H%M%S).out
say() { echo "$@" | tee -a "$OUT"; }
: > "$OUT"
region_reset $REGIONS
say "== five groups: $(echo $REGIONS | wc -w) monitors, $(( $(echo $REGIONS | wc -w) * 3 )) nodes =="
region_up $REGIONS || { say "did not come up"; exit 1; }
for r in $REGIONS; do region_wait $r || { say "$r never converged"; exit 1; }; region_label $r; done
for r in $REGIONS; do say "   $r: primary $(region_primary $r)"; done

say "== schema =="
topo_schema $REGIONS

say "== publications, row-filtered on region =="
topo_publications $REGIONS ap-south-to-ap-southeast

say "== subscriptions: through every node of the publishing group, failover-enabled =="
topo_subscriptions

say "== waiting for the initial table sync of every subscription =="
topo_wait_sync $REGIONS
sleep 5

say "== concurrent writers: 2000 rows each, four regions in parallel =="
write() { rpq "$(region_primary "$1")" "insert into events (region, event_id, payload) select '$1', g, md5(g::text) from generate_series(1, 2000) g" >/dev/null; }
write us-east & write us-west & write ap-south & write ap-southeast & wait
say "== settling =="
sleep 10

{
  echo "=== row counts on every primary (expect 8000 everywhere once fully mirrored) ==="
  for r in $REGIONS; do echo "$r: $(pq $r "select count(*) from events")"; done
  echo
  echo "=== per-region row counts on the warehouse (expect 2000 each) ==="
  pq eu-west "select region || ': ' || count(*) from events group by region order by region"
  echo
  echo "=== the standbys hold the same rows (physical replication), eu-west and us-east shown ==="
  for n in eu-west-b eu-west-c us-east-b us-east-c; do echo "$n: $(rpq $n 'select count(*) from events')"; done
  echo
  echo "=== apply/sync error counts, every primary ==="
  echo "(pg_stat_subscription_stats' per-conflict-type columns, confl_insert_exists"
  echo " and friends, are PG18+; this demo runs PG17, so apply_error_count and"
  echo " sync_error_count are what is available.)"
  for r in $REGIONS; do
    echo "-- $r --"
    pq $r "select subname || ': apply_errors=' || apply_error_count || ' sync_errors=' || sync_error_count from pg_stat_subscription_stats order by 1"
  done
} | tee -a "$OUT"

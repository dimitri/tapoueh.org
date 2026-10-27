#!/usr/bin/env bash
# Profile c: four regions with equal primaries, row-filtered cascades, one warehouse.
# Every region is a pg_auto_failover group of its own (a monitor and three nodes).
# Builds the topology "Cascading, one hop downstream" and "The architecture that
# makes the question moot" describe, end to end, and checks the result with
# concurrent writers in all four regions.
set -uo pipefail
cd "$(dirname "$0")"
. ./lib-regions.sh
export COMPOSE_PROFILES=c

REGIONS="us-east us-west ap-south ap-southeast eu-west"
mkdir -p results
OUT=results/c-$(date +%Y%m%d-%H%M%S).out
say() { echo "$@" | tee -a "$OUT"; }
: > "$OUT"
P() { echo "$1" | tr - _; }          # us-east -> us_east
pq() { rpq "$(region_primary "$1")" "$2"; }   # run sql on a region's primary

region_reset $REGIONS
say "== five groups: $(echo $REGIONS | wc -w) monitors, $(( $(echo $REGIONS | wc -w) * 3 )) nodes =="
region_up $REGIONS || { say "did not come up"; exit 1; }
for r in $REGIONS; do region_wait $r || { say "$r never converged"; exit 1; }; region_label $r; done
for r in $REGIONS; do say "   $r: primary $(region_primary $r)"; done

say "== schema =="
for r in $REGIONS; do pq $r "$(cat sql/c-01-schema.sql)" >/dev/null; done

say "== publications, row-filtered on region =="
# pub_to_eu_west must stay row-filtered to this region's own regions: left unfiltered,
# the warehouse's own mirror, relayed back down to this region, would ride straight back
# out to the warehouse the moment it lands, an echo loop.
pq us-east      "create publication pub_to_eu_west for table events where (region = 'us-east' or region = 'us-west')"
pq us-east      "create publication pub_to_us_west for table events where (region <> 'us-west')"
pq us-west      "create publication pub_to_us_east for table events where (region = 'us-west')"
pq ap-south     "create publication pub_to_eu_west for table events where (region = 'ap-south' or region = 'ap-southeast')"
pq ap-south     "create publication pub_to_ap_southeast for table events where (region <> 'ap-southeast')"
pq ap-southeast "create publication pub_to_ap_south for table events where (region = 'ap-southeast')"
pq eu-west      "create publication pub_to_us_east for table events where (region <> 'us-east' and region <> 'us-west')"
pq eu-west      "create publication pub_to_ap_south for table events where (region <> 'ap-south' and region <> 'ap-southeast')"

say "== subscriptions: through every node of the publishing group, failover-enabled =="
# a subscription is named by both endpoints: the replication slot it creates on the
# publisher carries its name, and must be unique there
sub() { # subscriber publisher publication
  pq "$1" "create subscription sub_$(P $1)_from_$(P $2) connection '$(rconninfo $2)' publication $3 with (failover = true)" >/dev/null
}
sub us-west      us-east      pub_to_us_west
sub us-east      us-west      pub_to_us_east
sub ap-southeast ap-south     pub_to_ap_southeast
sub ap-south     ap-southeast pub_to_ap_south
sub eu-west      us-east      pub_to_eu_west
sub us-east      eu-west      pub_to_us_east
sub eu-west      ap-south     pub_to_eu_west
sub ap-south     eu-west      pub_to_ap_south

say "== waiting for the initial table sync of every subscription =="
for i in $(seq 1 60); do
  pending=0
  for r in $REGIONS; do
    n=$(pq $r "select count(*) from pg_subscription_rel where srsubstate <> 'r'")
    pending=$((pending + ${n:-1}))
  done
  [ "$pending" = 0 ] && break; sleep 1
done
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

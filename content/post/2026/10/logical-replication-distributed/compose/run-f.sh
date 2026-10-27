#!/usr/bin/env bash
# Profile f: two regions, us-east and us-west, each a pg_auto_failover group of its
# own (a monitor and three nodes). They replicate to each other with native logical
# replication, each owning the rows of its own region key. The primary of one region
# is killed under load: what happens to the slot that region publishes, and to the
# subscription it runs from the other region?
#
#   ./run-f.sh east     kill us-east-a (us-west reads from us-east)
#   ./run-f.sh west     kill us-west-a
set -uo pipefail
cd "$(dirname "$0")"
. ./lib-regions.sh
export COMPOSE_PROFILES=f

SCN=${1:?east | west}
case "$SCN" in
  east) V=us-east; O=us-west ;;
  west) V=us-west; O=us-east ;;
  *) echo "east | west"; exit 2 ;;
esac
OUT=results/f-failover-$SCN.out
say() { echo "$@" | tee -a "$OUT"; }
: > "$OUT"
cl() { dc exec -T client "$@"; }
q() { cl psql "$(ruri "$1")" -X -At -c "$2"; }     # against whichever node is the region's primary

./reset-f.sh >/dev/null
say "== two regions, each a pg_auto_failover group: the primary of $V is killed =="
region_up us-east us-west || { say "did not come up"; exit 1; }
dc up -d client >/dev/null 2>&1
for r in us-east us-west; do
  region_wait $r || { say "$r never converged"; exit 1; }
  region_label $r
done
say "   us-east primary: $(region_primary us-east), us-west primary: $(region_primary us-west)"

SCHEMA="create table events (region text not null, id bigint not null, applied_at timestamptz not null default clock_timestamp(), primary key (region, id))"
for r in us-east us-west; do
  q $r "$SCHEMA" >/dev/null
  q $r "create publication pub_${r//-/_} for table events (region, id) where (region = '$r')" >/dev/null
done
# each region subscribes to the other through every node of the group; the slot a
# subscription creates on the publisher is failover-enabled, so the standbys keep a copy
q us-west "create subscription sub_us_west_from_us_east connection '$(rconninfo us-east)' publication pub_us_east with (copy_data = false, failover = true)" >/dev/null 2>&1
q us-east "create subscription sub_us_east_from_us_west connection '$(rconninfo us-west)' publication pub_us_west with (copy_data = false, failover = true)" >/dev/null 2>&1

say "-- slots on each primary, and what its standbys hold"
for r in us-east us-west; do
  P=$(region_primary $r)
  rpq $P "select slot_name, slot_type, failover from pg_replication_slots where slot_type = 'logical' order by 1" | sed "s/^/   $P: /" | tee -a "$OUT"
  say "   $P: synchronized_standby_slots = $(rpq $P 'show synchronized_standby_slots')"
done
for r in us-east us-west; do
  for n in $r-b $r-c; do
    for i in $(seq 1 90); do
      s=$(rpq $n "select synced and not temporary from pg_replication_slots where slot_type = 'logical' limit 1" 2>/dev/null); [ "$s" = t ] && break; sleep 1
    done
    say "   $n: synced copy of the failover slot: $s (after ${i}s)"
  done
done

cl rm -f /tmp/f-stop
dc exec -d client sh -c "/traffic/f-writer.sh '$(ruri us-east)' us-east /results/f-writer-east.log"
dc exec -d client sh -c "/traffic/f-writer.sh '$(ruri us-west)' us-west /results/f-writer-west.log"
sleep 15
say "-- steady state: us-east holds $(q us-east "select count(*) from events where region = 'us-east'") own rows and $(q us-east "select count(*) from events where region = 'us-west'") from us-west"
OLD=$(region_primary $V)
T0=$(dc exec -T $V-monitor date +%s.%N | tr -d '\r')
say "-- t0=$T0: docker kill $OLD"
dc kill $OLD >/dev/null 2>&1

for i in $(seq 1 300); do
  NEW=$(rmon $V "select nodename from pgautofailover.node where nodename <> '$OLD' and reportedstate in ('primary','wait_primary','single') order by nodeid limit 1" 2>/dev/null)
  [ -n "$NEW" ] && break; sleep 1
done
say "   new primary of $V: $NEW, $(rmon $V "select round((extract(epoch from min(eventtime)) - $T0)::numeric, 1) from pgautofailover.event where nodeid = (select nodeid from pgautofailover.node where nodename = '$NEW') and reportedstate in ('wait_primary','primary') and extract(epoch from eventtime) > $T0") s after t0"
sleep 90
cl touch /tmp/f-stop
sleep 3

say "-- waiting for both regions to converge"
conv=0
for i in $(seq 1 180); do
  a=$(q us-east "select count(*) from events where region = 'us-east'" 2>/dev/null); b=$(q us-west "select count(*) from events where region = 'us-east'" 2>/dev/null)
  c=$(q us-east "select count(*) from events where region = 'us-west'" 2>/dev/null); d=$(q us-west "select count(*) from events where region = 'us-west'" 2>/dev/null)
  [ -n "$a" ] && [ "$a" = "$b" ] && [ "$c" = "$d" ] && { conv=1; break; }
  sleep 1
done
say "   converged: $conv after ${i}s  (us-east rows on us-east/us-west: $a/$b, us-west rows: $c/$d)"

ids() { q "$1" "select id from events where region = '$2' order by id"; }   # node-region, row-region
ids us-east us-east > results/f-east-on-east.ids; ids us-west us-east > results/f-east-on-west.ids
ids us-east us-west > results/f-west-on-east.ids; ids us-west us-west > results/f-west-on-west.ids
say; say "== result =="
for r in east west; do
  acked=$(wc -l < results/f-writer-$r.log)
  say "rows owned by us-$r: acknowledged $acked, on us-east $(wc -l < results/f-$r-on-east.ids), on us-west $(wc -l < results/f-$r-on-west.ids)"
  say "   missing from us-east: $(comm -13 results/f-$r-on-east.ids results/f-$r-on-west.ids | wc -l | tr -d ' ')   missing from us-west: $(comm -23 results/f-$r-on-east.ids results/f-$r-on-west.ids | wc -l | tr -d ' ')"
  say "   acknowledged but nowhere: $(awk '{print $1}' results/f-writer-$r.log | sort | comm -23 - <(sort results/f-$r-on-east.ids results/f-$r-on-west.ids | uniq) | wc -l | tr -d ' ')"
done
say; say "-- apply errors, per subscription"
for r in us-east us-west; do
  say "   on $r: $(q $r "select subname || ' apply_errors=' || apply_error_count || ' sync_errors=' || sync_error_count from pg_stat_subscription_stats")"
done
say "-- longest silence between consecutive rows, per region key and database (applied_at, each database's own clock)"
GAP="select region, round(extract(epoch from max(gap))::numeric, 1) from (select region, applied_at - lag(applied_at) over (partition by region order by id) as gap from events) g group by region order by 1"
for r in us-east us-west; do q $r "$GAP" | sed "s/^/   on $r: /" | tee -a "$OUT"; done
say "-- slots now"
for r in us-east us-west; do
  P=$(region_primary $r)
  rpq $P "select slot_name, slot_type, failover, active from pg_replication_slots where slot_type = 'logical' order by 1" | sed "s/^/   $P: /" | tee -a "$OUT"
done

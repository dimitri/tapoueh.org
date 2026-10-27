#!/usr/bin/env bash
# Profile f: region X is a pg_auto_failover group (monitor, primary, standby),
# region Y is a plain Postgres. They replicate to each other with native logical
# replication, each owning its own rows (region column). The primary of X is killed
# under load; what happens to the slot X publishes to Y, and to the subscription X
# runs from Y?
set -uo pipefail
cd "$(dirname "$0")"
dc() { docker compose -p lrdist "$@"; }
cl() { dc exec -T a-client "$@"; }
OUT=results/f-failover.out
say() { echo "$@" | tee -a "$OUT"; }
mon() { dc exec -T monitor psql -X -At -h 127.0.0.1 -U postgres -d pg_auto_failover -c "$1"; }
px() { dc exec -T "$1" psql -X -At -h 127.0.0.1 -U postgres -d app -c "$2"; }
XURI="postgres://postgres@pg-a-primary:5432,pg-a-standby:5432/app?target_session_attrs=read-write&connect_timeout=2"
YURI="postgres://postgres@y:5432/app?connect_timeout=2"
: > "$OUT"

./reset-a.sh >/dev/null
dc rm -sfv y >/dev/null 2>&1
say "== two regions: X = pg_auto_failover group, Y = plain Postgres =="
COMPOSE_PROFILES=f dc up -d --wait monitor pg-a-primary y >/dev/null 2>&1 || { say "did not come up"; exit 1; }
COMPOSE_PROFILES=f dc up -d pg-a-standby a-client >/dev/null 2>&1
for i in $(seq 1 120); do
  n=$(mon "select count(*) from pgautofailover.node where reportedstate = goalstate and reportedstate in ('primary','secondary')" 2>/dev/null)
  [ "$n" = 2 ] && break; sleep 1
done
[ "$n" = 2 ] || { say "formation never converged"; exit 1; }
dc exec -T monitor pg_autoctl set node region --pgdata /data/pgdata --name node_1 us-east-a >/dev/null 2>&1
dc exec -T monitor pg_autoctl set node region --pgdata /data/pgdata --name node_2 us-east-b >/dev/null 2>&1

SCHEMA="create table events (region text not null, id bigint not null, applied_at timestamptz not null default clock_timestamp(), primary key (region, id))"
px pg-a-primary "$SCHEMA" >/dev/null
dc exec -T y psql -X -q -h 127.0.0.1 -U postgres -d app -c "$SCHEMA" >/dev/null
px pg-a-primary "create publication pub_x for table events (region, id) where (region = 'x')" >/dev/null
dc exec -T y psql -X -q -h 127.0.0.1 -U postgres -d app -c "create publication pub_y for table events (region, id) where (region = 'y')" >/dev/null
# Y subscribes to X through every node of the group; the slot on X is failover-enabled
dc exec -T y psql -X -q -h 127.0.0.1 -U postgres -d app -c "create subscription sub_y_from_x connection 'host=pg-a-primary,pg-a-standby port=5432 dbname=app user=postgres target_session_attrs=read-write connect_timeout=2' publication pub_x with (copy_data = false, failover = true)" >/dev/null
# X subscribes to Y; created on the primary, it lives in the catalogs the standby replicates
px pg-a-primary "create subscription sub_x_from_y connection 'host=y port=5432 dbname=app user=postgres connect_timeout=2' publication pub_y with (copy_data = false)" >/dev/null

say "-- slots on X's primary, and what the standby holds"
px pg-a-primary "select slot_name, slot_type, failover from pg_replication_slots order by 1" | sed 's/^/   primary: /' | tee -a "$OUT"
for i in $(seq 1 60); do
  r=$(px pg-a-standby "select synced and not temporary from pg_replication_slots where slot_name = 'sub_y_from_x'" 2>/dev/null); [ "$r" = t ] && break; sleep 1
done
px pg-a-standby "select slot_name, failover, synced, temporary from pg_replication_slots order by 1" | sed 's/^/   standby: /' | tee -a "$OUT"
say "   synchronized_standby_slots on the primary: $(px pg-a-primary 'show synchronized_standby_slots')"
say "   subscription on X: $(px pg-a-primary "select subname, subenabled from pg_subscription")"

cl rm -f /tmp/f-stop
dc exec -d a-client sh -c "/traffic/f-writer.sh '$XURI' x /results/f-writer-x.log"
dc exec -d a-client sh -c "/traffic/f-writer.sh '$YURI' y /results/f-writer-y.log"
sleep 15
say "-- steady state: X has $(px pg-a-primary "select count(*) from events where region='x'") x-rows and $(px pg-a-primary "select count(*) from events where region='y'") y-rows; Y has $(dc exec -T y psql -X -At -h 127.0.0.1 -U postgres -d app -c "select count(*) from events where region='x'") x-rows"
T0=$(dc exec -T monitor date +%s.%N | tr -d '\r')
say "-- t0=$T0: docker kill pg-a-primary"
dc kill pg-a-primary >/dev/null 2>&1

for i in $(seq 1 300); do
  r=$(mon "select reportedstate from pgautofailover.node where nodehost = 'pg-a-standby'" 2>/dev/null)
  case "$r" in primary|wait_primary|single) break ;; esac; sleep 1
done
say "   pg-a-standby is $r (monitor), $(mon "select round((extract(epoch from min(eventtime)) - $T0)::numeric, 1) from pgautofailover.event where nodeid = (select nodeid from pgautofailover.node where nodehost = 'pg-a-standby') and reportedstate in ('wait_primary','primary') and extract(epoch from eventtime) > $T0") s after t0"
sleep 90
cl touch /tmp/f-stop
sleep 3

say "-- waiting for both regions to converge"
conv=0
for i in $(seq 1 180); do
  a=$(px pg-a-standby "select count(*) from events where region='x'" 2>/dev/null)
  b=$(dc exec -T y psql -X -At -h 127.0.0.1 -U postgres -d app -c "select count(*) from events where region='x'" 2>/dev/null)
  c=$(px pg-a-standby "select count(*) from events where region='y'" 2>/dev/null)
  d=$(dc exec -T y psql -X -At -h 127.0.0.1 -U postgres -d app -c "select count(*) from events where region='y'" 2>/dev/null)
  [ -n "$a" ] && [ "$a" = "$b" ] && [ "$c" = "$d" ] && { conv=1; break; }
  sleep 1
done
say "   converged: $conv after ${i}s  (x-rows X/Y: $a/$b, y-rows X/Y: $c/$d)"

ids() { # node region
  if [ "$1" = y ]; then dc exec -T y psql -X -At -h 127.0.0.1 -U postgres -d app -c "select id from events where region = '$2' order by id"
  else px pg-a-standby "select id from events where region = '$2' order by id"; fi
}
ids x x > results/f-x-on-x.ids; ids y x > results/f-x-on-y.ids
ids x y > results/f-y-on-x.ids; ids y y > results/f-y-on-y.ids
say; say "== result =="
for r in x y; do
  acked=$(wc -l < results/f-writer-$r.log)
  say "region $r: acknowledged $acked, on X $(wc -l < results/f-$r-on-x.ids), on Y $(wc -l < results/f-$r-on-y.ids)"
  say "   missing from X: $(comm -13 results/f-$r-on-x.ids results/f-$r-on-y.ids | wc -l | tr -d ' ')   missing from Y: $(comm -23 results/f-$r-on-x.ids results/f-$r-on-y.ids | wc -l | tr -d ' ')"
  say "   acknowledged but nowhere: $(awk '{print $1}' results/f-writer-$r.log | sort | comm -23 - <(sort results/f-$r-on-x.ids results/f-$r-on-y.ids | uniq) | wc -l | tr -d ' ')"
done
say; say "-- apply errors and subscription state"
say "   on X (new primary): $(px pg-a-standby "select subname || ' apply_errors=' || apply_error_count || ' sync_errors=' || sync_error_count from pg_stat_subscription_stats")"
say "   on Y: $(dc exec -T y psql -X -At -h 127.0.0.1 -U postgres -d app -c "select subname || ' apply_errors=' || apply_error_count || ' sync_errors=' || sync_error_count from pg_stat_subscription_stats")"
say "-- longest silence between consecutive rows, per database (applied_at, each database's own clock)"
GAP="select region, round(extract(epoch from max(gap))::numeric, 1) as longest_gap_s from (select region, applied_at - lag(applied_at) over (partition by region order by id) as gap from events) g group by region order by 1"
px pg-a-standby "$GAP" | sed 's/^/   on X: /' | tee -a "$OUT"
dc exec -T y psql -X -At -h 127.0.0.1 -U postgres -d app -c "$GAP" | sed 's/^/   on Y: /' | tee -a "$OUT"
say "-- slots now"
px pg-a-standby "select slot_name, slot_type, failover, active from pg_replication_slots order by 1" | sed 's/^/   X: /' | tee -a "$OUT"

#!/usr/bin/env bash
# Profile i: the five-region system of profile c, under load.
#
#   ./run-i.sh cascade   writers in the four regions; the primary of us-east, the hub
#                        us-west's rows and the Asian rows travel through, is killed; once
#                        every region has converged, a column is added on all five with a
#                        short pause of the writers
#   ./run-i.sh join      four regions holding 300,000 rows and taking writes; ap-southeast
#                        joins with a first copy, and writes keep flowing meanwhile
#
# RUN=<n> names the output results/i-<scenario>-run<n>.out
set -uo pipefail
cd "$(dirname "$0")"
. ./lib-regions.sh
. ./lib-topology.sh
export COMPOSE_PROFILES=i

SCN=${1:?cascade | join}
mkdir -p results
OUT=results/i-$SCN${RUN:+-run$RUN}.out
say() { echo "$@" | tee -a "$OUT"; }
: > "$OUT"
cl() { dc exec -T client "$@"; }
q() { cl psql "$(ruri "$1")" -X -At -c "$2"; }     # against whichever node is the region's primary
now() { date +%s.%N; }

start_writer() { # region first-id
  dc exec -d client sh -c "/traffic/i-writer.sh '$(ruri $1)' $1 /tmp/i-writer-$1.log ${2:-1}"
}
acked() { cl sh -c "wc -l < /tmp/i-writer-$1.log" | tr -d ' \r'; }
# ids_digest <node-region> <row-region>: count and md5 of the ids a region's primary holds for a region key
holds() { q "$1" "select count(*) || ' ' || coalesce(md5(string_agg(event_id::text, ',' order by event_id)), '-') from events where region = '$2'"; }
max_gap() { # region: longest pause between two acknowledged rows, in seconds
  cl sh -c "awk 'NR>1 {g=\$2-p; if (g>m) m=g} {p=\$2} END{printf \"%.1f\", m}' /tmp/i-writer-$1.log"
}

wait_converged() { # region... : every primary holds, for every region key, what the writers were acknowledged
  local deadline=$(( $(date +%s) + ${TIMEOUT:-300} )) ok r n want
  while [ "$(date +%s)" -lt "$deadline" ]; do
    ok=1
    for r in $WRITERS; do
      want=$(( ${PRELOAD:-0} + $(acked $r) ))
      for n in $ALL; do
        [ "$(q $n "select count(*) from events where region = '$r'" 2>/dev/null)" = "$want" ] || { ok=0; break; }
      done
      [ $ok = 1 ] || break
    done
    [ $ok = 1 ] && return 0
    sleep 2
  done
  return 1
}

report_matrix() { # what every region holds of every region key, against what was acknowledged
  local r n want
  for r in $WRITERS; do
    want=$(( ${PRELOAD:-0} + $(acked $r) ))
    line="rows owned by $r: acknowledged $want; held by"
    for n in $ALL; do line="$line $n=$(q $n "select count(*) from events where region = '$r'")"; done
    say "   $line"
  done
  say "   digests of the ids per region key, identical on all five primaries?"
  for r in $WRITERS; do
    d=$(for n in $ALL; do holds $n $r | cut -d' ' -f2; done | sort -u | wc -l | tr -d ' ')
    say "   $r: $d distinct digest(s) across $(echo $ALL | wc -w) primaries"
  done
}

errors() {
  for r in $ALL; do
    say "   $r: $(q $r "select string_agg(subname || ' apply_errors=' || apply_error_count || ' sync_errors=' || sync_error_count, '; ') from pg_stat_subscription_stats")"
  done
}

bring_up() { # region...
  region_reset "$@"
  region_up "$@" || { say "did not come up"; exit 1; }
  local r
  for r in "$@"; do region_wait $r || { say "$r never converged"; exit 1; }; region_label $r; done
  for r in "$@"; do say "   $r: primary $(region_primary $r)"; done
}

case "$SCN" in
cascade)
  ALL="us-east us-west ap-south ap-southeast eu-west"
  WRITERS="us-east us-west ap-south ap-southeast"
  dc rm -sf client >/dev/null 2>&1
  say "== five groups =="
  bring_up $ALL
  dc up -d client >/dev/null 2>&1
  topo_schema $ALL
  topo_publications $ALL ap-south-to-ap-southeast
  topo_subscriptions
  topo_wait_sync $ALL || say "table sync did not finish"
  sleep 5
  cl rm -f /tmp/i-stop /tmp/i-pause
  say "== writers in the four regions, about 25 rows a second each =="
  for r in $WRITERS; do start_writer $r; done
  sleep 20
  say "-- steady state: us-west holds $(q us-west "select count(*) from events where region = 'ap-southeast'") rows of ap-southeast, two hops away (ap-southeast, eu-west, us-east, us-west)"

  # sample, once a second, what the two rows farthest from the hub have reached
  SAMPLES=$(mktemp)
  ( while [ ! -e "$SAMPLES.stop" ]; do
      echo "$(date +%s) $(q eu-west "select count(*) from events where region = 'us-west'" 2>/dev/null) $(q us-west "select count(*) from events where region = 'ap-southeast'" 2>/dev/null)" >> "$SAMPLES"
      sleep 1
    done ) &
  SAMPLER=$!

  OLD=$(region_primary us-east)
  T0=$(dc exec -T us-east-monitor date +%s.%N | tr -d '\r')
  say "-- t0=$T0: docker kill $OLD, the primary of us-east, which us-west and eu-west read from and which feeds both of them"
  dc kill $OLD >/dev/null 2>&1
  for i in $(seq 1 300); do
    NEW=$(rmon us-east "select nodename from pgautofailover.node where nodename <> '$OLD' and reportedstate in ('primary','wait_primary','single') order by nodeid limit 1" 2>/dev/null)
    [ -n "$NEW" ] && break; sleep 1
  done
  say "   new primary of us-east: $NEW, $(rmon us-east "select round((extract(epoch from min(eventtime)) - $T0)::numeric, 1) from pgautofailover.event where nodeid = (select nodeid from pgautofailover.node where nodename = '$NEW') and reportedstate in ('wait_primary','primary') and extract(epoch from eventtime) > $T0") s after t0"
  sleep 90
  cl touch /tmp/i-pause
  sleep 3
  touch "$SAMPLES.stop"; wait $SAMPLER 2>/dev/null
  say "== writers paused, waiting for every region to hold every acknowledged row =="
  Tp=$(now)
  if wait_converged; then say "   converged $(printf '%.0f' "$(echo "$(now) - $Tp" | bc)") s after the pause"; else say "   NOT converged after ${TIMEOUT:-300} s"; fi
  report_matrix
  say "-- apply and sync errors on the five primaries"
  errors
  # the longest time the rows farthest from the hub stood still, whatever the writers did meanwhile
  awk 'NR==1 {p2=$2; p3=$3; s2=$1; s3=$1; next}
       $2!=p2 {if ($1-s2>m2) m2=$1-s2; s2=$1; p2=$2}
       $3!=p3 {if ($1-s3>m3) m3=$1-s3; s3=$1; p3=$3}
       END {printf "   longest time the us-west rows stood still on eu-west: %d s; the ap-southeast rows on us-west: %d s\n", m2, m3}' "$SAMPLES" | tee -a "$OUT"
  say "-- the longest pause between two acknowledged rows, per writer"
  for r in $WRITERS; do say "   $r: $(max_gap $r) s"; done
  say "-- slots on the new primary of us-east"
  P=$(region_primary us-east)
  rpq $P "select slot_name, failover, active from pg_replication_slots where slot_type = 'logical' order by 1" | sed "s/^/   $P: /" | tee -a "$OUT"

  say "== a column added on all five primaries, writers paused =="
  Tw=$(now)
  for r in $ALL; do q $r "alter table events add column note text" >/dev/null; done
  say "   altered on every primary $(printf '%.1f' "$(echo "$(now) - $Tw" | bc)") s after the pause was confirmed, no row in flight"
  cl rm -f /tmp/i-pause
  sleep 30
  cl touch /tmp/i-stop
  sleep 3
  PRELOAD=0
  wait_converged && say "   converged again after 30 s of writes on the new schema" || say "   NOT converged after the change"
  report_matrix
  say "-- the column exists on: $(for r in $ALL; do echo -n "$r=$(q $r "select count(*) from information_schema.columns where table_name = 'events' and column_name = 'note'") "; done)"
  say "-- apply and sync errors after the change"
  errors
  ;;

join)
  PRE="us-east us-west ap-south eu-west"
  WRITERS="us-east us-west ap-south"
  ALL="$PRE"
  PRELOAD=${PRELOAD:-100000}
  dc rm -sf client >/dev/null 2>&1
  say "== four groups, $PRELOAD rows owned by each of three regions =="
  bring_up $PRE
  dc up -d client >/dev/null 2>&1
  topo_schema $PRE
  topo_publications $PRE
  # the subscriptions among the four regions present
  sub us-west      us-east      pub_to_us_west
  sub us-east      us-west      pub_to_us_east
  sub eu-west      us-east      pub_to_eu_west
  sub us-east      eu-west      pub_to_us_east
  sub eu-west      ap-south     pub_to_eu_west
  sub ap-south     eu-west      pub_to_ap_south
  topo_wait_sync $PRE || say "table sync did not finish"
  for r in $WRITERS; do
    q $r "insert into events (region, event_id, payload) select '$r', g, md5(g::text) from generate_series(1, $PRELOAD) g" >/dev/null &
  done
  wait
  Tp=$(now)
  for i in $(seq 1 600); do
    ok=1; for n in $PRE; do [ "$(q $n "select count(*) from events")" = $(( PRELOAD * 3 )) ] || ok=0; done
    [ $ok = 1 ] && break; sleep 2
  done
  say "   every one of the four primaries holds $(q eu-west "select count(*) from events") rows, $(printf '%.0f' "$(echo "$(now) - $Tp" | bc)") s after the bulk insert"
  say "   size of the table: $(q ap-south "select pg_size_pretty(pg_total_relation_size('events'))")"

  cl rm -f /tmp/i-stop /tmp/i-pause
  for r in $WRITERS; do start_writer $r $((PRELOAD + 1)); done
  sleep 15

  say "== ap-southeast joins =="
  Tj=$(now)
  bring_up ap-southeast
  say "   the new group is up $(printf '%.0f' "$(echo "$(now) - $Tj" | bc)") s later"
  topo_schema ap-southeast
  topo_publications ap-southeast ap-south-to-ap-southeast
  # ap-south serves a first copy of everything it holds to the new region, row-filtered
  Ts=$(now)
  WALS=$(mktemp)
  ( while [ ! -e "$WALS.stop" ]; do
      echo "$(date +%s) $(rpq "$(region_primary ap-south)" "select coalesce(max(pg_wal_lsn_diff(pg_current_wal_lsn(), restart_lsn)), 0)::bigint from pg_replication_slots where slot_type = 'logical'" 2>/dev/null)" >> "$WALS"
      sleep 1
    done ) &
  WSAMPLER=$!
  sub ap-southeast ap-south pub_to_ap_southeast true
  sub ap-south ap-southeast pub_to_ap_south false
  for i in $(seq 1 1800); do
    [ "$(q ap-southeast "select count(*) from pg_subscription_rel where srsubstate <> 'r'")" = 0 ] && [ "$(q ap-southeast "select count(*) from pg_subscription_rel")" != 0 ] && break; sleep 1
  done
  Tc=$(now)
  say "   first copy done $(printf '%.0f' "$(echo "$Tc - $Ts" | bc)") s after the subscription: ap-southeast holds $(q ap-southeast "select count(*) from events") rows"
  touch "$WALS.stop"; wait $WSAMPLER 2>/dev/null
  say "   most WAL any logical slot on the ap-south primary kept during the copy: $(sort -k2 -n "$WALS" | tail -1 | awk '{printf "%.0f MB", $2/1048576}')"
  ALL="us-east us-west ap-south ap-southeast eu-west"
  start_writer ap-southeast 1
  WRITERS="us-east us-west ap-south ap-southeast"
  sleep 30
  cl touch /tmp/i-pause
  sleep 3
  say "== writers paused, waiting for every region to hold every acknowledged row =="
  Tq=$(now)
  # the three preloaded regions carry PRELOAD rows; ap-southeast carries none
  wait_converged_join() {
    local deadline=$(( $(date +%s) + 300 )) ok r n want pre
    while [ "$(date +%s)" -lt "$deadline" ]; do
      ok=1
      for r in $WRITERS; do
        pre=$PRELOAD; [ $r = ap-southeast ] && pre=0
        want=$(( pre + $(acked $r) ))
        for n in $ALL; do [ "$(q $n "select count(*) from events where region = '$r'" 2>/dev/null)" = "$want" ] || { ok=0; break; }; done
        [ $ok = 1 ] || break
      done
      [ $ok = 1 ] && return 0; sleep 2
    done; return 1
  }
  if wait_converged_join; then say "   converged $(printf '%.0f' "$(echo "$(now) - $Tq" | bc)") s after the pause"; else say "   NOT converged"; fi
  for r in $WRITERS; do
    pre=$PRELOAD; [ $r = ap-southeast ] && pre=0
    line="rows owned by $r: acknowledged $((pre + $(acked $r))); held by"
    for n in $ALL; do line="$line $n=$(q $n "select count(*) from events where region = '$r'")"; done
    say "   $line"
  done
  for r in $WRITERS; do
    d=$(for n in $ALL; do holds $n $r | cut -d' ' -f2; done | sort -u | wc -l | tr -d ' ')
    say "   $r: $d distinct digest(s) of its ids across the five primaries"
  done
  say "-- the longest pause between two acknowledged rows, per writer, over the whole run including the join"
  for r in $WRITERS; do say "   $r: $(max_gap $r) s"; done
  say "-- apply and sync errors"
  errors
  cl touch /tmp/i-stop
  ;;
*) echo "cascade | join"; exit 2 ;;
esac

#!/usr/bin/env bash
# Profile a: a CDC consumer on a failover slot, across a pg_auto_failover failover.
# The region is us-east: one group of its own, a monitor and three nodes
# (us-east-a, us-east-b, us-east-c).
#
#   ./run-a.sh planned            pg_autoctl perform failover, synchronized_standby_slots on
#   ./run-a.sh kill               docker kill of the primary, synchronized_standby_slots on
#   ./run-a.sh kill-lag-gated     both standbys are cut off from the primary's WAL, then the
#                                 primary is killed; synchronized_standby_slots on
#   ./run-a.sh kill-lag-ungated   same, with synchronized_standby_slots emptied: the
#                                 misconfiguration the guarantee's conditions warn about
set -uo pipefail
cd "$(dirname "$0")"
. ./lib-regions.sh
export COMPOSE_PROFILES=a

SCN=${1:?scenario: planned | kill | kill-lag-gated | kill-lag-ungated}
case "$SCN" in planned|kill|kill-lag-gated|kill-lag-ungated) ;; *) echo "unknown scenario"; exit 2 ;; esac

R=us-east
URI=$(ruri $R)
OUT=results/a-$SCN.out
LOGPFX=/results/a-$SCN   # path inside the container
HP=results/a-$SCN         # same files on the host

say() { echo "$@" | tee -a "$OUT"; }
: > "$OUT"

./reset-a.sh >/dev/null
say "== scenario: $SCN =="
region_up $R || { say "the group did not come up"; exit 1; }
dc up -d client >/dev/null 2>&1

say "-- waiting for one primary and two secondaries"
region_wait $R || { say "the group never converged"; rmon $R "select nodename, reportedstate, goalstate from pgautofailover.node" | tee -a "$OUT"; exit 1; }
say "group ready"
region_label $R
OLD=$(region_primary $R)
say "   primary: $OLD"

snap() { # label: what the operator would see right now
  say; say "== $1 =="
  say "-- pg_autoctl show state"
  dc exec -T $R-monitor pg_autoctl show state --pgdata /data/pgdata 2>&1 | sed 's/^/   /' | tee -a "$OUT"
  say "-- pg_autoctl show synchronized-standby-slots"
  dc exec -T $R-monitor pg_autoctl show synchronized-standby-slots --pgdata /data/pgdata 2>&1 | sed 's/^/   /' | tee -a "$OUT"
  say "-- pg_autoctl watch (one frame)"
  watch_frame | tee -a "$OUT"
}
watch_frame() {
  dc exec -T $R-monitor sh -c 'tmux kill-server 2>/dev/null; tmux new-session -d -x 170 -y 15 "pg_autoctl watch --pgdata /data/pgdata --group 0"; sleep 3; tmux capture-pane -p; tmux kill-server' 2>&1 | sed 's/[[:space:]]*$//' | sed 's/^/   /'
}

rpq $OLD "create table events (id bigint primary key, ts timestamptz not null default clock_timestamp(), node inet)" >/dev/null
rpq $OLD "select pg_create_logical_replication_slot('cdc', 'test_decoding', false, false, true)" >/dev/null
snap "before the failover"
say; say "-- pg_autoctl show settings"
dc exec -T $R-monitor pg_autoctl show settings --pgdata /data/pgdata 2>&1 | sed 's/^/   /' | tee -a "$OUT"
say "-- settings on the primary"
for g in wal_level synchronized_standby_slots synchronous_standby_names synchronous_commit; do
  say "   $g = $(rpq $OLD "show $g")"
done
say "-- settings on the standbys"
for n in $R-b $R-c; do
  say "   $n: sync_replication_slots = $(rpq $n "show sync_replication_slots"), hot_standby_feedback = $(rpq $n "show hot_standby_feedback"), dbname in primary_conninfo: $(rpq $n "select current_setting('primary_conninfo') like '%dbname=%'")"
done

case "$SCN" in
  *ungated) rpq $OLD "alter system set synchronized_standby_slots = ''" >/dev/null
            rpq $OLD "select pg_reload_conf()" >/dev/null
            say "-- gate OFF: synchronized_standby_slots = '$(rpq $OLD "show synchronized_standby_slots")'" ;;
esac
WENV=""
case "$SCN" in kill-lag-*) WENV="PGOPTIONS='-c synchronous_commit=local'" ;; esac

dc exec -T client rm -f /tmp/a-stop /tmp/a-stop-consumer
dc exec -d client sh -c "/traffic/a-consumer.sh $LOGPFX-consumer.log"
dc exec -d client sh -c "$WENV /traffic/a-writer.sh $LOGPFX-writer.log"

say "-- waiting for both standbys to hold a synced copy of the slot"
synced=0
for i in $(seq 1 120); do
  ok=0
  for n in $R-b $R-c; do
    r=$(rpq $n "select synced and failover and not temporary and invalidation_reason is null from pg_replication_slots where slot_name = 'cdc'" 2>/dev/null)
    [ "$r" = t ] && ok=$((ok + 1))
  done
  [ "$ok" = 2 ] && { synced=1; break; }
  sleep 1
done
say "   slot synced on both standbys: $synced (after ${i}s)"
[ "$synced" = 1 ] || for n in $R-b $R-c; do rpq $n "select slot_name, synced, failover, temporary, invalidation_reason from pg_replication_slots" | tee -a "$OUT"; done
sleep 10
say "-- steady state: $(wc -l < $HP-writer.log) rows written, $(grep -c 'INSERT: id' $HP-consumer.log) delivered"

# ---- the trigger ----
case "$SCN" in
  kill-lag-*)
    for n in $R-b $R-c; do
      ip=$(dc exec -T $OLD getent hosts $n | awk '{print $1}')
      dc exec -T $OLD sh -c "sed -i '1i host replication all $ip/32 reject' /data/pgdata/pg_hba.conf"
    done
    rpq $OLD "select pg_reload_conf()" >/dev/null
    rpq $OLD "select pg_terminate_backend(pid) from pg_stat_replication where application_name like 'pgautofailover%'" >/dev/null
    say "-- both standbys cut off from the primary's WAL; writes continue with synchronous_commit=local"
    sleep 5
    say "   rows written so far: $(wc -l < $HP-writer.log), delivered to the consumer: $(grep -c 'INSERT: id' $HP-consumer.log)"
    say "   standbys have: $R-b $(rpq $R-b "select count(*) from events"), $R-c $(rpq $R-c "select count(*) from events")"
    ;;
esac
case "$SCN" in
  planned)
    T0=$(dc exec -T $R-monitor date +%s.%N | tr -d '\r')
    say "-- t0=$T0: pg_autoctl perform failover"
    dc exec -T $R-monitor pg_autoctl perform failover --pgdata /data/pgdata >/dev/null 2>&1 ;;
  *)
    T0=$(dc exec -T $R-monitor date +%s.%N | tr -d '\r')
    say "-- t0=$T0: docker kill $OLD"
    dc kill $OLD >/dev/null 2>&1 ;;
esac

case "$SCN" in planned) DURING=3 ;; *) DURING=25 ;; esac
say; say "== during the failover ($DURING s after t0) =="
sleep $DURING
watch_frame | tee -a "$OUT"
say "-- waiting for the monitor to report a new writable primary"
promoted=0
for i in $(seq 1 300); do
  NEW=$(rmon $R "select nodename from pgautofailover.node where nodename <> '$OLD' and reportedstate in ('primary','wait_primary','single') order by nodeid limit 1" 2>/dev/null)
  [ -n "$NEW" ] && { promoted=1; break; }
  sleep 1
done
say "   promoted: $promoted (new primary: $NEW)"
say "   monitor: $NEW reached wait_primary/primary at t0 +$(rmon $R "select round((extract(epoch from min(eventtime)) - $T0)::numeric, 1) from pgautofailover.event where nodeid = (select nodeid from pgautofailover.node where nodename = '$NEW') and reportedstate in ('wait_primary','primary') and extract(epoch from eventtime) > $T0") s"
sleep 15

dc exec -T client touch /tmp/a-stop
sleep 3
# let the consumer drain
for i in $(seq 1 30); do
  last=$(dc exec -T client psql "$URI" -X -At -c "select max(id) from events" 2>/dev/null)
  got=$(grep -c "INSERT: id\[bigint\]:$last " $HP-consumer.log)
  [ "${got:-0}" -ge 1 ] && break
  sleep 1
done
dc exec -T client touch /tmp/a-stop-consumer
dc exec -T client pkill pg_recvlogical >/dev/null 2>&1
sleep 1

dc exec -T client psql "$URI" -X -At -c "select id from events order by id" > $HP-db.ids 2>/dev/null
snap "after the failover"
say
say "== result: $SCN =="
perl analyze-a.pl "$T0" $HP-db.ids $HP-writer.log $HP-consumer.log | tee -a "$OUT"
say
say "== monitor events =="
rmon $R "select to_char(eventtime, 'HH24:MI:SS.MS'), nodename, reportedstate, goalstate, left(description, 90) from pgautofailover.event order by eventid" | sed 's/^/   /' | tail -25 | tee -a "$OUT"

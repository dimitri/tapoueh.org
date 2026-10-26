#!/usr/bin/env bash
# Profile a: a CDC consumer on a failover slot, across a pg_auto_failover failover.
#
#   ./run-a.sh planned            pg_autoctl perform failover, synchronized_standby_slots on
#   ./run-a.sh kill               docker kill of the primary, synchronized_standby_slots on
#   ./run-a.sh kill-lag-gated     the standby is cut off from the primary first, then the
#                                 primary is killed; synchronized_standby_slots on
#   ./run-a.sh kill-lag-ungated   same, with synchronized_standby_slots emptied: the
#                                 misconfiguration the guarantee's conditions warn about
set -uo pipefail
cd "$(dirname "$0")"
SCN=${1:?scenario: planned | kill | kill-lag-gated | kill-lag-ungated}
case "$SCN" in planned|kill|kill-lag-gated|kill-lag-ungated) ;; *) echo "unknown scenario"; exit 2 ;; esac

dc() { docker compose -p lrdist "$@"; }
cl() { dc exec -T a-client "$@"; }
URI="postgres://postgres@pg-a-primary:5432,pg-a-standby:5432/app?target_session_attrs=read-write&connect_timeout=2"
OUT=results/a-$SCN.out
LOGPFX=/results/a-$SCN   # path inside the container
HP=results/a-$SCN         # same files on the host
mon() { dc exec -T monitor psql -X -At -h 127.0.0.1 -U postgres -d pg_auto_failover -c "$1"; }
pgq() { dc exec -T "$1" psql -X -At -h 127.0.0.1 -U postgres -d app -c "$2"; }

say() { echo "$@" | tee -a "$OUT"; }
: > "$OUT"

./reset-a.sh >/dev/null
say "== scenario: $SCN =="
COMPOSE_PROFILES=a dc up -d --wait monitor pg-a-primary >/dev/null 2>&1 || { say "primary did not come up"; exit 1; }
COMPOSE_PROFILES=a dc up -d pg-a-standby a-client >/dev/null 2>&1

say "-- waiting for primary + secondary"
ready=0
for i in $(seq 1 120); do
  n=$(mon "select count(*) from pgautofailover.node where reportedstate = goalstate and reportedstate in ('primary','secondary')" 2>/dev/null)
  [ "$n" = 2 ] && { ready=1; break; }
  sleep 1
done
[ "$ready" = 1 ] || { say "formation never reached primary+secondary"; mon "select nodename, reportedstate, goalstate from pgautofailover.node" | tee -a "$OUT"; exit 1; }
say "formation ready after ${i}s"
mon "select nodename, reportedstate, goalstate from pgautofailover.node order by nodeid" | sed 's/^/   /' | tee -a "$OUT"

pgq pg-a-primary "create table events (id bigint primary key, ts timestamptz not null default clock_timestamp(), node inet)" >/dev/null
pgq pg-a-primary "select pg_create_logical_replication_slot('cdc', 'test_decoding', false, false, true)" >/dev/null
say "-- settings on the primary"
for g in wal_level synchronized_standby_slots synchronous_standby_names synchronous_commit; do
  say "   $g = $(pgq pg-a-primary "show $g")"
done
say "-- settings on the standby"
for g in sync_replication_slots hot_standby_feedback; do say "   $g = $(pgq pg-a-standby "show $g")"; done
say "   primary_conninfo has dbname: $(pgq pg-a-standby "select current_setting('primary_conninfo') like '%dbname=%'")"

case "$SCN" in
  *ungated) pgq pg-a-primary "alter system set synchronized_standby_slots = ''" >/dev/null
            pgq pg-a-primary "select pg_reload_conf()" >/dev/null
            say "-- gate OFF: synchronized_standby_slots = '$(pgq pg-a-primary "show synchronized_standby_slots")'" ;;
esac
WENV=""
case "$SCN" in kill-lag-*) WENV="PGOPTIONS='-c synchronous_commit=local'" ;; esac

cl rm -f /tmp/a-stop /tmp/a-stop-consumer
dc exec -d a-client sh -c "/traffic/a-consumer.sh $LOGPFX-consumer.log"
dc exec -d a-client sh -c "$WENV /traffic/a-writer.sh $LOGPFX-writer.log"

say "-- waiting for the standby to hold a synced copy of the slot"
synced=0
for i in $(seq 1 90); do
  r=$(pgq pg-a-standby "select synced and failover and not temporary and invalidation_reason is null from pg_replication_slots where slot_name = 'cdc'" 2>/dev/null)
  [ "$r" = t ] && { synced=1; break; }
  sleep 1
done
say "   slot synced on the standby: $synced (after ${i}s)"
[ "$synced" = 1 ] || pgq pg-a-standby "select slot_name, synced, failover, temporary, invalidation_reason from pg_replication_slots" | tee -a "$OUT"
sleep 10
say "-- steady state: $(wc -l < $HP-writer.log) rows written, $(grep -c 'INSERT: id' $HP-consumer.log) delivered"

# ---- the trigger ----
case "$SCN" in
  kill-lag-*)
    sbip=$(dc exec -T pg-a-primary getent hosts pg-a-standby | awk '{print $1}')
    dc exec -T pg-a-primary sh -c "sed -i '1i host replication all $sbip/32 reject' /data/pgdata/pg_hba.conf"
    pgq pg-a-primary "select pg_reload_conf()" >/dev/null
    pgq pg-a-primary "select pg_terminate_backend(pid) from pg_stat_replication where application_name like 'pgautofailover%'" >/dev/null
    say "-- standby cut off from the primary's WAL; writes continue with synchronous_commit=local"
    sleep 5
    say "   rows written so far: $(wc -l < $HP-writer.log), delivered to the consumer: $(grep -c 'INSERT: id' $HP-consumer.log)"
    say "   standby has: $(pgq pg-a-standby "select count(*) from events")"
    ;;
esac
case "$SCN" in
  planned)
    T0=$(dc exec -T monitor date +%s.%N | tr -d '\r')
    say "-- t0=$T0: pg_autoctl perform failover"
    dc exec -T monitor pg_autoctl perform failover --pgdata /data/pgdata >/dev/null 2>&1 ;;
  *)
    T0=$(dc exec -T monitor date +%s.%N | tr -d '\r')
    say "-- t0=$T0: docker kill pg-a-primary"
    dc kill pg-a-primary >/dev/null 2>&1 ;;
esac

say "-- waiting for the monitor to report pg-a-standby as the writable primary"
promoted=0
for i in $(seq 1 300); do
  r=$(mon "select reportedstate from pgautofailover.node where nodehost = 'pg-a-standby'" 2>/dev/null)
  case "$r" in primary|wait_primary|single) promoted=1; break ;; esac
  sleep 1
done
say "   promoted: $promoted (state: $r)"
say "   monitor: pg-a-standby reached $r at t0 +$(mon "select round((extract(epoch from min(eventtime)) - $T0)::numeric, 1) from pgautofailover.event where nodeid = (select nodeid from pgautofailover.node where nodehost = 'pg-a-standby') and reportedstate in ('wait_primary','primary') and extract(epoch from eventtime) > $T0") s"
sleep 15

cl touch /tmp/a-stop
sleep 3
# let the consumer drain
for i in $(seq 1 30); do
  last=$(cl psql "$URI" -X -At -c "select max(id) from events" 2>/dev/null)
  got=$(grep -c "INSERT: id\[bigint\]:$last " $HP-consumer.log)
  [ "${got:-0}" -ge 1 ] && break
  sleep 1
done
cl touch /tmp/a-stop-consumer
dc exec -T a-client pkill pg_recvlogical >/dev/null 2>&1
sleep 1

cl psql "$URI" -X -At -c "select id from events order by id" > $HP-db.ids 2>/dev/null
say
say "== result: $SCN =="
perl analyze-a.pl "$T0" $HP-db.ids $HP-writer.log $HP-consumer.log | tee -a "$OUT"
say
say "== monitor events =="
mon "select to_char(eventtime, 'HH24:MI:SS.MS'), nodename, reportedstate, goalstate, left(description, 90) from pgautofailover.event order by eventid" | sed 's/^/   /' | tail -25 | tee -a "$OUT"

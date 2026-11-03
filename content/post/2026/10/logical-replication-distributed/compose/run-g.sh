#!/usr/bin/env bash
# Profile g: DDL across regions. us-east and us-west, each a pg_auto_failover group of
# its own (a monitor and three nodes).
#
#   ./run-g.sh core        native logical replication, both directions, row-filtered:
#                          what an ALTER TABLE does, and three ways to roll it out
#   ./run-g.sh pgcopydb    a one-way link us-east -> us-west carried by pgcopydb
#                          clone --follow with the DDL event trigger: ADD, RENAME and
#                          DROP COLUMN, then the us-east primary is killed
#   ./run-g.sh pgcopydb-failover
#                          the same with a pgcopydb whose slot is FAILOVER true
#                          (--enable-failover), restarted after the kill
#   WRITER_PAUSE=0.4 ./run-g.sh pgcopydb     slows the writer (default 0.02, ~25 rows/s)
#   KILL_REGION=us-west ./run-g.sh pgcopydb-failover   kills the target's primary instead
set -uo pipefail
cd "$(dirname "$0")"
. ./lib-regions.sh
export COMPOSE_PROFILES=g

SCN=${1:?core | pgcopydb | pgcopydb-failover}
case "$SCN" in core|pgcopydb|pgcopydb-failover) ;; *) echo "core | pgcopydb | pgcopydb-failover"; exit 2 ;; esac
OUT=results/g-$SCN.out
say() { echo "$@" | tee -a "$OUT"; }
: > "$OUT"
cl() { dc exec -T client "$@"; }
q() { cl psql "$(ruri "$1")" -X -At -c "$2" 2>&1; }      # against the region's primary
other() { [ "$1" = us-east ] && echo us-west || echo us-east; }
SCHEMA="create table events (region text not null, id bigint not null, applied_at timestamptz not null default clock_timestamp(), primary key (region, id))"

./reset-g.sh >/dev/null
region_up us-east us-west || { say "did not come up"; exit 1; }
dc up -d client >/dev/null 2>&1
for r in us-east us-west; do region_wait $r || { say "$r never converged"; exit 1; }; done
say "== profile g, $SCN: us-east primary $(region_primary us-east), us-west primary $(region_primary us-west) =="

# rows of region $1 that its own primary has and the other region does not yet have
lagrows() { echo $(( $(q "$1" "select count(*) from events where region = '$1'") - $(q "$(other "$1")" "select count(*) from events where region = '$1'") )); }
apply_errs() { q "$1" "select coalesce(sum(apply_error_count), 0) from pg_stat_subscription_stats"; }
writers_start() { # east|both [pause between inserts]
  cl rm -f /tmp/g-stop /tmp/g-pause
  dc exec -d client sh -c "/traffic/g-writer.sh '$(ruri us-east)' us-east /results/g-writer-east.log ${2:-0.02}"
  [ "$1" = both ] && dc exec -d client sh -c "/traffic/g-writer.sh '$(ruri us-west)' us-west /results/g-writer-west.log ${2:-0.02}"
}

core() {
  for r in us-east us-west; do
    q $r "$SCHEMA" >/dev/null
    q $r "create publication pub_${r//-/_} for table events where (region = '$r')" >/dev/null
  done
  q us-west "create subscription sub_us_west_from_us_east connection '$(rconninfo us-east)' publication pub_us_east with (copy_data = false, failover = true)" >/dev/null
  q us-east "create subscription sub_us_east_from_us_west connection '$(rconninfo us-west)' publication pub_us_west with (copy_data = false, failover = true)" >/dev/null
  writers_start both
  sleep 15
  say "-- steady state: lag us-east -> us-west $(lagrows us-east) rows, us-west -> us-east $(lagrows us-west) rows; apply errors $(apply_errs us-east)/$(apply_errs us-west)"

  say; say "== A: ALTER TABLE ... ADD COLUMN on us-east only, us-west ten seconds later =="
  e0=$(apply_errs us-east); w0=$(apply_errs us-west)
  t0=$(date +%s)
  q us-east "alter table events add column note_a text" >/dev/null
  sleep 10
  say "   10 s after the ALTER on us-east: us-west apply errors +$(( $(apply_errs us-west) - w0 )), us-east rows not yet on us-west: $(lagrows us-east)"
  say "   us-west log: $(dc exec -T $(region_primary us-west) sh -c 'grep -h "missing replicated column" /data/pgdata/log/*.log | head -1' | sed 's/^[0-9-]* [0-9:.]* UTC \[[0-9]*\] //')"
  q us-west "alter table events add column note_a text" >/dev/null
  for i in $(seq 1 120); do [ "$(lagrows us-east)" -le 3 ] && break; sleep 0.5; done
  say "   us-west caught up $(( $(date +%s) - t0 )) s after the ALTER on us-east; no row lost: us-east rows on us-west $(q us-west "select count(*) from events where region = 'us-east'") of $(q us-east "select count(*) from events where region = 'us-east'")"
  say "   the standby us-east-b has the column without any command of ours: $(rpq us-east-b "select count(*) from information_schema.columns where table_name = 'events' and column_name = 'note_a'")"

  say; say "== B: the same ALTER on both primaries at the same moment, three times =="
  for k in 1 2 3; do
    e0=$(apply_errs us-east); w0=$(apply_errs us-west)
    q us-east "alter table events add column note_b$k text" >/dev/null &
    q us-west "alter table events add column note_b$k text" >/dev/null &
    wait
    sleep 12
    say "   B$k: apply errors +$(( $(apply_errs us-east) - e0 )) on us-east, +$(( $(apply_errs us-west) - w0 )) on us-west; lag $(lagrows us-east)/$(lagrows us-west) rows"
  done

  say; say "== C: writes paused, drained, ALTER on both, writes resumed =="
  e0=$(apply_errs us-east); w0=$(apply_errs us-west)
  tp=$(date +%s.%N)
  cl touch /tmp/g-pause
  for i in $(seq 1 120); do
    [ "$(lagrows us-east)" = 0 ] && [ "$(lagrows us-west)" = 0 ] && break; sleep 0.5
  done
  q us-east "alter table events add column note_c text" >/dev/null
  q us-west "alter table events add column note_c text" >/dev/null
  cl rm -f /tmp/g-pause
  tr=$(date +%s.%N)
  say "   writes paused for $(echo "$tr - $tp" | bc | cut -c1-4) s"
  sleep 12
  say "   apply errors +$(( $(apply_errs us-east) - e0 )) on us-east, +$(( $(apply_errs us-west) - w0 )) on us-west; lag $(lagrows us-east)/$(lagrows us-west) rows"

  cl touch /tmp/g-stop; sleep 3
  for i in $(seq 1 60); do [ "$(lagrows us-east)" = 0 ] && [ "$(lagrows us-west)" = 0 ] && break; sleep 1; done
  say; say "== result =="
  for r in us-east us-west; do
    say "   rows owned by $r: acknowledged $(wc -l < results/g-writer-${r#us-}.log | tr -d ' '), on $r $(q $r "select count(*) from events where region = '$r'"), on $(other $r) $(q $(other $r) "select count(*) from events where region = '$r'")"
  done
}

pgcopydb_link() {
  local SRC TGT SVC=pgcopydb-g PAUSE=${WRITER_PAUSE:-0.02}
  SRC=$(rconninfo us-east); TGT=$(rconninfo us-west)
  q us-east "$SCHEMA" >/dev/null
  dc up -d $SVC >/dev/null 2>&1
  say "-- $SVC: pgcopydb $(dc exec -T $SVC pgcopydb --version 2>/dev/null | tr -d '\r' | head -1); clone --follow us-east -> us-west, writer pause ${PAUSE}s"
  local FLAGS=""
  [ "$SCN" = pgcopydb-failover ] && FLAGS="--enable-failover"
  dc exec -d $SVC sh -c "pgcopydb clone --follow $FLAGS --source '$SRC' --target '$TGT' > /results/g-$SCN.log 2>&1"
  for i in $(seq 1 90); do
    [ "$(q us-west "select to_regclass('public.events') is not null")" = t ] && break; sleep 1
  done
  SLOTQ="select coalesce(string_agg(slot_name || ' (failover=' || failover || ', active=' || active || ')', ', '), 'none') from pg_replication_slots where slot_type = 'logical'"
  say "   table on us-west after ${i}s; logical slots on the us-east primary: $(rpq $(region_primary us-east) "$SLOTQ")"
  writers_start east $PAUSE
  sleep 8
  dc exec -T $SVC pgcopydb stream event-trigger --source "$SRC" >/dev/null 2>&1
  say "   event trigger installed; steady state: lag $(lagrows us-east) rows"
  maxlag() { local m=0 l i; for i in $(seq 1 ${2:-10}); do l=$(lagrows us-east); [ "$l" -gt "$m" ] && m=$l; sleep 1; done; echo $m; }
  local e0 w0 e1 w1 ml
  e0=$(q us-east "select count(*) from events where region = 'us-east'"); w0=$(q us-west "select count(*) from events where region = 'us-east'")
  ml=$(maxlag x 20)
  e1=$(q us-east "select count(*) from events where region = 'us-east'"); w1=$(q us-west "select count(*) from events where region = 'us-east'")
  say "   over 20 s: $(( (e1 - e0) / 20 )) rows/s written on us-east, $(( (w1 - w0) / 20 )) rows/s arrived on us-west; largest lag $ml rows"
  waitcol() { # present|absent <column>
    local i c; for i in $(seq 1 60); do
      c=$(q us-west "select count(*) from information_schema.columns where table_name = 'events' and column_name = '$2'")
      { [ "$1" = present ] && [ "$c" = 1 ]; } || { [ "$1" = absent ] && [ "$c" = 0 ]; } && { echo "${i}s"; return; }
      sleep 1
    done; echo "never"
  }
  say; say "== ADD COLUMN on us-east, nothing run on us-west =="
  q us-east "alter table events add column note text" >/dev/null
  q us-east "insert into events (region, id, note) values ('us-east', 1000000, 'carries the new column')" >/dev/null
  say "   column on us-west after $(waitcol present note); row with the new column: $(q us-west "select note from events where id = 1000000")"
  say "   largest lag over the next 10 s: $(maxlag) rows"
  say; say "== RENAME COLUMN =="
  q us-east "alter table events rename column note to remark" >/dev/null
  say "   renamed on us-west after $(waitcol present remark); largest lag over the next 10 s: $(maxlag) rows"
  say; say "== DROP COLUMN =="
  q us-east "alter table events drop column remark" >/dev/null
  say "   dropped on us-west after $(waitcol absent remark); largest lag over the next 10 s: $(maxlag) rows"

  K=${KILL_REGION:-us-east}
  say; say "== the $K primary is killed under load =="
  OLD=$(region_primary $K)
  for n in $K-a $K-b $K-c; do
    [ "$n" != "$OLD" ] && say "   $n: logical slots: $(rpq $n "select coalesce(string_agg(slot_name || ' (failover=' || failover || ', synced=' || coalesce(synced::text, '-') || ')', ', '), 'none') from pg_replication_slots where slot_type = 'logical'")"
  done
  T0=$(dc exec -T $K-monitor date +%s.%N | tr -d '\r')
  say "   at the kill: us-east holds $(q us-east "select count(*) from events where region = 'us-east'") rows, us-west $(q us-west "select count(*) from events where region = 'us-east'")"
  say "   t0=$T0: docker kill $OLD"
  dc kill $OLD >/dev/null 2>&1
  for i in $(seq 1 300); do
    NEW=$(rmon $K "select nodename from pgautofailover.node where nodename <> '$OLD' and reportedstate in ('primary','wait_primary','single') order by nodeid limit 1" 2>/dev/null)
    [ -n "$NEW" ] && break; sleep 1
  done
  say "   new primary of $K: $NEW"
  sleep 60
  cl touch /tmp/g-stop; sleep 3
  say "   slots on the new primary: $(rpq $NEW "$SLOTQ")"
  say "   after the writer stopped: us-west is $(lagrows us-east) rows behind"
  for i in $(seq 1 120); do [ "$(lagrows us-east)" -le 0 ] && break; sleep 1; done
  say "   rows owned by us-east: acknowledged $(wc -l < results/g-writer-east.log | tr -d ' '), on us-east $(q us-east "select count(*) from events where region = 'us-east'"), on us-west $(q us-west "select count(*) from events where region = 'us-east'") (caught up after a further ${i}s)"
  say "-- the errors in the pgcopydb log"
  grep -m4 "ERROR\|FATAL" results/g-$SCN.log | cut -c1-240 | sed 's/^/   /' | tee -a "$OUT"
  say "-- the last lines of the pgcopydb log"
  tail -4 results/g-$SCN.log | cut -c1-200 | sed 's/^/   /' | tee -a "$OUT"
}

case "$SCN" in
  core)     core ;;
  pgcopydb|pgcopydb-failover) pgcopydb_link ;;
esac

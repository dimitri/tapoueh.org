#!/usr/bin/env bash
# Profile k: pgcopydb does the first copy and follows for a while, then a subscription of the
# long-lived tools takes the stream over, while a writer keeps inserting. e-w1 is the source,
# e-w2 the target (Postgres 18, the pglogical image).
#
#   ./reset-e.sh && ./run-k.sh core        core logical replication takes over pgcopydb's slot
#   ./reset-e.sh && ./run-k.sh pglogical   a pglogical subscription takes over
#
# The check at the end compares the ids of the two servers: a hole is a row the target never got,
# a duplicate is refused by the primary key and shows as an apply error. PRELOAD sets the size of
# the first copy, SECS how long the writer runs (50 rows a second).
set -uo pipefail
cd "$(dirname "$0")"
mkdir -p results

dc() { docker compose -p lrdist "$@"; }
q()  { local n=$1; shift; dc exec -T "$n" psql -X -q -U postgres -d app -v ON_ERROR_STOP=0 "$@" 2>&1; }
qa() { local n=$1; shift; dc exec -T "$n" psql -X -At -U postgres -d app "$@" 2>&1; }
cd_() { dc exec -T pgcopydb-e "$@"; }
SRC='postgres://postgres@e-w1/app'; TGT='postgres://postgres@e-w2/app'
PRELOAD=${PRELOAD:-300000}; SECS=${SECS:-80}
M=${1:?core | pglogical}
OUT=results/k-handover-$M${RUN:+-run$RUN}.out
say() { echo "$@" | tee -a "$OUT"; }
: > "$OUT"

ids() { qa "$1" -c "select count(*) || ' rows, ids ' || min(id) || ' to ' || max(id) || ', digest ' || md5(string_agg(id::text, ',' order by id)) from t"; }
pcdb() { cd_ pgcopydb "$@" --source "$SRC" --target "$TGT"; }
wait_exit() { local i; for i in $(seq 1 120); do [ "$(cd_ sh -c "pgrep -f 'pgcopydb (clone|follow)' | wc -l" | tr -d ' \r')" = 0 ] && return 0; sleep 1; done; return 1; }

say "== $M: pgcopydb copies $PRELOAD rows and follows, then $M takes over, a writer inserting meanwhile =="
q e-w1 -c "create table t (id bigserial primary key, payload text not null)" \
       -c "insert into t (payload) select repeat(md5(g::text), 3) from generate_series(1, $PRELOAD) g" >/dev/null
dc exec -T e-w1 sh -c "echo 'insert into t (payload) values (repeat(md5(random()::text), 3));' > /tmp/ins.sql"
dc exec -d e-w1 sh -c "pgbench -n -f /tmp/ins.sql -R 50 -T $SECS -U postgres app > /tmp/pgbench.out 2>&1"

COMPOSE_PROFILES=e dc up -d --force-recreate pgcopydb-e >/dev/null 2>&1
cd_ sh -c "rm -rf /root/.local/share/pgcopydb /tmp/pgcopydb"
dc exec -d pgcopydb-e sh -c "pgcopydb clone --follow --plugin pgoutput --slot-name handover --origin handover --source '$SRC' --target '$TGT' > /tmp/pgcopydb-k.log 2>&1"
for i in $(seq 1 120); do
  s=$(qa e-w1 -c "select count(*) from t"); t=$(qa e-w2 -c "select count(*) from t" 2>/dev/null)
  case $t in ''|*[!0-9]*) t=0 ;; esac
  [ "$t" -ge "$PRELOAD" ] && [ $((s - t)) -lt 300 ] && break; sleep 1
done
say "-- pgcopydb has copied $PRELOAD rows and caught up after $i s: source $s rows, target $t rows"
say "-- slot on the source while pgcopydb follows"
qa e-w1 -c "select '   ' || slot_name || ' (' || plugin || '), confirmed_flush_lsn ' || confirmed_flush_lsn || ', current ' || pg_current_wal_lsn() from pg_replication_slots" | tee -a "$OUT"

case $M in
core)
  say "== stop pgcopydb at a marker =="
  cd_ pgcopydb stream sentinel set endpos --current --source "$SRC" 2>&1 | tail -1 | sed 's/^/   endpos /' | tee -a "$OUT"
  wait_exit && say "   pgcopydb exited" || say "   pgcopydb is still running"
  R=$(qa e-w1 -c "select confirmed_flush_lsn from pg_replication_slots where slot_name = 'handover'")
  A=$(qa e-w2 -c "select remote_lsn from pg_replication_origin_status where external_id = 'handover'")
  say "   the slot's confirmed_flush_lsn is $R, the last transaction pgcopydb applied is at $A"
  say "   (pgcopydb confirms what its receiver has spooled, and the applier is behind it)"
  say "== apply what pgcopydb has received, up to the slot's position =="
  cd_ pgcopydb follow --resume --not-consistent --plugin pgoutput --slot-name handover --origin handover --endpos "$R" --source "$SRC" --target "$TGT" > /tmp/k-follow2.log 2>&1
  A=$(qa e-w2 -c "select remote_lsn from pg_replication_origin_status where external_id = 'handover'")
  R2=$(qa e-w1 -c "select confirmed_flush_lsn from pg_replication_slots where slot_name = 'handover'")
  say "   last transaction applied: $A, slot's confirmed_flush_lsn: $R2"
  say "== core subscription on pgcopydb's slot and publication, its origin moved to $A =="
  q e-w2 -c "create subscription takeover connection 'host=e-w1 dbname=app user=postgres' publication handover with (create_slot = false, slot_name = 'handover', copy_data = false, enabled = false)" | tee -a "$OUT"
  O=$(qa e-w2 -c "select 'pg_' || oid from pg_subscription where subname = 'takeover'")
  q e-w2 -c "select pg_replication_origin_advance('$O', '$A')" -c "alter subscription takeover enable" >/dev/null
  ;;
pglogical)
  say "== pglogical on both servers, subscription created and disabled at once =="
  q e-w1 -c "create extension if not exists pglogical" -c "select pglogical.create_node('provider', 'host=e-w1 dbname=app user=postgres')" -c "select pglogical.replication_set_add_table('default', 't', false)" >/dev/null
  q e-w2 -c "create extension if not exists pglogical" -c "select pglogical.create_node('subscriber', 'host=e-w2 dbname=app user=postgres')" >/dev/null
  q e-w2 -c "select pglogical.create_subscription('takeover', 'host=e-w1 dbname=app user=postgres', synchronize_structure := false, synchronize_data := false)" >/dev/null
  q e-w2 -c "select pglogical.alter_subscription_disable('takeover', true)" >/dev/null
  sleep 2
  say "-- slots on the provider"
  qa e-w1 -c "select '   ' || slot_name || ' (' || plugin || '), active=' || active || ', confirmed_flush_lsn ' || confirmed_flush_lsn || ', current ' || pg_current_wal_lsn() from pg_replication_slots" | tee -a "$OUT"
  say "== stop pgcopydb at a marker =="
  cd_ pgcopydb stream sentinel set endpos --current --source "$SRC" 2>&1 | tail -1 | sed 's/^/   endpos /' | tee -a "$OUT"
  wait_exit && say "   pgcopydb exited" || say "   pgcopydb is still running"
  A=$(qa e-w2 -c "select remote_lsn from pg_replication_origin_status where external_id = 'handover'")
  say "   the last transaction pgcopydb applied is at $A"
  q e-w1 -c "select pg_drop_replication_slot('handover')" >/dev/null
  say "== enable the pglogical subscription (its slot started before pgcopydb stopped, so inserts that pgcopydb applied arrive again) =="
  q e-w2 -c "alter system set pglogical.conflict_log_level = warning" -c "select pg_reload_conf()" >/dev/null
  q e-w2 -c "select pglogical.alter_subscription_enable('takeover', true)" | tee -a "$OUT"
  ;;
esac

say "== the writer finishes, then both servers must hold the same rows =="
for i in $(seq 1 $((SECS + 30))); do
  [ "$(dc exec -T e-w1 sh -c "pgrep -x pgbench | wc -l" | tr -d ' \r')" = 0 ] && break; sleep 1
done
for i in $(seq 1 90); do
  [ "$(ids e-w1)" = "$(ids e-w2)" ] && { say "   identical $i s after the writer stopped"; break; }; sleep 1
done
say "   source: $(ids e-w1)"
say "   target: $(ids e-w2)"
case $M in
core) say "-- subscription errors"; qa e-w2 -c "select '   apply_error_count=' || apply_error_count || ' sync_error_count=' || sync_error_count from pg_stat_subscription_stats" | tee -a "$OUT"
      say "-- the slot"; qa e-w1 -c "select '   ' || slot_name || ' active=' || active || ' failover=' || failover from pg_replication_slots" | tee -a "$OUT" ;;
pglogical)
      say "-- subscription"; qa e-w2 -c "select '   ' || subscription_name || ' ' || status from pglogical.show_subscription_status()" | tee -a "$OUT"
      say "-- in the subscriber's log: $(dc logs e-w2 2>&1 | grep -c 'CONFLICT') inserts already applied by pgcopydb, resolved with apply_remote; $(dc logs e-w2 2>&1 | grep -c 'duplicate key') duplicate-key errors from the apply worker's first start, before we disabled it"
      say "-- a schema change through the stream, with the writers stopped"
      q e-w1 -c "select pglogical.replicate_ddl_command(\$\$ alter table public.t add column note text \$\$)" | tee -a "$OUT"
      sleep 3; say "   column on the target: $(qa e-w2 -c "select count(*) from information_schema.columns where table_name = 't' and column_name = 'note'")" ;;
esac

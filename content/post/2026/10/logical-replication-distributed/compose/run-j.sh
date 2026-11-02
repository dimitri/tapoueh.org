#!/usr/bin/env bash
# Profile j: what core logical replication does when two nodes subscribed to each other
# write the same keys, on Postgres 18 and on Postgres 19 release candidate 1.
#
#   J_IMAGE=postgres:18 RUN=1 ./run-j.sh updates   4 clients per node updating the same 1,000 keys
#   J_IMAGE=postgres:19rc1 RUN=1 ./run-j.sh mixed  the same keys updated, deleted and inserted;
#                                                  RETAIN=on sets retain_dead_tuples on both subscriptions
#
# Same workload as run-e.sh concurrent, so the numbers sit next to pglogical's.
set -uo pipefail
cd "$(dirname "$0")"
mkdir -p results
export J_IMAGE=${J_IMAGE:-postgres:18}
SCN=${1:?updates | mixed}
SECS=${SECS:-15}
RETAIN=${RETAIN:-off}
TAG=${J_IMAGE//[:\/]/-}
OUT=results/j-$TAG-$SCN
[ "$SCN" = mixed ] && OUT=$OUT-retain-$RETAIN
OUT=$OUT${RUN:+-run$RUN}.out
dc() { docker compose -p lrdist "$@"; }
q() { local n=$1; shift; dc exec -T "$n" psql -X -q -U postgres -d app "$@" 2>&1; }
qa() { local n=$1; shift; dc exec -T "$n" psql -X -At -U postgres -d app "$@" 2>&1; }
say() { echo "$@" | tee -a "$OUT"; }
: > "$OUT"

export COMPOSE_PROFILES=j
dc rm -sf j-a j-b >/dev/null 2>&1
dc up -d --wait j-a j-b >/dev/null 2>&1 || { say "did not start"; exit 1; }
say "== $(qa j-a -c 'select version()' | cut -c1-40), $SCN, $SECS s, 4 clients per node, retain_dead_tuples=$RETAIN =="

for n in j-a j-b; do
  q $n -c "create table kv (id int primary key, v int not null, writer text not null); insert into kv select g, 0, 'seed' from generate_series(1, 1000) g; create publication pub_kv for table kv" >/dev/null
done
OPTS="copy_data = false, origin = none"
[ "$RETAIN" = on ] && OPTS="$OPTS, retain_dead_tuples = true"
q j-a -c "create subscription sub_j_a_from_j_b connection 'host=j-b dbname=app user=postgres' publication pub_kv with ($OPTS)" >/dev/null
q j-b -c "create subscription sub_j_b_from_j_a connection 'host=j-a dbname=app user=postgres' publication pub_kv with ($OPTS)" >/dev/null
for i in $(seq 1 30); do
  [ "$(qa j-a -c "select count(*) from pg_stat_subscription where relid is null and pid is not null")" = 1 ] &&
  [ "$(qa j-b -c "select count(*) from pg_stat_subscription where relid is null and pid is not null")" = 1 ] && break; sleep 1
done

workload() { # node tag
  case $SCN in
    updates) SCRIPT="\\set id random(1, 1000)
update kv set v = v + 1, writer = '$2' where id = :id;" ;;
    mixed) SCRIPT="\\set id random(1, 1000)
\\set r random(1, 100)
\\if :r <= 60
update kv set v = v + 1, writer = '$2' where id = :id;
\\elif :r <= 80
delete from kv where id = :id;
\\else
insert into kv values (:id, 1, '$2') on conflict (id) do update set v = kv.v + 1, writer = '$2';
\\endif" ;;
  esac
  dc exec -T "$1" sh -c "cat > /tmp/w.sql <<'EOS'
$SCRIPT
EOS
pgbench -n -U postgres -d app -c 4 -j 2 -R 2000 -T $SECS -f /tmp/w.sql 2>&1 | grep -E 'transactions actually'" | grep -o '[0-9]*$'
}
workload j-a a > /tmp/j-a.$$ & workload j-b b > /tmp/j-b.$$ & wait
c1=$(cat /tmp/j-a.$$); c2=$(cat /tmp/j-b.$$); rm -f /tmp/j-a.$$ /tmp/j-b.$$
say "committed statements: j-a=$c1 j-b=$c2 (total $((c1 + c2)))"

snap() { qa "$1" -c "select id, v, writer from kv order by id" | md5sum | cut -c1-12; }
stable=0
for i in $(seq 1 60); do
  s1=$(snap j-a); s2=$(snap j-b)
  if [ "$s1" = "$s2" ]; then stable=$((stable + 1)); [ $stable -ge 3 ] && break; else stable=0; fi
  sleep 1
done
a=$(mktemp); b=$(mktemp)
qa j-a -c "select id, v, writer from kv order by id" > "$a"; qa j-b -c "select id, v, writer from kv order by id" > "$b"
say "after settling: $([ "$s1" = "$s2" ] && echo CONVERGED || echo DIVERGED); rows that differ between the nodes: $(diff "$a" "$b" | grep -c '^<'); rows present: j-a=$(wc -l < "$a" | tr -d ' ') j-b=$(wc -l < "$b" | tr -d ' ')"
rm -f "$a" "$b"
if [ "$SCN" = updates ]; then
  say "sum(v), every update adds 1: j-a=$(qa j-a -c 'select sum(v) from kv') j-b=$(qa j-b -c 'select sum(v) from kv')"
fi
for n in j-a j-b; do
  say "-- $n: conflicts counted and apply errors (pg_stat_subscription_stats, non-zero columns)"
  qa $n -c "select '   ' || key || ' = ' || value from pg_stat_subscription_stats s, jsonb_each_text(to_jsonb(s)) where (key like 'confl_%' or key like '%error_count') and value::bigint > 0 order by key" | tee -a "$OUT"
  say "   subscription workers running: $(qa $n -c "select count(*) from pg_stat_subscription where relid is null and pid is not null")"
done

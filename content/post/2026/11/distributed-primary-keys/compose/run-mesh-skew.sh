#!/bin/bash
# Three regions whose clocks differ: region 0 on time, region 1 <skew> ahead, region 2 <skew> behind.
# ./run-mesh-skew.sh <expression|uuidv7|regionseq> <skew in seconds>
#   Same mesh as run-mesh.sh.  Reported: convergence, and the size and leaf density of the primary key on region 0.
cd "$(dirname "$0")"; . ./lib.sh; IMAGE=pk-seqam-ft
K=${1:?key}; SK=${2:?skew in seconds}
case $K in expression) V=snowflake ;; uuidv7) V=uuidv7 ;; regionseq) V=regionam ;; *) echo "unknown key $K"; exit 1 ;; esac
NET=pknet; docker network create $NET >/dev/null 2>&1
TMPFS=1500m
OFF=("+0s" "+${SK}s" "-${SK}s")
for i in 0 1 2; do
  EXTRA=""; [ $V = regionam ] && EXTRA=$(rsargs $((i+1)))
  pg_start pk-ms$i -c wal_level=logical -c max_wal_size=3GB -c track_commit_timestamp=on $EXTRA
  docker exec pk-ms$i sh -c "echo ${OFF[$i]} > /tmp/ft.new && mv /tmp/ft.new /tmp/ft"
done
for i in 0 1 2; do
  variant $V $i
  psqlx pk-ms$i -c "create extension pgstattuple" -c "$DDL" -c "create publication p for table t"
done
for i in 0 1 2; do for j in 0 1 2; do
  [ $i = $j ] && continue
  psqlx pk-ms$i -c "create subscription s_${i}_from_$j connection 'host=pk-ms$j dbname=postgres user=postgres' publication p with (copy_data = false, origin = none)" >/dev/null
done; done
sleep 3
for i in 0 1 2; do
  variant $V $i
  echo "insert into t (id, region, payload) select $EXPR, $i, $PAYLOAD from generate_series(1, 100);" > /tmp/pk-ms-$i.sql
  docker cp /tmp/pk-ms-$i.sql pk-ms$i:/tmp/w.sql
done
for i in 0 1 2; do
  docker exec pk-ms$i pgbench -n -U postgres -d postgres -c 2 -j 2 -T 10 -R 200 --continue-on-error -f /tmp/w.sql > /tmp/pk-ms-out-$i.txt 2>&1 &
done
wait
WROTE=0; for i in 0 1 2; do n=$(psqlx pk-ms$i -At -c "select count(*) from t where region = $i"); WROTE=$((WROTE+n)); done
for t in 1 2 3 4 5 6 7 8 9 10 11 12; do
  sleep 5; ok=1
  for i in 0 1 2; do [ "$(psqlx pk-ms$i -At -c 'select count(*) from t')" = "$WROTE" ] || ok=0; done
  [ $ok = 1 ] && break
done
echo "== key=$K clocks: 0, +${SK}s, -${SK}s"
echo "rows written $WROTE"
for i in 0 1 2; do
  psqlx pk-ms$i -At -F ' | ' <<SQL
select 'region $i holds', count(*), 'digest', sum(hashtextextended(id::text || ':' || region::text, 0)) from t;
select 'region $i apply errors', coalesce(sum(apply_error_count), 0) from pg_stat_subscription_stats;
SQL
done
psqlx pk-ms0 -At -F ' | ' -c "select 'pk index on region 0, MB', round(pg_relation_size('t_pkey') / 1e6, 1), 'leaf density %', (pgstatindex('t_pkey')).avg_leaf_density"
for i in 0 1 2; do docker rm -f pk-ms$i >/dev/null; done

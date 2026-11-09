#!/bin/bash
# Three regions, each writing at once and each subscribed to the other two (origin = none).
# ./run-mesh.sh <variant> [RUN]    variants: naive seqstep composite uuidv4 uuidv7 snowflake snowam snowam-same regionam regionam-same
cd "$(dirname "$0")"; . ./lib.sh
V=${1:?variant}; RUN=${2:-1}
NET=pknet; docker network create $NET >/dev/null 2>&1
TMPFS=1500m
for i in 0 1 2; do
  EXTRA=""
  [ "$V" = regionam ] && EXTRA=$(rsargs $((i+1)))
  [ "$V" = regionam-same ] && EXTRA=$(rsargs 1)       # the same number in every region: the mistake
  pg_start pk-m$i -c wal_level=logical -c max_wal_size=3GB -c track_commit_timestamp=on $EXTRA
done
for i in 0 1 2; do
  variant $V $i
  psqlx pk-m$i -c "$DDL" -c "create publication p for table t"
done
for i in 0 1 2; do for j in 0 1 2; do
  [ $i = $j ] && continue
  psqlx pk-m$i -c "create subscription s_${i}_from_$j connection 'host=pk-m$j dbname=postgres user=postgres' publication p with (copy_data = false, origin = none)" >/dev/null
done; done
sleep 3
for i in 0 1 2; do
  variant $V $i
  cat > /tmp/pk-mesh-$i.sql <<SQL
insert into t (id, region, payload) select $EXPR, $i, $PAYLOAD from generate_series(1, 100);
SQL
  docker cp /tmp/pk-mesh-$i.sql pk-m$i:/tmp/w.sql
done
echo "== variant=$V run=$RUN: three regions write 200 transactions a second of 100 rows each, for 10 s"
for i in 0 1 2; do
  docker exec pk-m$i pgbench -n -U postgres -d postgres -c 2 -j 2 -T 10 -R 200 -f /tmp/w.sql > /tmp/pk-mesh-out-$i.txt 2>&1 &
done
wait
for i in 0 1 2; do
  echo "writer $i: $(grep 'number of transactions actually' /tmp/pk-mesh-out-$i.txt)"
done
# what each region wrote itself
WROTE=0
for i in 0 1 2; do
  n=$(psqlx pk-m$i -At -c "select count(*) from t where region = $i")
  echo "region $i wrote $n rows"; WROTE=$((WROTE + n))
done
echo "total written: $WROTE"
for t in 1 2 3 4 5 6 7 8 9 10 11 12; do
  sleep 5
  done_=1
  for i in 0 1 2; do
    n=$(psqlx pk-m$i -At -c "select count(*) from t"); [ "$n" = "$WROTE" ] || done_=0
  done
  [ $done_ = 1 ] && break
done
echo "waited about $((t*5)) s for the links to catch up"
for i in 0 1 2; do
  psqlx pk-m$i -At -F ' | ' <<SQL
select 'region $i holds', count(*), 'rows, digest', sum(hashtextextended(id::text || ':' || region::text, 0)) from t;
select 'region $i apply errors', coalesce(sum(apply_error_count), 0), 'conflicts insert_exists', coalesce(sum(confl_insert_exists), 0) from pg_stat_subscription_stats;
SQL
done
docker logs pk-m0 2>&1 | grep -E "ERROR|DETAIL" | sort | uniq -c | sort -rn | head -3
for i in 0 1 2; do docker rm -f pk-m$i >/dev/null; done

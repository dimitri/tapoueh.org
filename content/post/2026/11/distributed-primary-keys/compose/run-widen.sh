#!/bin/bash
# Widening an int key to bigint under logical replication: which side first.
# ./run-widen.sh <publisher-first|subscriber-first>
cd "$(dirname "$0")"; . ./lib.sh
ORDER=${1:?publisher-first|subscriber-first}
NET=pknet; docker network create $NET >/dev/null 2>&1
TMPFS=2g; ROWS=${ROWS:-3000000}
pg_start pk-wp -c wal_level=logical -c max_wal_size=3GB
pg_start pk-ws -c wal_level=logical -c max_wal_size=3GB
DDL="create table t (id int primary key, region smallint not null, payload text not null)"
psqlx pk-wp -c "$DDL" -c "create publication p for table t where (region = 1)" >/dev/null 2>&1 || \
  psqlx pk-wp -c "$DDL" -c "create publication p for table t"
psqlx pk-wp -c "insert into t select g, 1, 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx' from generate_series(1, $ROWS) g"
psqlx pk-ws -c "$DDL"
psqlx pk-ws -c "create subscription s connection 'host=pk-wp dbname=postgres user=postgres' publication p" >/dev/null
until [ "$(psqlx pk-ws -At -c 'select count(*) from t')" = $ROWS ]; do sleep 2; done
echo "== $ORDER: $ROWS rows copied, both sides int"
now() { perl -MTime::HiRes=time -e 'printf "%.2f", time'; }
widen() {  # container
  local t0=$(now)
  psqlx $1 -c "alter table t alter column id type bigint"
  echo "$1: alter table ... type bigint took $(perl -e "printf '%.1f', $(now) - $t0") s"
}
if [ $ORDER = publisher-first ]; then
  widen pk-wp
  psqlx pk-wp -c "insert into t values (3000000000, 1, 'past int4')"
  sleep 8
  echo "-- subscriber still int; the subscription after 8 s:"
  psqlx pk-ws -At -F ' | ' -c "select 'rows on subscriber', count(*) from t" -c "select 'apply_error_count', apply_error_count from pg_stat_subscription_stats"
  docker logs pk-ws 2>&1 | grep -E "ERROR|CONTEXT" | tail -2
  widen pk-ws
  sleep 8
  psqlx pk-ws -At -F ' | ' -c "select 'after widening the subscriber: has 3000000000', count(*) from t where id = 3000000000"
else
  widen pk-ws
  widen pk-wp
  psqlx pk-wp -c "insert into t values (3000000000, 1, 'past int4')"
  sleep 8
  psqlx pk-ws -At -F ' | ' -c "select 'rows on subscriber has 3000000000', count(*) from t where id = 3000000000" -c "select 'apply_error_count', apply_error_count from pg_stat_subscription_stats"
fi
docker rm -f pk-wp pk-ws >/dev/null

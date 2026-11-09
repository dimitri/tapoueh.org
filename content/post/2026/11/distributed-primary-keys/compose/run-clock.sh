#!/bin/bash
# A key made from the clock, with the clock stepped back while four clients insert.
# ./run-clock.sh <variant> <step> <rate> [RUN]     variants: snowam snowone uuidv7
#   step: how far back the clock goes after 3 s, e.g. 2s or 50ms; rate: "full" or transactions a second (100 rows each)
# Needs pk-seqam-ft (seqam/Dockerfile.faketime): libfaketime on the server process, stepped through /tmp/ft.
# The file is replaced atomically: libfaketime reads it at every clock call and a half-written file kills the server.
cd "$(dirname "$0")"; . ./lib.sh; IMAGE=pk-seqam-ft
V=${1:?variant}; STEP=${2:?step}; RATE=${3:?rate}; RUN=${4:-1}
N=pk-clock; pg_start $N -c max_wal_size=3GB
SNOW1="(((extract(epoch from clock_timestamp()) * 1000)::bigint - 1767225600000) << 22) | (1::bigint << 18) | (nextval('s') & 262143)"
case $V in
  snowam) DDL="create extension snowflake; create sequence s using snowflake; create table t (id bigint primary key, payload text);"; EXPR="nextval('s')" ;;
  snowone) DDL="create sequence s; create table t (id bigint primary key, payload text);"; EXPR="$SNOW1" ;;
  uuidv7) DDL="create table t (id uuid primary key, payload text);"; EXPR="uuidv7()" ;;
  *) echo "unknown variant $V"; exit 1 ;;
esac
psqlx $N -c "$DDL"
echo "insert into t select $EXPR, 'x' from generate_series(1, 100);" > /tmp/pk-clock.sql
docker cp /tmp/pk-clock.sql $N:/tmp/w.sql
R=""; [ "$RATE" != full ] && R="-R $RATE"
echo "== variant=$V step=-$STEP rate=$RATE run=$RUN"
( sleep 3; docker exec $N sh -c "echo -$STEP > /tmp/ft.new && mv /tmp/ft.new /tmp/ft" ) &
docker exec $N pgbench -n -U postgres -d postgres -c 4 -j 4 -T 8 $R --continue-on-error -f /tmp/w.sql 2>&1 \
  | grep -E "^(number of transactions actually|number of failed|number of other|tps)"
wait
psqlx $N -At -F ' | ' -c "select 'rows kept', count(*) from t"
docker logs $N 2>&1 | grep -c "duplicate key" | sed 's/^/duplicate key errors in the log: /'
docker rm -f $N >/dev/null

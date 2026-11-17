#!/bin/bash
# What ordering a key keeps when the clock of the instance steps back while it works.
# ./run-clock-order.sh <sequence|uuidv4|uuidv7|expression|regionseq> <step> [RUN]    step: libfaketime syntax, 0s, 2s, 10s
#   4 clients insert 100 ids per transaction at 200 transactions a second for 10 s; the clock steps back after 4 s.
#   Reported: ids lower than one the same client already got, lower than one any client got, and for the keys
#   that carry the time, how far the time in the id is ahead of the server's clock.  pk-seqam-ft image.
cd "$(dirname "$0")"; . ./lib.sh; IMAGE=pk-seqam-ft
V=${1:?key}; STEP=${2:?step}; RUN=${3:-1}
EXTRA=""; TIMED=""
case $V in
  sequence)   DDL="create sequence s; create table t (n bigint generated always as identity, client int, id bigint, ts timestamptz default clock_timestamp());"; EXPR="nextval('s')"; K="lpad(id::text,20,'0')" ;;
  uuidv4)     DDL="create table t (n bigint generated always as identity, client int, id uuid, ts timestamptz default clock_timestamp());"; EXPR="gen_random_uuid()"; K="id::text" ;;
  uuidv7)     DDL="create table t (n bigint generated always as identity, client int, id uuid, ts timestamptz default clock_timestamp());"; EXPR="uuidv7()"; K="id::text"
              TIMED="(('x' || substr(replace(id::text, '-', ''), 1, 12))::bit(48)::bigint)" ;;
  expression) DDL="create sequence s; create table t (n bigint generated always as identity, client int, id bigint, ts timestamptz default clock_timestamp());"
              EXPR="(((extract(epoch from clock_timestamp()) * 1000)::bigint - 1767225600000) << 22) | (1::bigint << 18) | (nextval('s') & 262143)"; K="lpad(id::text,20,'0')"
              TIMED="((id >> 22) + 1767225600000)" ;;
  regionseq)  DDL="create extension regionseq; create sequence s using regionseq; create table t (n bigint generated always as identity, client int, id bigint, ts timestamptz default clock_timestamp());"
              EXPR="nextval('s')"; K="lpad(id::text,20,'0')"; EXTRA=$(rsargs 1); TIMED="((id >> 22) + 1767225600000)" ;;
  *) echo "unknown key $V"; exit 1 ;;
esac
N=pk-order; pg_start $N -c max_wal_size=2GB $EXTRA
psqlx $N -c "$DDL"
echo "insert into t (client, id) select :client_id, $EXPR from generate_series(1, 100);" > /tmp/pk-order.sql
docker cp /tmp/pk-order.sql $N:/tmp/w.sql
( sleep 4; docker exec $N sh -c "echo -$STEP > /tmp/ft.new && mv /tmp/ft.new /tmp/ft" ) &
docker exec $N pgbench -n -U postgres -d postgres -c 4 -j 4 -T 10 -R 200 --continue-on-error -f /tmp/w.sql 2>&1 | grep -E "^number of (transactions actually|failed)"
wait
echo "== key=$V clock back $STEP run=$RUN"
psqlx $N -At -F ' | ' <<SQL
create view v as select n, client, id, ts, $K as k from t;
select 'rows', count(*), 'distinct ids', count(distinct id) from t;
select 'lower than an id the same client already got', count(*) filter (where k collate "C" < m)
  from (select k, max(k collate "C") over (partition by client order by n rows between unbounded preceding and 1 preceding) m from v) s;
select 'lower than an id any client already got', count(*) filter (where k collate "C" < m)
  from (select k, max(k collate "C") over (order by n rows between unbounded preceding and 1 preceding) m from v) s;
SQL
if [ -n "$TIMED" ]; then
  psqlx $N -At -F ' | ' -c "select 'time in the id ahead of the server clock, ms: max', max($TIMED - (extract(epoch from ts) * 1000)::bigint), 'at the end', (select $TIMED - (extract(epoch from ts) * 1000)::bigint from t order by n desc limit 1) from t"
fi
docker rm -f $N >/dev/null

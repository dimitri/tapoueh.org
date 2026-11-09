#!/bin/bash
# The regionseq extension (extension/) in the lab: configuration mistakes, a crash with the clock set back,
# dump and restore into another region.  Needs pk-seqam-ft (seqam/Dockerfile.faketime).
cd "$(dirname "$0")"; . ./lib.sh; IMAGE=pk-seqam-ft
setclock() { docker exec $1 sh -c "echo $2 > /tmp/ft.new && mv /tmp/ft.new /tmp/ft"; }

echo "== library not preloaded: the sequence cannot be used"
pg_start pk-rf
psqlx pk-rf -c "create extension regionseq" -c "create sequence s using regionseq" 2>&1 | grep -E "ERROR|DETAIL"
docker rm -f pk-rf >/dev/null

echo "== preloaded, region number not set"
pg_start pk-rf -c shared_preload_libraries=regionseq
psqlx pk-rf -c "create extension regionseq" -c "create sequence s using regionseq" -c "select nextval('s')" 2>&1 | grep -E "ERROR|HINT"
echo "-- region number out of range:"
docker rm -f pk-rf >/dev/null
docker run --rm --entrypoint bash $IMAGE -c 'gosu postgres postgres -D /tmp/x -c shared_preload_libraries=regionseq -c regionseq.region_id=64 2>&1 | head -2' 2>&1 | grep -E "FATAL|invalid" | head -2

echo "== a crash, with the clock set back 10 seconds just before it"
pg_start pk-rf $(rsargs 2)
psqlx pk-rf -c "create extension regionseq" -c "create sequence s using regionseq" -c "create table t (id bigint primary key)"
psqlx pk-rf -c "insert into t select nextval('s') from generate_series(1, 100000)"
OLDMAX=$(psqlx pk-rf -At -c "select max(id) from t")
setclock pk-rf -10s
sleep 1
psqlx pk-rf -At -c "select 'clock now is ' || round(extract(epoch from now() - clock_timestamp()))::text || ' s from the old one'" >/dev/null
docker exec pk-rf sh -c 'for p in /proc/[0-9]*; do c=$(tr "\0" " " < $p/cmdline 2>/dev/null); case "$c" in *"postgres: checkpointer"*) [ ${p#/proc/} != $$ ] && kill -9 ${p#/proc/};; esac; done'
sleep 3; until docker exec pk-rf pg_isready -q -U postgres; do sleep 1; done; sleep 2
echo "crash recoveries in the log: $(docker logs pk-rf 2>&1 | grep -c 'not properly shut down')"
psqlx pk-rf -c "select * from s"
psqlx pk-rf -c "insert into t select nextval('s') from generate_series(1, 100000)"
psqlx pk-rf -At -F ' | ' <<SQL
select 'rows', count(*), 'distinct', count(distinct id) from t;
select 'new ids all above the old ones', bool_and(id > $OLDMAX) from t where id not in (select id from t order by id limit 100000);
select 'first new id is ' || ((min(id) filter (where id > $OLDMAX)) >> 22) - (($OLDMAX) >> 22) || ' ms above the last old one' from t;
SQL

echo "== dump from region 2, restore into region 3"
psqlx pk-rf -c "select setval('s', (select max(id) from t), true)" >/dev/null
docker exec pk-rf pg_dump -U postgres --schema-only -t s postgres | grep -v '^--' | grep -E "SEQUENCE|default_sequence|CREATE EXTENSION" | head
pg_start pk-rf2 $(rsargs 3)
psqlx pk-rf2 -c "create extension regionseq"
docker exec pk-rf pg_dump -U postgres -Fc -t t -t s postgres | docker exec -i pk-rf2 pg_restore -U postgres -d postgres 2>&1 | grep -E "error|warning"
psqlx pk-rf2 -At -F ' | ' <<'SQL'
select 'restored rows', count(*), 'from region', regionseq_region(max(id)) from t;
select 'sequence state: last_ms, counter, is_called', last_ms, counter, is_called from s;
select 'next id: region', regionseq_region(nextval('s')), 'above the restored ones', lastval() > (select max(id) from t);
SQL
docker rm -f pk-rf pk-rf2 >/dev/null

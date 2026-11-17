#!/bin/bash
# A failover where the promoted standby's clock is BEHIND the old primary's by <skew> seconds.
# ./run-failover-skew.sh <expression|uuidv7|regionseq> <insync|lagging> <skew> [RUN]
#   The client takes ids as fast as it can from the old primary; it is killed after 3 s; the standby is promoted
#   and makes 200,000 ids.  Reported: how many repeat an id the old primary gave, and how many are below the highest
#   one it gave.  Needs pk-seqam-ft (libfaketime on the server process).
cd "$(dirname "$0")"; . ./lib.sh; IMAGE=pk-seqam-ft
K=${1:?key}; SC=${2:?insync|lagging}; SK=${3:?skew in seconds}; RUN=${4:-1}
EXTRA=""
case $K in
  expression) CREATE="create sequence s"
              E="lpad((((((extract(epoch from clock_timestamp()) * 1000)::bigint - 1767225600000) << 22) | (1::bigint << 18) | (nextval('s') & 262143)))::text, 20, '0')" ;;
  uuidv7)     CREATE="select 1"; E="uuidv7()::text" ;;
  regionseq)  CREATE="create extension regionseq; create sequence s using regionseq"; EXTRA=$(rsargs 1)
              E="lpad(nextval('s')::text, 20, '0')" ;;
  *) echo "unknown key $K"; exit 1 ;;
esac
NET=pknet; docker network create $NET >/dev/null 2>&1
TMPFS=1g
pg_start pk-fs-p $EXTRA
docker exec pk-fs-p sh -c 'echo "host replication all all trust" >> $PGDATA/pg_hba.conf'
psqlx pk-fs-p -c "select pg_reload_conf()" -c "$CREATE" >/dev/null
docker rm -f pk-fs-s >/dev/null 2>&1
docker run -d --name pk-fs-s --network $NET --tmpfs /var/lib/postgresql:rw,size=1g -e POSTGRES_PASSWORD=x \
  --entrypoint bash $IMAGE -c 'mkdir -p $PGDATA && chown postgres:postgres $PGDATA && chmod 700 $PGDATA &&
    gosu postgres pg_basebackup -h pk-fs-p -U postgres -D $PGDATA -R -X stream &&
    echo "-'$SK's" > /tmp/ft && chmod 666 /tmp/ft &&
    LD_PRELOAD=$(ls /usr/lib/*/faketime/libfaketime.so.1) FAKETIME_TIMESTAMP_FILE=/tmp/ft FAKETIME_NO_CACHE=1 exec gosu postgres postgres $0' "$EXTRA" >/dev/null
until docker exec pk-fs-s pg_isready -q -U postgres 2>/dev/null; do sleep 1; done
sleep 2
if [ $SC = lagging ]; then
  psqlx pk-fs-s -c "alter system set primary_conninfo = 'host=nowhere user=postgres'" -c "select pg_reload_conf()" >/dev/null
  sleep 3
fi
( echo "select $E from generate_series(1, 1000) \\watch 0.01" ) \
  | docker exec -i pk-fs-p psql -X -At -U postgres 2>/dev/null | grep -E '^[0-9a-f-]{20,}$' > /tmp/pk-fs-got.txt &
sleep 3
docker kill pk-fs-p >/dev/null
wait
sed -i.bak '$d' /tmp/pk-fs-got.txt; rm -f /tmp/pk-fs-got.txt.bak      # the last line may be cut
psqlx pk-fs-s -At -c "select pg_promote(wait_seconds => 30)" >/dev/null
psqlx pk-fs-s -c "create table old (id text)" -c "create table new as select $E as id from generate_series(1, 200000)"
docker exec -i pk-fs-s psql -X -q -U postgres -c "copy old from stdin" < /tmp/pk-fs-got.txt
echo "== key=$K standby=$SC standby clock behind by ${SK}s run=$RUN"
psqlx pk-fs-s -At -F ' | ' <<'SQL'
select 'ids from the old primary', count(*), 'distinct', count(distinct id) from old;
select 'new ids', count(*), 'repeating an old id', (select count(*) from new join old using (id)),
       'below the highest old id', (select count(*) from new where id collate "C" < (select max(id collate "C") from old)) from new;
SQL
docker rm -f pk-fs-p pk-fs-s >/dev/null

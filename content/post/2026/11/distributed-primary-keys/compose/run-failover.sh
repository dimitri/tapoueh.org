#!/bin/bash
# What a promoted standby's sequence hands out compared with what the old primary already handed out.
# ./run-failover.sh <insync|lagging> [RUN]
cd "$(dirname "$0")"; . ./lib.sh
SC=${1:?insync|lagging}; RUN=${2:-1}
# third argument "snowam" or "regionam" (our own, extension/): the snowflake sequence access method instead of a plain sequence (IMAGE=pk-seqam)
SEQ=${3:-plain}
# "snowflake": the time, region, counter bigint of the article, made from a plain sequence in an expression
EXPR="nextval('s')"
if [ $SEQ = snowflake ]; then EXPR="(((extract(epoch from clock_timestamp()) * 1000)::bigint - 1767225600000) << 22) | (1::bigint << 18) | (nextval('s') & 262143)"; fi
EXTRA=""
if [ $SEQ = regionam ]; then CREATE="create extension regionseq; create sequence s using regionseq"; EXTRA=$(rsargs 1)
elif [ $SEQ = snowam ]; then CREATE="create extension snowflake; create sequence s using snowflake"; else CREATE="create sequence s"; fi
NET=pknet; docker network create $NET >/dev/null 2>&1
TMPFS=1g
pg_start pk-fo-p $EXTRA
docker exec pk-fo-p sh -c 'echo "host replication all all trust" >> $PGDATA/pg_hba.conf'
psqlx pk-fo-p -c "select pg_reload_conf()" -c "$CREATE" >/dev/null
docker rm -f pk-fo-s >/dev/null 2>&1
docker run -d --name pk-fo-s --network $NET --tmpfs /var/lib/postgresql:rw,size=1g -e POSTGRES_PASSWORD=x \
  --entrypoint bash $IMAGE -c 'mkdir -p $PGDATA && chown postgres:postgres $PGDATA && chmod 700 $PGDATA &&
    gosu postgres pg_basebackup -h pk-fo-p -U postgres -D $PGDATA -R -X stream &&
    exec gosu postgres postgres $0' "$EXTRA" >/dev/null
until docker exec pk-fo-s pg_isready -q -U postgres 2>/dev/null; do sleep 1; done
sleep 2
echo "== scenario=$SC sequence=$SEQ run=$RUN; standby in recovery: $(psqlx pk-fo-s -At -c 'select pg_is_in_recovery()')"
if [ $SC = lagging ]; then
  psqlx pk-fo-s -c "alter system set primary_conninfo = 'host=nowhere user=postgres'" -c "select pg_reload_conf()" >/dev/null
  sleep 3
fi
# a client takes numbers from the sequence as fast as it can; what it received is kept on this side
( echo "select $EXPR from generate_series(1, 1000) \\watch 0.01" ) \
  | docker exec -i pk-fo-p psql -X -At -U postgres > /tmp/pk-fo-got.txt 2>/tmp/pk-fo-err.txt &
sleep 3
docker kill pk-fo-p >/dev/null
wait
LAST=$(tail -n 1 /tmp/pk-fo-got.txt)
psqlx pk-fo-s -At -c "select pg_promote(wait_seconds => 30)" >/dev/null
FIRST=$(psqlx pk-fo-s -At -c "select $EXPR")
echo "last number the client received from the old primary: $LAST"
echo "first number the promoted standby hands out:           $FIRST"
echo "difference: $((FIRST - LAST))  (negative or zero: numbers handed out twice; positive: numbers skipped, minus one)"
docker rm -f pk-fo-p pk-fo-s >/dev/null

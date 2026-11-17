#!/bin/bash
# A publication that filters by region on the key alone: no region column, no companion index, replica identity = the primary key.
# ./run-region-filter.sh <regionseq|expression>
#   regionseq:  ids from the regionseq extension, region in bits 16..21  -> where ((id >> 16) & 63) = 1
#   expression: ids from the article's expression, region in bits 18..21 -> where ((id >> 18) & 15) = 1
# Publisher is region 1; rows of region 2 are put in with explicit ids, as a mesh would deliver them.
cd "$(dirname "$0")"; . ./lib.sh; IMAGE=pk-seqam
K=${1:?regionseq|expression}
NET=pknet; docker network create $NET >/dev/null 2>&1
if [ $K = regionseq ]; then
  EXTRA=$(rsargs 1); FILTER="((id >> 16) & 63) = 1"; SH=16
  NEW="nextval('s')"; DDL="create extension regionseq; create sequence s using regionseq;"
else
  EXTRA=""; FILTER="((id >> 18) & 15) = 1"; SH=18
  NEW="(((extract(epoch from clock_timestamp()) * 1000)::bigint - 1767225600000) << 22) | (1::bigint << 18) | (nextval('s') & 262143)"
  DDL="create sequence s;"
fi
FOREIGN="(((extract(epoch from clock_timestamp()) * 1000)::bigint - 1767225600000) << 22) | (2::bigint << $SH) | n"
pg_start pk-rfp -c wal_level=logical $EXTRA
pg_start pk-rfs
psqlx pk-rfp -c "$DDL" -c "create table t (id bigint primary key, payload text not null)" \
  -c "create publication p for table t where ($FILTER) with (publish = 'insert, update, delete')"
psqlx pk-rfs -c "create table t (id bigint primary key, payload text not null)"
psqlx pk-rfs -c "create subscription sub connection 'host=pk-rfp dbname=postgres user=postgres' publication p" >/dev/null
sleep 3
echo "== $K: filter ($FILTER), replica identity: $(psqlx pk-rfp -At -c "select relreplident from pg_class where relname = 't'") (d = primary key), indexes on t: $(psqlx pk-rfp -At -c "select count(*) from pg_indexes where tablename = 't'")"
psqlx pk-rfp <<SQL
insert into t select $NEW, 'own' from generate_series(1, 1000);
insert into t select $FOREIGN, 'foreign' from generate_series(1, 1000) n;
update t set payload = 'own, updated' where payload = 'own' and id in (select id from t where payload = 'own' limit 100);
update t set payload = 'foreign, updated' where payload = 'foreign' and id in (select id from t where payload = 'foreign' limit 100);
delete from t where payload = 'own' and id in (select id from t where payload = 'own' limit 50);
delete from t where payload = 'foreign' and id in (select id from t where payload = 'foreign' limit 50);
SQL
sleep 5
echo "publisher:"; psqlx pk-rfp -At -c "select payload, count(*) from t group by 1 order by 1"
echo "subscriber:"; psqlx pk-rfs -At -c "select payload, count(*) from t group by 1 order by 1"
psqlx pk-rfs -At -c "select 'apply errors', coalesce(sum(apply_error_count),0) from pg_stat_subscription_stats"
docker rm -f pk-rfp pk-rfs >/dev/null

#!/bin/bash
# The row filter example of the article, as the SQL the article prints: a publisher of region 1 whose publication filters on the region
# bits of the key, with the primary key as the replica identity, and a subscriber that receives the rows of region 1 only.
cd "$(dirname "$0")"; . ./lib.sh
NET=pknet; docker network create $NET >/dev/null 2>&1
pg_start pk-rfp -c wal_level=logical
pg_start pk-rfs
psqlx pk-rfp -c "alter database postgres set app.region = 1"
psqlx pk-rfp -c "create sequence s" -c "create table t (
    id bigint primary key default (
        ((extract(epoch from clock_timestamp() - timestamptz '2026-01-01 UTC') * 1000)::bigint << 22)
        | (current_setting('app.region')::bigint << 18)
        | (nextval('s') & 262143)
    ),
    region smallint not null,
    payload text not null
)"
psqlx pk-rfs -c "create table t (id bigint primary key, region smallint not null, payload text not null)"
psqlx pk-rfp <<'SQL'
create publication region_1
    for table t where (((id >> 18) & 15) = 1);
SQL
psqlx pk-rfs -c "create subscription region_1 connection 'host=pk-rfp dbname=postgres user=postgres' publication region_1" >/dev/null
sleep 3
psqlx pk-rfp <<'SQL'
\c postgres
-- 1,000 rows made here, with region 1 in the bits of their ids
insert into t (region, payload)
     select 1, 'mine' from generate_series(1, 1000);

-- 1,000 rows that came from region 2, with region 2 in the bits of their ids
insert into t (id, region, payload)
     select (2::bigint << 18) | n, 2, 'theirs' from generate_series(1, 1000) n;

-- change 100 rows of each, then delete 50 of each
update t set payload = payload || ', updated' where (id & 262143) <= 100;
delete from t where (id & 262143) between 101 and 150;
SQL
sleep 5
echo "publisher:"; psqlx pk-rfp -c "select payload, count(*) from t group by 1 order by 1"
echo "subscriber:"; psqlx pk-rfs -c "select payload, count(*) from t group by 1 order by 1"
psqlx pk-rfs -At -c "select 'apply errors', coalesce(sum(apply_error_count),0) from pg_stat_subscription_stats"
psqlx pk-rfp -At -c "select 'replica identity', relreplident, '(d = primary key), indexes on t:', (select count(*) from pg_indexes where tablename = 't') from pg_class where relname = 't'"
docker rm -f pk-rfp pk-rfs >/dev/null

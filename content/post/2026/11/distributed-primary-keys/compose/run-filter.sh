#!/bin/bash
# A uuid key under a row-filtered publication that also publishes UPDATE and DELETE.
cd "$(dirname "$0")"; . ./lib.sh
NET=pknet; docker network create $NET >/dev/null 2>&1
TMPFS=500m
pg_start pk-fa -c wal_level=logical
pg_start pk-fb -c wal_level=logical
DDL="create table t (id uuid primary key default uuidv7(), region smallint not null, payload text not null)"
psqlx pk-fa -c "$DDL"; psqlx pk-fb -c "$DDL"
echo "== uuid primary key only"
psqlx pk-fa -c "create publication p for table t where (region = 1)"
psqlx pk-fb -c "create subscription s connection 'host=pk-fa dbname=postgres user=postgres' publication p" >/dev/null
psqlx pk-fa -c "insert into t (region, payload) values (1, 'one'), (2, 'two')"
psqlx pk-fa -v ON_ERROR_STOP=0 -c "update t set payload = 'changed' where region = 1" 2>&1
echo "== with a companion unique index on (id, region) as the replica identity"
psqlx pk-fa -c "create unique index t_id_region on t (id, region)" -c "alter table t replica identity using index t_id_region"
psqlx pk-fa -c "update t set payload = 'changed' where region = 1" -c "delete from t where region = 2"
sleep 3
psqlx pk-fb -At -F ' | ' -c "select region, payload from t order by 1"
docker rm -f pk-fa pk-fb >/dev/null

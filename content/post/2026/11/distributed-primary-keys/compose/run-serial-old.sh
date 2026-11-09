#!/bin/bash
# A serial column created on Postgres 9.6, then dumped by the new pg_dump and restored on 19rc1.
cd "$(dirname "$0")"; . ./lib.sh
NET=pknet; docker network create $NET >/dev/null 2>&1
IMAGE=postgres:9.6 pg_start pk-old
echo "== on 9.6"
psqlx pk-old -v ON_ERROR_STOP=0 <<'SQL'
select version();
create table t (id serial primary key);
select a.attname, a.atttypid::regtype as column_type from pg_attribute a where a.attrelid = 't'::regclass and a.attname = 'id';
select (select max_value from t_id_seq) as sequence_max;
select setval('t_id_seq', 2147483647);
insert into t default values;
SQL
pg_start pk-new
echo "== pg_dump of the new server, schema only, binary-upgrade mode (what pg_upgrade runs)"
docker exec pk-new pg_dump -h pk-old -U postgres --schema-only --binary-upgrade postgres | grep -iE -B1 -A6 "CREATE SEQUENCE"
echo "== plain dump restored on 19rc1"
docker exec pk-new sh -c 'pg_dump -h pk-old -U postgres postgres | psql -X -q -U postgres postgres' 2>&1 | grep -v "^$" | head
psqlx pk-new -v ON_ERROR_STOP=0 <<'SQL'
select a.atttypid::regtype as column_type, s.seqtypid::regtype as sequence_type, s.seqmax from pg_attribute a, pg_sequence s
 where a.attrelid = 't'::regclass and a.attname = 'id' and s.seqrelid = 't_id_seq'::regclass;
select setval('t_id_seq', 2147483647);
insert into t default values;
SQL
docker rm -f pk-old pk-new >/dev/null

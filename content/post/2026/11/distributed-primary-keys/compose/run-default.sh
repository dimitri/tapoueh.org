#!/bin/bash
# A column default written as uuidv7() on 17 (the SQL function), then dumped by 19rc1's pg_dump and restored on 19rc1.
cd "$(dirname "$0")"; . ./lib.sh
NET=pknet; docker network create $NET >/dev/null 2>&1
TMPFS=500m
IMAGE=postgres:17 pg_start pk-d17
psqlx pk-d17 -f - < ../sql/20-uuidv7-sql.sql >/dev/null 2>&1
psqlx pk-d17 -c "create table t (id uuid primary key default uuidv7(), payload text)"
echo "== on 17, the default as stored"
psqlx pk-d17 -At -c "select pg_get_expr(adbin, adrelid) from pg_attrdef where adrelid = 't'::regclass"
pg_start pk-d19
echo "== pg_dump of 19rc1 against 17, schema only"
docker exec pk-d19 pg_dump -h pk-d17 -U postgres --schema-only postgres | grep -iE "DEFAULT|FUNCTION public"
docker exec pk-d19 sh -c 'pg_dump -h pk-d17 -U postgres postgres | psql -X -q -U postgres postgres' >/dev/null 2>&1
echo "== on 19rc1 after the restore: which function the default calls"
psqlx pk-d19 -At -c "select pg_get_expr(adbin, adrelid) from pg_attrdef where adrelid = 't'::regclass" \
  -c "select refobjid::regprocedure from pg_depend d join pg_attrdef a on a.oid = d.objid where a.adrelid = 't'::regclass and d.refclassid = 'pg_proc'::regclass"
echo "== after dropping the SQL function and setting the default again"
psqlx pk-d19 -At -c "alter table t alter column id drop default" -c "drop function public.uuidv7()" -c "alter table t alter column id set default uuidv7()" \
  -c "select pg_get_expr(adbin, adrelid) from pg_attrdef where adrelid = 't'::regclass" \
  -c "select refobjid::regprocedure from pg_depend d join pg_attrdef a on a.oid = d.objid where a.adrelid = 't'::regclass and d.refclassid = 'pg_proc'::regclass"
docker rm -f pk-d17 pk-d19 >/dev/null

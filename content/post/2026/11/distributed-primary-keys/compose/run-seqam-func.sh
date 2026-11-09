#!/bin/bash
# What the snowflake sequence AM of the v29 patches does: ids, column widths, limits, dump/restore, crash.
# Needs IMAGE=pk-seqam (seqam/Dockerfile).
cd "$(dirname "$0")"; . ./lib.sh; IMAGE=pk-seqam
N=pk-sa; pg_start $N -c max_wal_size=2GB
echo "== version, and the sequence relation"
psqlx $N -c "select version()" -c "create extension snowflake" -c "create sequence sf using snowflake" -c "\\d+ sf" -c "select * from sf"
echo "== one id, decoded; identity column through default_sequence_access_method"
psqlx $N <<'SQL'
select nextval('sf') as id;
select to_timestamp(time_ms / 1000.0) at time zone 'utc' as at, machine, counter from snowflake_get(nextval('sf'));
set default_sequence_access_method = 'snowflake';
create table t (id bigint generated always as identity primary key, v text);
insert into t (v) select 'x' from generate_series(1, 3);
select id, (snowflake_get(id)).* from t;
SQL
echo "== column widths: int and smallint identity columns (the sequence is typed integer, maximum 2147483647)"
psqlx $N <<'SQL'
set default_sequence_access_method = 'snowflake';
create table ti (a int generated always as identity primary key);
create table ts (a smallint generated always as identity primary key);
insert into ti default values; insert into ts default values;
select 'int column holds', a from ti;
select 'smallint column holds', a from ts;
do $$ declare n int := 0; begin
  loop insert into ti default values; n := n + 1; end loop;
exception when unique_violation then raise notice 'int column: duplicate key after % rows', n; end $$;
do $$ declare n int := 0; begin
  loop insert into ts default values; n := n + 1; end loop;
exception when unique_violation then raise notice 'smallint column: duplicate key after % rows', n;
  when numeric_value_out_of_range then raise notice 'smallint column: out of range after % rows', n; end $$;
SQL
echo "== the state is one small counter: setval"
psqlx $N -c "select * from sf" -c "select setval('sf', 4000)" -c "select * from sf" -c "select (snowflake_get(nextval('sf'))).counter"
echo "== speed of nextval in one session: 1,000,000 values"
psqlx $N <<'SQL'
create sequence plain;
\timing on
select count(nextval('plain')) from generate_series(1, 1000000);
select count(nextval('sf')) from generate_series(1, 1000000);
select count(nextval('plain')) from generate_series(1, 1000000);
select count(nextval('sf')) from generate_series(1, 1000000);
SQL
echo "== pg_dump and restore"
docker exec $N pg_dump -U postgres --schema-only -t t -t t_id_seq -t sf postgres | grep -v '^--' | grep -v '^$'
psqlx $N -c "create database copy"
docker exec $N sh -c 'pg_dump -U postgres -Fc postgres | pg_restore -U postgres -d copy'
psqlx $N -d copy -c "select amname, amtype from pg_am where amname = 'snowflake'" -c "select relname, relpersistence, am.amname from pg_class c join pg_am am on am.oid = c.relam where relname in ('sf','t_id_seq')" -c "select (snowflake_get(nextval('t_id_seq'))).counter"
echo "== kill -9 of a server process, then crash recovery"
psqlx $N -c "select nextval('sf') as before_crash, (select count from sf) as counter_before"
# kill -9 on a child process: the postmaster runs crash recovery
docker exec $N sh -c 'for p in /proc/[0-9]*; do c=$(tr "\0" " " < $p/cmdline 2>/dev/null); case "$c" in *"postgres: checkpointer"*) [ ${p#/proc/} != $$ ] && kill -9 ${p#/proc/};; esac; done' 
sleep 3; until docker exec $N pg_isready -q -U postgres; do sleep 1; done; sleep 2
echo "crash recoveries in the server log: $(docker logs $N 2>&1 | grep -c "not properly shut down")"
psqlx $N -c "select * from sf" -c "select nextval('sf') as after_crash"
echo "== MAXVALUE is not enforced by nextval, and a dump of such a sequence does not restore"
psqlx $N -c "alter sequence sf restart" -c "alter sequence sf maxvalue 100" -c "select max(nextval('sf')) as above_maxvalue from generate_series(1, 150)" -c "select * from sf"
psqlx $N -c "drop database copy" -c "create database copy"
docker exec $N sh -c 'pg_dump -U postgres -Fc postgres | pg_restore -U postgres -d copy' 2>&1 | grep -E "error|warning"
docker rm -f $N >/dev/null

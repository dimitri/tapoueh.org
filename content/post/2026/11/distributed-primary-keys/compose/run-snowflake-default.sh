#!/bin/bash
# The time, region, counter bigint of the article as a column default (region from the app.region setting, origin in UTC),
# against the same bits written as an expression in each INSERT, which is what the other scripts here do.
# ./run-snowflake-default.sh
cd "$(dirname "$0")"; . ./lib.sh
N=pk-sfdef
pg_start $N -c max_wal_size=4GB
psqlx $N <<'SQL'
alter database postgres set app.region = 3;
\c postgres
select current_setting('app.region')::bigint as region_from_the_database_setting;
select current_setting('region') as plain_name \gset
SQL
psqlx $N -v ON_ERROR_STOP=0 <<'SQL'
\c postgres
create sequence s_default;
create table t_default (
  id bigint primary key default (
    ((extract(epoch from clock_timestamp() - timestamptz '2026-01-01 UTC') * 1000)::bigint << 22)
    | (current_setting('app.region')::bigint << 18)
    | (nextval('s_default') & 262143)
  ),
  region smallint not null, payload text not null);
create sequence s_inline;
create table t_inline (id bigint primary key, region smallint not null, payload text not null);

insert into t_default (region, payload) select 3, 'x' from generate_series(1, 3);
select (id >> 22) as ms_since_2026, (id >> 18) & 15 as region, id & 262143 as counter from t_default;

-- the origin does not follow the session's time zone once it is spelled in UTC
set timezone = 'Asia/Tokyo';
select extract(epoch from timestamptz '2026-01-01') * 1000 as bare_origin_ms,
       extract(epoch from timestamptz '2026-01-01 UTC') * 1000 as utc_origin_ms;
reset timezone;

truncate t_default;
\timing on
insert into t_default (region, payload) select 3, 'x' from generate_series(1, 2000000);
insert into t_inline select (((extract(epoch from clock_timestamp()) * 1000)::bigint - 1767225600000) << 22) | (3::bigint << 18) | (nextval('s_inline') & 262143), 3, 'x' from generate_series(1, 2000000);
insert into t_default (region, payload) select 3, 'x' from generate_series(1, 2000000);
insert into t_inline select (((extract(epoch from clock_timestamp()) * 1000)::bigint - 1767225600000) << 22) | (3::bigint << 18) | (nextval('s_inline') & 262143), 3, 'x' from generate_series(1, 2000000);
\timing off
select 'default' as how, pg_size_pretty(pg_relation_size('t_default_pkey')) as index_size, pg_size_pretty(pg_table_size('t_default')) as table_size
union all
select 'inline', pg_size_pretty(pg_relation_size('t_inline_pkey')), pg_size_pretty(pg_table_size('t_inline'));
SQL
docker rm -f $N >/dev/null

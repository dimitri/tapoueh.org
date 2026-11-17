#!/bin/bash
# What one row and one primary key entry take on a page, under each key, read with pageinspect.
# Then how full the primary key's leaf pages stay with four writers (pgstattuple), 1,000,000 rows.
set -e
cd "$(dirname "$0")"; . ./lib.sh
N=pk-sizes
pg_start $N
COLS="region smallint not null, payload text not null"
SNOW="((extract(epoch from clock_timestamp() - timestamptz '2026-01-01 UTC') * 1000)::bigint << 22) | (:r::bigint << 18) | (nextval(('s5_r' || :r)::regclass) & 262143)"
psqlx $N <<SQL
create extension pageinspect;
create extension pgstattuple;
create sequence s1_r0 increment 10 start 1; create sequence s1_r1 increment 10 start 2;
create sequence s1_r2 increment 10 start 3; create sequence s1_r3 increment 10 start 4;
create sequence s2_r0; create sequence s2_r1; create sequence s2_r2; create sequence s2_r3;
create sequence s5_r0; create sequence s5_r1; create sequence s5_r2; create sequence s5_r3;
create sequence s6_r0; create sequence s6_r1; create sequence s6_r2; create sequence s6_r3;
create table t1 (id bigint primary key, $COLS);
create table t2 (region smallint not null, id bigint not null, payload text not null, primary key (region, id));
create table t3 (id uuid primary key, $COLS);
create table t4 (id uuid primary key, $COLS);
create table t5 (id bigint primary key, $COLS);
-- the composite key again, with the bigint declared first in the table
create table t6 (id bigint not null, region smallint not null, payload text not null, primary key (region, id));
SQL
cat > /tmp/pk-sizes.sql <<SQL
\set r :client_id % 4
insert into t1 select nextval(('s1_r' || :r)::regclass), :r, 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx' from generate_series(1, 100);
insert into t2 select :r, nextval(('s2_r' || :r)::regclass), 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx' from generate_series(1, 100);
insert into t3 select gen_random_uuid(), :r, 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx' from generate_series(1, 100);
insert into t4 select uuidv7(), :r, 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx' from generate_series(1, 100);
insert into t5 select $SNOW, :r, 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx' from generate_series(1, 100);
insert into t6 select nextval(('s6_r' || :r)::regclass), :r, 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx' from generate_series(1, 100);
SQL
docker cp /tmp/pk-sizes.sql $N:/tmp/pk-sizes.sql
docker exec $N pgbench -n -U postgres -d postgres -c 4 -j 4 -t 2500 -f /tmp/pk-sizes.sql > /tmp/pgbench.out 2>&1 || { cat /tmp/pgbench.out; exit 1; }
echo "== one row and one primary key entry, in bytes; then 1,000,000 rows from four writers"
echo "table | row data (lp_len) | row on the page (8-byte aligned) | line pointer | rows per heap page | pkey entry (itemlen) | pkey MB | leaf density %"
psqlx $N -At -F ' | ' <<'SQL'
select t, h.lp_len, (h.lp_len + 7) / 8 * 8, 4,
       (select count(*) from heap_page_items(get_raw_page(t, 1))),
       (select itemlen from bt_page_items(t || '_pkey', 1) offset 1 limit 1),
       round(pg_relation_size(t || '_pkey') / 1048576.0, 1),
       (pgstatindex(t || '_pkey')).avg_leaf_density
  from unnest(array['t1', 't2', 't3', 't4', 't5', 't6']) t,
       lateral (select * from heap_page_items(get_raw_page(t, 1)) where lp = 1) h;
select 'heap tuple header (t_hoff)', t_hoff from heap_page_items(get_raw_page('t1', 1)) where lp = 1;
SQL
docker rm -f $N >/dev/null

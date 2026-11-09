#!/bin/bash
# One server, four writers (one per region), the same table under each key.
# ./run-single.sh <variant> [RUN]   variants: see the case statement.
set -e
cd "$(dirname "$0")"; . ./lib.sh
V=${1:?variant}; RUN=${2:-1}
ROWS_PER_TX=100; TX=${TX:-12500}      # 4 clients x 12,500 x 100 = 5,000,000 rows

SEQS="create sequence s_r0; create sequence s_r1; create sequence s_r2; create sequence s_r3;"
STEP="create sequence s_r0 increment 10 start 1; create sequence s_r1 increment 10 start 2;
      create sequence s_r2 increment 10 start 3; create sequence s_r3 increment 10 start 4;"
NEXT="nextval(('s_r' || :r)::regclass)"
SNOW="(((extract(epoch from clock_timestamp()) * 1000)::bigint - 1767225600000) << 22) | (:r::bigint << 18) | (nextval(('s_r' || :r)::regclass) & 262143)"
COMPANION="create unique index t_id_region on t (id, region); alter table t replica identity using index t_id_region;"
COLS="region smallint not null, payload text not null"
PRE=""
BASE=${V%+}                               # a trailing + means: with the companion index
case $BASE in
  seqstep)   DDL="$STEP create table t (id bigint primary key, $COLS);";           EXPR="$NEXT" ;;
  composite) DDL="$SEQS create table t (region smallint not null, id bigint not null, payload text not null, primary key (region, id));"; EXPR="$NEXT" ;;
  uuidv4)    DDL="create table t (id uuid primary key, $COLS);";                   EXPR="gen_random_uuid()" ;;
  uuidv7)    DDL="create table t (id uuid primary key, $COLS);";                   EXPR="uuidv7()" ;;
  snowflake) DDL="$SEQS create table t (id bigint primary key, $COLS);";           EXPR="$SNOW" ;;
  # IMAGE=pk-seqam (seqam/Dockerfile): the snowflake sequence access method of the v29 patches,
  # and the same bit layout from a default expression, both on ONE shared sequence.
  snowam)    DDL="create extension snowflake; create sequence sf using snowflake; create table t (id bigint primary key, $COLS);"
             EXPR="nextval('sf')"; PRE="set snowflake.machine_id = :r;" ;;
  snowone)   DDL="create sequence s_r0; create table t (id bigint primary key, $COLS);"
             EXPR="(((extract(epoch from clock_timestamp()) * 1000)::bigint - 1767225600000) << 22) | (:r::bigint << 18) | (nextval('s_r0') & 262143)" ;;
  *) echo "unknown variant $V"; exit 1 ;;
esac
[ "$V" != "$BASE" ] && DDL="$DDL $COMPANION"

N=pk-single
pg_start $N -c max_wal_size=6GB -c checkpoint_timeout=15min -c track_io_timing=on
psqlx $N -c "$DDL"
if [ "$BASE" = composite ]; then
  IDCOL="region, id, payload"; SEL="select :r, $EXPR, 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx'"
else
  IDCOL="id, region, payload";  SEL="select $EXPR, :r, 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx'"
fi
cat > /tmp/pk-single.sql <<SQL
\set r :client_id % 4
$PRE
insert into t ($IDCOL) $SEL from generate_series(1, $ROWS_PER_TX);
SQL
docker cp /tmp/pk-single.sql $N:/tmp/pk-single.sql
psqlx $N -c "checkpoint" -c "select pg_stat_reset(), pg_stat_reset_shared('io'), pg_stat_reset_shared('wal')" >/dev/null
echo "== variant=$V run=$RUN"
docker exec $N pgbench -n -U postgres -d postgres -c 4 -j 4 -t $TX -f /tmp/pk-single.sql > /tmp/pgbench.out 2>&1 || { cat /tmp/pgbench.out; exit 1; }
grep -E "^(number of transactions actually|latency average|tps)" /tmp/pgbench.out
REPORT() { psqlx $N -At -F ' | ' <<'SQL'
select 'rows', count(*) from t;
select 'table_bytes', pg_relation_size('t');
select 'index_' || indexrelname, pg_relation_size(indexrelid)
  from pg_stat_user_indexes where relname = 't' order by 1;
select 'wal_bytes', wal_bytes, 'wal_fpi', wal_fpi, 'wal_records', wal_records from pg_stat_wal;
select 'idx_blks_hit+read ' || indexrelname, idx_blks_hit, idx_blks_read
  from pg_statio_user_indexes where relname = 't' order by 1;
select 'io_relation reads', sum(reads), 'writes', sum(writes), 'evictions', sum(evictions)
  from pg_stat_io where object = 'relation';
SQL
}
REPORT
# Phase 2: the first 500,000 rows after a checkpoint, into the table that now holds 5,000,000.
# The first change to a page after a checkpoint writes the whole page to the WAL (full-page image).
psqlx $N -c "checkpoint" -c "select pg_stat_reset(), pg_stat_reset_shared('io'), pg_stat_reset_shared('wal')" >/dev/null
echo "== phase 2: 500,000 rows after a checkpoint"
docker exec $N pgbench -n -U postgres -d postgres -c 4 -j 4 -t 1250 -f /tmp/pk-single.sql > /tmp/pgbench.out 2>&1 || { cat /tmp/pgbench.out; exit 1; }
grep -E "^(latency average|tps)" /tmp/pgbench.out
psqlx $N -At -F ' | ' <<'SQL'
select 'wal_bytes', wal_bytes, 'wal_fpi', wal_fpi, 'wal_records', wal_records from pg_stat_wal;
select 'idx_blks_hit+read ' || indexrelname, idx_blks_hit, idx_blks_read
  from pg_statio_user_indexes where relname = 't' order by 1;
SQL
docker rm -f $N >/dev/null

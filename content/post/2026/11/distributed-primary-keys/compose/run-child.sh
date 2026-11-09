#!/bin/bash
# What a key costs in the tables that point at it: 1,000,000 parents, 5,000,000 children.
cd "$(dirname "$0")"; . ./lib.sh
pg_start pk-child -c max_wal_size=1GB -c shared_buffers=1GB
P=1000000; C=5000000
PAY="'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx'"
# run <name> <parent key columns> <parent key list> <parent key values> <child fk columns> <child fk list> <child fk values> <join condition>
run() {
  local name=$1 pcols=$2 pk=$3 pvals=$4 ccols=$5 fk=$6 cvals=$7 on=$8
  echo "== $name"
  psqlx pk-child -At -F ' | ' <<SQL
drop table if exists child; drop table if exists parent; checkpoint;
create table parent ($pcols, payload text not null, primary key ($pk));
create table child (cid bigint generated always as identity primary key, $ccols, payload text not null,
                    foreign key ($fk) references parent ($pk));
insert into parent select $pvals, $PAY from generate_series(1, $P) g;
insert into child ($fk, payload) select $cvals, $PAY from generate_series(1, $C) g;
create index on child ($fk);
vacuum analyze parent, child;
select 'parent heap', pg_relation_size('parent') union all
select 'parent indexes', pg_indexes_size('parent') union all
select 'child heap', pg_relation_size('child') union all
select 'child indexes (pk + fk)', pg_indexes_size('child');
SQL
  for k in 1 2 3; do
    psqlx pk-child -At -c "set max_parallel_workers_per_gather = 0" \
      -c "explain (analyze, timing off, summary on) select count(*) from child c join parent p on $on where p.payload is not null" \
      | grep "Execution Time" | sed "s/^/join run $k: /"
  done
}
Q="((g % $P) + 1)"
R="(array['us-east','us-west','ap-south','ap-southeast'])"
run "bigint key" \
    "id bigint" "id" "g" \
    "parent_id bigint not null" "parent_id" "$Q" "c.parent_id = p.id"
run "uuid key (the same 16 bytes for v4 and v7)" \
    "id uuid" "id" "md5(g::text)::uuid" \
    "parent_id uuid not null" "parent_id" "md5($Q::text)::uuid" "c.parent_id = p.id"
run "composite (region smallint, id bigint), columns in key order" \
    "region smallint, id bigint" "region, id" "(g % 4)::smallint, g" \
    "parent_region smallint not null, parent_id bigint not null" "parent_region, parent_id" "($Q % 4)::smallint, $Q" \
    "c.parent_region = p.region and c.parent_id = p.id"
run "composite (region smallint, id bigint), bigint first in the table" \
    "id bigint, region smallint" "region, id" "g, (g % 4)::smallint" \
    "parent_id bigint not null, parent_region smallint not null" "parent_region, parent_id" "($Q % 4)::smallint, $Q" \
    "c.parent_region = p.region and c.parent_id = p.id"
run "composite (region text, id bigint)" \
    "region text, id bigint" "region, id" "$R[(g % 4) + 1], g" \
    "parent_region text not null, parent_id bigint not null" "parent_region, parent_id" "$R[($Q % 4) + 1], $Q" \
    "c.parent_region = p.region and c.parent_id = p.id"
docker rm -f pk-child >/dev/null

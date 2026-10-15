#!/usr/bin/env bash
# Profile d: DDL as a message, not a queue table — against the real
# implementation (pgcopydb PR #1064), not the design doc.
#
# Order matters, found the hard way: the event trigger has to go in
# *after* clone --follow has already done its initial schema+data copy
# and settled into streaming, not before clone starts (clone's own
# schema snapshot gets confused by the pgcopydb schema the trigger
# installs) and not as a separate `clone` + `follow` pair either (follow
# on its own doesn't accept a snapshot clone already consumed).
set -euo pipefail
cd "$(dirname "$0")"

PROJECT=lrdist
SRC_URI="postgres://postgres:postgres@d-source:5432/postgres"
TGT_URI="postgres://postgres:postgres@d-target:5432/postgres"

dc() { docker compose -p "$PROJECT" "$@"; }

echo "== schema + seed data on the source =="
dc exec -T d-source psql -U postgres -v ON_ERROR_STOP=1 -q < sql/d-01-schema.sql

echo "== pgcopydb clone --follow, in the background =="
mkdir -p results
dc exec -d pgcopydb sh -c "
  pgcopydb clone --follow --source '$SRC_URI' --target '$TGT_URI' \
    > /results/d-clone-follow.log 2>&1
"

echo "== waiting for the initial clone to land on the target =="
landed=0
for i in $(seq 1 60); do
  n=$(dc exec -T d-target psql -U postgres -t -A -c "select count(*) from shop" 2>/dev/null || echo 0)
  if [ "$n" = "20" ]; then landed=1; echo "target has 20 rows after ${i}s"; break; fi
  sleep 1
done
if [ "$landed" != "1" ]; then
  echo "clone never landed -- see results/d-clone-follow.log"; exit 1
fi

echo "== settling into streaming mode =="
sleep 5

echo "== NOW install the event trigger, mid-stream =="
dc exec -T pgcopydb pgcopydb stream event-trigger --source "$SRC_URI"

echo "== issuing a real ALTER TABLE on the source, zero manual steps on the target =="
dc exec -T d-source psql -U postgres -c "alter table shop add column price numeric(10,2) default 0;"
dc exec -T d-source psql -U postgres -c "update shop set price = id * 1.5 where id <= 5;"

echo "== waiting for the column to show up on the target =="
got_column=0
for i in $(seq 1 30); do
  has_col=$(dc exec -T d-target psql -U postgres -t -A -c "
    select count(*) from information_schema.columns
    where table_name = 'shop' and column_name = 'price';
  " 2>/dev/null || echo 0)
  if [ "$has_col" = "1" ]; then got_column=1; echo "column landed after ${i}s"; break; fi
  sleep 1
done

sleep 3
{
  echo "=== information_schema.columns on the target, 'shop' table ==="
  dc exec -T d-target psql -U postgres -c "
    select column_name, data_type from information_schema.columns
    where table_name = 'shop' order by ordinal_position;
  "
  echo
  echo "=== rows with the new column populated, target ==="
  dc exec -T d-target psql -U postgres -c "select id, name, price from shop order by id limit 6;" 2>&1 || true
  echo
  echo "=== got_column=$got_column (1 means the DDL message round-trip actually worked) ==="
} | tee results/d-$(date +%Y%m%d-%H%M%S).out

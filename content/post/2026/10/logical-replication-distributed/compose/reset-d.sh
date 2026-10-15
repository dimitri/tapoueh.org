#!/usr/bin/env bash
# Full teardown of profile d's replication state, so run-d.sh can start clean.
set -uo pipefail
cd "$(dirname "$0")"
PROJECT=lrdist
dc() { docker compose -p "$PROJECT" "$@"; }

dc exec -T pgcopydb sh -c "pkill -f 'pgcopydb (clone|follow)' 2>/dev/null; true"
sleep 1 2>/dev/null || true

dc exec -T d-target psql -U postgres -c "select pg_replication_origin_drop(roname) from pg_replication_origin;" 2>/dev/null
dc exec -T d-target psql -U postgres -c "
  drop table if exists shop;
  drop publication if exists pgcopydb;
" 2>/dev/null
# the pgcopydb schema (event trigger + function) lives on the source only,
# but clone's own schema dump/restore copies the pgcopydb *publication*
# object onto the target too -- leftover from a prior run makes the next
# clone's post-data restore fail with "publication already exists",
# which this reset used to miss entirely.

dc exec -T d-source psql -U postgres -c "
  select pg_drop_replication_slot(slot_name) from pg_replication_slots;
  drop publication if exists pgcopydb;
  drop schema if exists pgcopydb cascade;
  drop table if exists shop;
" 2>/dev/null

dc exec -T pgcopydb sh -c "rm -rf /root/.local/share/pgcopydb /tmp/pgcopydb" 2>/dev/null
echo "reset done"

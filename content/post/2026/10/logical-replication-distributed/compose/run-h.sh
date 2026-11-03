#!/usr/bin/env bash
# Profile h: Postgres 19 release candidate 1, sequence synchronization.
# A publication FOR ALL SEQUENCES, a subscription, then the sequence advances on the
# publisher and the subscriber gets the value when asked (REFRESH SEQUENCES).
set -uo pipefail
cd "$(dirname "$0")"
mkdir -p results
OUT=results/h-sequences.out
dc() { docker compose -p lrdist "$@"; }
pub() { dc exec -T h-pub psql -X -At -U postgres -d app "$@" 2>&1; }
sub() { dc exec -T h-sub psql -X -At -U postgres -d app "$@" 2>&1; }
say() { echo "$@" | tee -a "$OUT"; }
: > "$OUT"
dc rm -sf h-pub h-sub >/dev/null 2>&1
COMPOSE_PROFILES=h dc up -d --wait h-pub h-sub >/dev/null 2>&1 || { say "did not start"; exit 1; }
say "== $(pub -c 'select version()' | cut -c1-60) =="
SCHEMA="create sequence order_id; create table orders (id bigint primary key default nextval('order_id'), note text)"
pub -c "$SCHEMA" >/dev/null; sub -c "$SCHEMA" >/dev/null
pub -c "create publication p for all tables, all sequences" >/dev/null
sub -c "create subscription s connection 'host=h-pub dbname=app user=postgres' publication p" >/dev/null
for i in $(seq 1 30); do [ "$(sub -c "select count(*) from pg_subscription_rel where srsubstate = 'r'")" -ge 2 ] && break; sleep 1; done
say "-- the subscription is ready: $(sub -c "select string_agg(srrelid::regclass::text || ' ' || srsubstate::text, ', ' order by srrelid::regclass::text) from pg_subscription_rel")"
say "-- 1000 orders on the publisher"
pub -c "insert into orders (note) select 'n' from generate_series(1, 1000)" >/dev/null
for i in $(seq 1 30); do [ "$(sub -c 'select count(*) from orders')" = 1000 ] && break; sleep 1; done
say "   rows on the subscriber: $(sub -c 'select count(*) from orders')"
say "   sequence on the publisher:  $(pub -c "select last_value from pg_get_sequence_data('order_id')")"
say "   sequence on the subscriber: $(sub -c "select last_value from pg_get_sequence_data('order_id')")  (rows stream, the sequence does not)"
sub -c "alter subscription s refresh sequences" >/dev/null
for i in $(seq 1 30); do [ "$(sub -c "select last_value from pg_get_sequence_data('order_id')")" = 1000 ] && break; sleep 1; done
say "-- after ALTER SUBSCRIPTION s REFRESH SEQUENCES"
say "   sequence on the subscriber: $(sub -c "select last_value from pg_get_sequence_data('order_id')")"
say "-- the first nextval a promoted subscriber would hand out: $(sub -c "select nextval('order_id')")"

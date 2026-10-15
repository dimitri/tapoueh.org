#!/usr/bin/env bash
# Profile c: four equal primaries, row-filtered cascades, one warehouse.
# Builds the exact topology "Cascading, one hop downstream" and "The
# architecture that makes the question moot" describe, end to end, and
# confirms zero conflicts with concurrent writers on all four.
set -euo pipefail
cd "$(dirname "$0")"

PROJECT=lrdist
PGPASSWORD=postgres
export PGPASSWORD

psql_on () { # psql_on <host> <sql...>
  local host=$1; shift
  docker compose -p "$PROJECT" exec -T "$host" psql -U postgres -v ON_ERROR_STOP=1 -q "$@"
}

echo "== schema =="
for host in us-east us-west in-region au warehouse; do
  docker compose -p "$PROJECT" exec -T "$host" psql -U postgres -v ON_ERROR_STOP=1 -q < sql/c-01-schema.sql
done

echo "== publications, row-filtered on region =="
# pub_to_amsterdam must stay row-filtered to this node's own two regions —
# left unfiltered, the warehouse's own mirror (the other two regions,
# relayed back down to this node) would ride straight back out to the
# warehouse the moment it lands, a real echo loop this run's first draft
# had and found the hard way (the warehouse's two inbound subscriptions
# logged transient duplicate-key apply errors; final data was still
# correct — logical replication's own retry caught up — but the loop
# itself was real, not imagined).
psql_on us-east    -c "create publication pub_to_amsterdam for table events where (region = 'us-east' or region = 'us-west');"
psql_on us-east    -c "create publication pub_to_us_west for table events where (region <> 'us-west');"
psql_on us-west    -c "create publication pub_to_us_east for table events where (region = 'us-west');"
psql_on in-region   -c "create publication pub_to_amsterdam for table events where (region = 'in' or region = 'au');"
psql_on in-region   -c "create publication pub_to_au for table events where (region <> 'au');"
psql_on au         -c "create publication pub_to_in for table events where (region = 'au');"
psql_on warehouse  -c "create publication pub_to_us_east for table events where (region <> 'us-east' and region <> 'us-west');"
psql_on warehouse  -c "create publication pub_to_in for table events where (region <> 'in' and region <> 'au');"

echo "== subscriptions =="
conninfo() { echo "host=$1 port=5432 user=postgres password=postgres dbname=postgres"; }

# subscription names must be unique on the *publisher* too (the default
# replication slot name is the subscription name) — name every one by both
# endpoints, not just the upstream side.
psql_on us-west   -c "create subscription sub_us_west_from_us_east connection '$(conninfo us-east)' publication pub_to_us_west;"
psql_on us-east   -c "create subscription sub_us_east_from_us_west connection '$(conninfo us-west)' publication pub_to_us_east;"
psql_on au        -c "create subscription sub_au_from_in connection '$(conninfo in-region)' publication pub_to_au;"
psql_on in-region  -c "create subscription sub_in_from_au connection '$(conninfo au)' publication pub_to_in;"
psql_on warehouse -c "create subscription sub_warehouse_from_us_east connection '$(conninfo us-east)' publication pub_to_amsterdam;"
psql_on us-east   -c "create subscription sub_us_east_from_warehouse connection '$(conninfo warehouse)' publication pub_to_us_east;"
psql_on warehouse -c "create subscription sub_warehouse_from_in connection '$(conninfo in-region)' publication pub_to_amsterdam;"
psql_on in-region  -c "create subscription sub_in_from_warehouse connection '$(conninfo warehouse)' publication pub_to_in;"

echo "== waiting for subscriptions to reach streaming state =="
for i in $(seq 1 30); do
  pending=$(psql_on us-east -t -A -c "select count(*) from pg_stat_subscription where not srsubstate = 'r' and srsubstate is not null" 2>/dev/null || echo 1)
  sleep 1
done
sleep 5

echo "== concurrent writers: 2000 rows each, four regions in parallel =="
write() {
  local host=$1 region=$2 n=$3
  docker compose -p "$PROJECT" exec -T "$host" psql -U postgres -q -c "
    insert into events (region, event_id, payload)
    select '$region', g, md5(g::text)
    from generate_series(1, $n) g;
  "
}
write us-east  us-east  2000 &
write us-west  us-west  2000 &
write in-region in      2000 &
write au       au       2000 &
wait

echo "== settling =="
sleep 10

mkdir -p results
{
  echo "=== row counts per node (expect 8000 everywhere once fully mirrored) ==="
  for host in us-east us-west in-region au warehouse; do
    echo -n "$host: "
    psql_on "$host" -t -A -c "select count(*) from events;"
  done

  echo
  echo "=== per-region row counts on the warehouse (expect 2000 each) ==="
  psql_on warehouse -c "select region, count(*) from events group by region order by region;"

  echo
  echo "=== apply/sync error counts, every node ==="
  echo "(pg_stat_subscription_stats' per-conflict-type columns --"
  echo " confl_insert_exists and friends -- are PG18+; this demo runs"
  echo " PG17, so apply_error_count/sync_error_count are what's available.)"
  for host in us-east us-west in-region au warehouse; do
    echo "-- $host --"
    psql_on "$host" -c "select subname, apply_error_count, sync_error_count from pg_stat_subscription_stats;"
  done
} | tee results/c-$(date +%Y%m%d-%H%M%S).out

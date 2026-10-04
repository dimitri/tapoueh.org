#!/usr/bin/env bash
# Usage: ./run.sh core|pglogical|pglogical-crash   (crash: optional, run after pglogical; see README)
# Runs sql/NN-*.sql in order. Line 1 of each file: `-- @service: name [name...]`
# (container(s) to run it on). Optional line 2: `-- @start: name` (compose service
# to start and wait for first). Output of psql -X -a (stdout+stderr) goes to
# results/<same base name>.out. For several services, run.sh writes a `== service ==`
# line before each service's output (the only line not printed by psql).
set -eu
cd "$(dirname "$0")"
export COMPOSE_IGNORE_ORPHANS=true
mode=${1:-core}
only=${2:-}   # optional: only run files whose name starts with this (dev aid)
case "$mode" in
  core)     file=docker-compose.yml;         glob='sql/[0-6][0-9]-*.sql'; extra="--profile worker4" ;;
  pglogical) file=docker-compose.pglogical.yml; glob='sql/7[0-4]-*.sql'; extra="" ;;
  pglogical-crash) file=docker-compose.pglogical.yml; glob='sql/79-*.sql'; extra="" ;;
  *) echo "usage: $0 core|pglogical|pglogical-crash" >&2; exit 2 ;;
esac
dc() { docker compose -f "$file" $extra "$@"; }
now() { perl -MTime::HiRes=time -e 'printf "%.2f", time'; }

mkdir -p results
[ -n "$only" ] || : > results/timings.txt
run_psql() { # svc, then psql args; sql on stdin
  local svc=$1; shift
  local id; id=$(echo "$svc" | sed 's/[^0-9]//g')
  dc exec -T -e PGOPTIONS='-c statement_timeout=180000' "$svc" sh -c 'psql -X -a -U postgres -d app "$@" 2>&1' sh \
     -v svc="$svc" -v worker_id="${id:-0}" "$@"
}

total0=$(now)
for f in $glob; do
  case "$(basename "$f")" in "$only"*) ;; *) continue ;; esac
  base=$(basename "$f" .sql)
  svcs=$(sed -n '1s/^-- @service: *//p' "$f")
  start=$(sed -n '2s/^-- @start: *//p' "$f")
  [ -n "$svcs" ] || { echo "$f: missing '-- @service:' first line" >&2; exit 1; }
  t0=$(now)
  if [ -n "$start" ]; then dc up -d --wait $start >/dev/null 2>&1; fi
  : > "results/$base.out"
  multi=0; [ "$(echo $svcs | wc -w)" -gt 1 ] && multi=1
  for s in $svcs; do
    # helpers, silently (always needed before the step file)
    dc exec -T "$s" psql -X -q -U postgres -d app -f - < lib/lr-helpers.sql >/dev/null 2>&1
    if [ $multi = 1 ]; then
      echo "== $s ==" >> "results/$base.out"
      run_psql "$s" -f - < "$f" >> "results/$base.out" || true
    else
      run_psql "$s" -f - < "$f" >> "results/$base.out" || true
    fi
  done
  t1=$(now)
  nerr=$(grep -c 'ERROR:' "results/$base.out" || true)
  printf '%-34s %6.2fs  ERROR lines: %s\n' "$base" "$(echo "$t1 - $t0" | bc)" "$nerr" | tee -a results/timings.txt
done
printf 'total %.2fs\n' "$(echo "$(now) - $total0" | bc)" | tee -a results/timings.txt

if [ "$mode" = pglogical-crash ]; then
  # The hub crash-loops until the subscription is gone. Best effort: try to drop it for a while
  # (may fail; `make clean` always works), and record that the segfault happened.
  base=$(basename sql/79-*.sql .sql)
  for i in $(seq 1 60); do
    dc exec -T pghub grep -q 'terminated by signal 11' /var/lib/postgresql/data/log/pg.log && break
    sleep 1
  done
  echo "== after the crash ==" >> "results/$base.out"
  dc exec -T pghub sh -c "grep 'terminated by signal' /var/lib/postgresql/data/log/pg.log | sed -E 's/\\(PID [0-9]+\\)/(PID N)/; s/16384:[0-9]+/16384:SUB/' | sort -u" >> "results/$base.out" || true
  for i in $(seq 1 30); do
    dc exec -T pghub sh -c "pg_isready -q -h 127.0.0.1 -U postgres -d app && psql -X -q -U postgres -d app -c \"SELECT pglogical.drop_subscription('sub_crash')\"" >/dev/null 2>&1 && break
    sleep 1
  done
fi

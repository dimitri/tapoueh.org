#!/usr/bin/env bash
# usage: ./run.sh FROM TO      run sql/NN-*.sql and sql/NN-*.sh with FROM <= NN <= TO
#
# Every step file starts with a comment naming where it runs:
#   -- @service: old        (sql: psql inside that compose service)
#   # @service: traffic     (sh: bash -s inside that compose service)
#   # @service: host        (sh: bash on the host)
# An optional second line "-- @db: name" selects the database (default: app
# for old/new, postgres otherwise).
# Raw output (psql -X -a, stdout+stderr, no prompts) goes to results/<base>.out
cd "$(dirname "$0")"
FROM=${1:-10}; TO=${2:-99}
export COMPOSE_PROFILES=a,b,c
DC="docker compose"
mkdir -p results

wait_ready() {                      # pg_isready on every running postgres service
  local s
  for s in old new pg19a pg19b pg19c; do
    $DC ps --status running --services 2>/dev/null | grep -qx "$s" || continue
    until $DC exec -T "$s" pg_isready -q -h 127.0.0.1 -U postgres; do sleep 0.5; done
  done
}

for f in sql/[0-9][0-9]-*; do
  base=$(basename "$f"); n=${base:0:2}; base=${base%.*}
  [ "$((10#$n))" -ge "$((10#$FROM))" ] && [ "$((10#$n))" -le "$((10#$TO))" ] || continue
  svc=$(sed -nE "1s/^(--|#) @service: *//p" "$f")
  db=$(sed -nE "2s/^(--|#) @db: *//p" "$f")
  [ -n "$svc" ] || { echo "$f: missing @service line" >&2; exit 1; }
  echo "==> $base  [$svc]"
  wait_ready
  case "$f" in
    *.sql)
      : "${db:=$( [[ $svc == old || $svc == new ]] && echo app || echo postgres )}"
      # stdout and stderr are merged INSIDE the container: docker demultiplexes them into
      # two streams and their relative order would otherwise be racy
      $DC exec -T "$svc" sh -c 'exec stdbuf -oL -eL psql -X -a -U postgres -d "$0" -f - 2>&1' "$db" < "$f" > "results/$base.out" ;;
    *.sh)
      if [ "$svc" = host ]; then bash "$f" > "results/$base.out" 2>&1
      else $DC exec -T "$svc" sh -c 'exec bash -s 2>&1' < "$f" > "results/$base.out"; fi ;;
  esac
  rc=$?
  [ $rc -eq 0 ] || { echo "    step exited with status $rc" | tee -a "results/$base.out"; }
done

#!/usr/bin/env bash
# Run every step in sql/ in lexical order.
#   sql/NN-name.sql : first line "-- @service: <container>", optional "-- @db: <database>"
#                     and "-- @nosync" (do not wait for subscriptions first).
#                     sh steps take "# @nosync" the same way.
#   sql/NN-name.sh  : shell step, sourced with lib.sh loaded (stdout+stderr recorded).
# Output of each step goes to results/NN-name.out (volatile values normalised).
set -u
ROOT="$(cd "$(dirname "$0")" && pwd)"; cd "$ROOT"
source ./lib.sh
mkdir -p results
# optional arguments: step-name prefixes to run (default: everything)
ONLY=("$@")
for f in $(ls sql/*.sql sql/*.sh | sort); do
  base=$(basename "$f"); base=${base%.*}
  if [ ${#ONLY[@]} -gt 0 ]; then
    hit=0; for o in "${ONLY[@]}"; do [[ "$base" == $o* ]] && hit=1; done
    [ $hit = 1 ] || continue
  fi
  out=results/$base.out
  echo "== $base"
  if [[ "$f" == *.sql ]]; then
    svc=$(sed -n '1s/^-- @service: *//p' "$f")
    db=$(sed -n 's/^-- @db: *//p' "$f" | head -1)
    [ -z "$svc" ] && { echo "missing @service in $f" >&2; exit 1; }
    [ -z "$db" ] && db=${svc%_standby}
    grep -q '^-- @nosync' "$f" || sync_all
    sq "$svc" "$db" -a < "$f" 2>&1 | normalize > "$out"
  else
    grep -q '^# @nosync' "$f" || sync_all
    ( set -u; source ./lib.sh; source "$f" ) 2>&1 | normalize > "$out"
  fi
done
echo "done"

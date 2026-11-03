#!/bin/sh
# Like a-writer.sh, for the two-region demo: inserts (region, id), logs acked ids.
# usage: f-writer.sh <uri> <region> <logfile>   (stops when /tmp/f-stop exists)
URI=${1:?uri}; REGION=${2:?region}; LOG=${3:?log}
n=1
: > "$LOG"
while [ ! -e /tmp/f-stop ]; do
  ok=0
  until [ -e /tmp/f-stop ]; do
    if psql "$URI" -X -q -v ON_ERROR_STOP=1 \
         -c "insert into events (region, id) values ('$REGION', $n)" 2>/dev/null; then
      ok=1; break
    fi
    if [ "$(psql "$URI" -X -At -c "select 1 from events where region = '$REGION' and id = $n" 2>/dev/null)" = 1 ]; then
      ok=1; break
    fi
    sleep 0.1
  done
  [ "$ok" = 1 ] || break
  echo "$n $(date +%s.%N)" >> "$LOG"
  n=$((n + 1))
  sleep 0.02
done

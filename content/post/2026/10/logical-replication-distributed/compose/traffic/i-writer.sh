#!/bin/sh
# Writer of the five-region demo: inserts (region, event_id) from a starting id, logs the
# acknowledged ids with a timestamp, pauses while /tmp/i-pause exists, stops at /tmp/i-stop.
# usage: i-writer.sh <uri> <region> <logfile> [first-id] [sleep-seconds]
URI=${1:?uri}; REGION=${2:?region}; LOG=${3:?log}; N=${4:-1}; PAUSE=${5:-0.02}
n=$N
: > "$LOG"
while [ ! -e /tmp/i-stop ]; do
  while [ -e /tmp/i-pause ] && [ ! -e /tmp/i-stop ]; do sleep 0.1; done
  ok=0
  until [ -e /tmp/i-stop ]; do
    if psql "$URI" -X -q -v ON_ERROR_STOP=1 \
         -c "insert into events (region, event_id) values ('$REGION', $n)" 2>/dev/null; then
      ok=1; break
    fi
    if [ "$(psql "$URI" -X -At -c "select 1 from events where region = '$REGION' and event_id = $n" 2>/dev/null)" = 1 ]; then
      ok=1; break
    fi
    sleep 0.1
  done
  [ "$ok" = 1 ] || break
  echo "$n $(date +%s.%N)" >> "$LOG"
  n=$((n + 1))
  sleep "$PAUSE"
done

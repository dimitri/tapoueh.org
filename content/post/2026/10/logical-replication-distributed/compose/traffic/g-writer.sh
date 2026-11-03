#!/bin/sh
# Like f-writer.sh, for the DDL demo: inserts (region, id), logs acked ids, and honours
# a pause file so the demo can quiesce writes without losing the id sequence.
# usage: g-writer.sh <uri> <region> <logfile> [pause-seconds]   (pauses while /tmp/g-pause exists, stops at /tmp/g-stop)
URI=${1:?uri}; REGION=${2:?region}; LOG=${3:?log}; PAUSE=${4:-0.02}
n=1
: > "$LOG"
while [ ! -e /tmp/g-stop ]; do
  while [ -e /tmp/g-pause ] && [ ! -e /tmp/g-stop ]; do sleep 0.1; done
  ok=0
  until [ -e /tmp/g-stop ]; do
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
  sleep "$PAUSE"
done

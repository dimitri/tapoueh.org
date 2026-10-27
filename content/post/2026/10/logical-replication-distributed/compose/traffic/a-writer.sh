#!/bin/sh
# One INSERT per transaction, ids chosen client-side so that a retried id is
# recognisable. Writes "<id> <ack time>" for every id the database confirmed.
# Stops when /tmp/a-stop exists.
URI="postgres://postgres@us-east-a:5432,us-east-b:5432,us-east-c:5432/app?target_session_attrs=read-write&connect_timeout=2"
LOG=${1:?log file}
n=1
: > "$LOG"
while [ ! -e /tmp/a-stop ]; do
  ok=0
  until [ -e /tmp/a-stop ]; do
    if psql "$URI" -X -q -v ON_ERROR_STOP=1 \
         -c "insert into events (id, node) values ($n, inet_server_addr())" 2>/dev/null; then
      ok=1; break
    fi
    # the failed attempt may still have committed
    if [ "$(psql "$URI" -X -At -c "select 1 from events where id = $n" 2>/dev/null)" = 1 ]; then
      ok=1; break
    fi
    sleep 0.1
  done
  [ "$ok" = 1 ] || break
  echo "$n $(date +%s.%N)" >> "$LOG"
  n=$((n + 1))
  sleep 0.02
done

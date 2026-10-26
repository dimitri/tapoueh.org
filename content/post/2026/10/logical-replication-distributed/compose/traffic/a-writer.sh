#!/bin/sh
# One INSERT per transaction, ids chosen client-side so that a retried id is
# recognisable. Writes "<id> <ack time>" for every id the database confirmed.
# Stops when /tmp/a-stop exists.
URI="postgres://postgres@pg-a-primary:5432,pg-a-standby:5432/app?target_session_attrs=read-write&connect_timeout=2"
LOG=${1:?log file}
n=1
: > "$LOG"
while [ ! -e /tmp/a-stop ]; do
  until [ -e /tmp/a-stop ]; do
    if psql "$URI" -X -q -v ON_ERROR_STOP=1 \
         -c "insert into events (id, node) values ($n, inet_server_addr())" 2>/dev/null; then
      break
    fi
    # the failed attempt may still have committed
    if [ "$(psql "$URI" -X -At -c "select 1 from events where id = $n" 2>/dev/null)" = 1 ]; then
      break
    fi
    sleep 0.1
  done
  echo "$n $(date +%s.%N)" >> "$LOG"
  n=$((n + 1))
  sleep 0.02
done

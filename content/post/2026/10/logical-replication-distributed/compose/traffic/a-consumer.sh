#!/bin/sh
# A CDC consumer: pg_recvlogical on the failover slot, reconnecting through the
# multi-host URI until it finds a read-write node. Every line is stamped with
# the time it was received. Stops when /tmp/a-stop-consumer exists.
URI="postgres://postgres@us-east-a:5432,us-east-b:5432,us-east-c:5432/app?target_session_attrs=read-write&connect_timeout=2"
LOG=${1:?log file}
: > "$LOG"
while [ ! -e /tmp/a-stop-consumer ]; do
  pg_recvlogical -d "$URI" --slot=cdc --start -f - -o skip-empty-xacts=1 \
      --status-interval=1 --fsync-interval=1 2>>"$LOG.err" \
    | perl -MTime::HiRes=time -ne 'BEGIN { $| = 1 } printf "%.3f %s", time, $_' >> "$LOG"
  sleep 0.2
done

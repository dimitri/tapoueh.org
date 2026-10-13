#!/bin/bash
# The "application": one transaction per loop turn, against whichever host is
# named in /tmp/target (that file is our stand-in for a DNS name, a pooler
# config or a load balancer: switching it is "switching the app").
# Log line: <end epoch ms> <seq> <host> ok|fail <latency ms> <detail>
export PGPASSWORD=app PGUSER=app PGCONNECT_TIMEOUT=5
LOG=/tmp/traffic.log
echo "${1:-old}" > /tmp/target
: > "$LOG"; rm -f /tmp/traffic.stop /tmp/traffic.done
seq=0
while [ ! -e /tmp/traffic.stop ]; do
  seq=$((seq + 1))
  host=$(cat /tmp/target)
  t0=$(date +%s%3N)
  out=$(psql -X -qAt -h "$host" -d app -v ON_ERROR_STOP=1 -v seq=$seq -f /work/traffic.sql 2>&1)
  rc=$?
  t1=$(date +%s%3N)
  if [ $rc -eq 0 ]; then
    echo "$t1 $seq $host ok $((t1 - t0)) ${out##*$'\n'}" >> "$LOG"
  else
    echo "$t1 $seq $host fail $((t1 - t0)) $(echo "$out" | head -1 | tr ' ' '_')" >> "$LOG"
    sleep 0.02
  fi
done
touch /tmp/traffic.done

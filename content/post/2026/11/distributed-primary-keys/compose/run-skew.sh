#!/bin/bash
# ./run-skew.sh  (ONLY=<case> to run one) -- one server, six clock situations, uuidv7(interval) as the skewed clock
cd "$(dirname "$0")"; . ./lib.sh
pg_start pk-skew
while read -r name s0 s1 s2; do
  [ -n "$ONLY" ] && [ "$ONLY" != "$name" ] && continue
  echo "== $name: region shifts $s0 / $s1 / $s2"
  psqlx pk-skew -At -F ' | ' -v s0="$s0" -v s1="$s1" -v s2="$s2" < skew.sql
done <<'CASES'
no_skew 0s 0s 0s
skew_5ms 0s 5ms -5ms
skew_50ms 0s 50ms -50ms
skew_200ms 0s 200ms -200ms
skew_2s 0s 2s -2s
skew_1min 0s 1min -1min
one_region_1h_ahead 0s 1h 0s
one_region_1day_ahead 0s 1day 0s
CASES
docker rm -f pk-skew >/dev/null

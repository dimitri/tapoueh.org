#!/bin/bash
# nextval() alone, N clients on ONE sequence: a plain sequence against the snowflake and the regionseq (extension/) access methods.
# 1,000 nextval() per transaction, 5 s per measurement. Needs IMAGE=pk-seqam.
cd "$(dirname "$0")"; . ./lib.sh; IMAGE=pk-seqam
N=pk-scale; pg_start $N $(rsargs 1)
psqlx $N -c "create extension snowflake" -c "create sequence plain" -c "create sequence sf using snowflake" -c "create extension regionseq" -c "create sequence rs using regionseq" -c "create sequence plain_c cache 32"
for seq in plain plain_c sf rs; do
  echo "select count(nextval('$seq')) from generate_series(1, 1000);" > /tmp/pk-scale-$seq.sql
  docker cp /tmp/pk-scale-$seq.sql $N:/tmp/$seq.sql
done
echo "clients | sequence | nextval/s"
for c in 1 4 16; do
  for seq in plain plain_c sf rs; do
    tps=$(docker exec $N pgbench -n -U postgres -d postgres -c $c -j $c -T 5 -f /tmp/$seq.sql 2>&1 | sed -n 's/^tps = \([0-9.]*\).*/\1/p')
    echo "$c | $seq | $(python3 -c "print(f'{float(\"$tps\")*1000:,.0f}')")"
  done
done
docker rm -f $N >/dev/null

#!/bin/bash
# Same one-server workload as run-single-all.sh, on the build with the sequence access method patches
# (IMAGE=pk-seqam, see seqam/Dockerfile): the five keys again on that build, then
#   snowone: the snowflake bit layout from a default expression, on ONE shared sequence
#   snowam:  the same layout from the snowflake sequence access method (one shared sequence)
cd "$(dirname "$0")"; export IMAGE=pk-seqam
for run in 1 2 3; do
  for v in seqstep composite uuidv4 uuidv7 snowflake snowone snowam; do
    ./run-single.sh $v $run > ../results/31-seqam-$v-run$run.out 2>&1
  done
done
echo done > ../results/31-seqam.done

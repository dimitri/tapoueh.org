#!/bin/bash
# The whole matrix: 9 variants x 3 runs. Results in ../results/30-single-<variant>-run<n>.out
cd "$(dirname "$0")"
for run in 1 2 3; do
  for v in seqstep seqstep+ composite uuidv4 uuidv4+ uuidv7 uuidv7+ snowflake snowflake+; do
    ./run-single.sh $v $run > ../results/30-single-$v-run$run.out 2>&1
  done
done
echo done > ../results/30-single.done

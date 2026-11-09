#!/bin/bash
cd "$(dirname "$0")"
R=../results
./run-child.sh > $R/95-child.out 2>&1
./run-skew.sh > $R/90-skew.out 2>&1
for v in naive seqstep composite uuidv4 uuidv7 snowflake; do ./run-mesh.sh $v 1 > $R/80-mesh-$v.out 2>&1; done
echo done > $R/rest2.done

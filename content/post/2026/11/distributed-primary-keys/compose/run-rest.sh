#!/bin/bash
cd "$(dirname "$0")"
R=../results
./run-filter.sh > $R/50-filter-uuid.out 2>&1
for o in publisher-first subscriber-first; do ./run-widen.sh $o > $R/60-widen-$o.out 2>&1; done
for sc in insync lagging; do for r in 1 2 3; do ./run-failover.sh $sc $r > $R/70-failover-$sc-run$r.out 2>&1; done; done
for v in naive seqstep composite uuidv4 uuidv7 snowflake; do ./run-mesh.sh $v 1 > $R/80-mesh-$v.out 2>&1; done
./run-skew.sh > $R/90-skew.out 2>&1
./run-child.sh > $R/95-child.out 2>&1
echo done > $R/rest.done

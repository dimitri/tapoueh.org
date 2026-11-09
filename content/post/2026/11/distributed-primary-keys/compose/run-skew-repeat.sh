#!/bin/bash
cd "$(dirname "$0")"
for r in 2 3; do for c in skew_5ms skew_50ms skew_200ms; do ONLY=$c ./run-skew.sh >> ../results/90-skew-repeat.out 2>&1; done; done
echo done > ../results/skew-repeat.done

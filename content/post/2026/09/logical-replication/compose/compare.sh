#!/usr/bin/env bash
# Usage: ./compare.sh dirA dirB   -- diff two results directories, ignoring measured timings.
# Lines carrying "unstable" (timings in step 3) are removed before diffing; everything else,
# including LSNs/xids/timestamps normalised inside the SQL (lr.norm), must be identical.
set -eu
a=$1; b=$2; rc=0
for f in "$a"/*.out; do
  n=$(basename "$f")
  if ! diff <(grep -v unstable "$f") <(grep -v unstable "$b/$n") >/dev/null; then
    echo "DIFFERS: $n"; diff <(grep -v unstable "$f") <(grep -v unstable "$b/$n") | head -20; rc=1
  fi
done
[ $rc = 0 ] && echo "identical (ignoring lines marked 'unstable'): $(ls "$a"/*.out | wc -l | tr -d ' ') files"
exit $rc

#!/usr/bin/env bash
# Run the whole demo twice from a clean state and diff the normalised results.
# (see normalize.sh for what is masked). Takes about 20 minutes.
# Raw copies of both runs are kept in .verify/run1 and .verify/run2.
cd "$(dirname "$0")"
run() {
  make clean >/dev/null 2>&1
  make all > ".verify/run$1.log" 2>&1
  rm -rf ".verify/run$1"; cp -R results ".verify/run$1"
}
rm -rf .verify; mkdir -p .verify
run 1; run 2
status=0
for f in .verify/run1/*.out; do
  b=$(basename "$f")
  if ! diff <(./normalize.sh < "$f") <(./normalize.sh < ".verify/run2/$b") > ".verify/diff-$b.txt"; then
    echo "DIFFERS: $b (see .verify/diff-$b.txt)"; status=1
  else rm -f ".verify/diff-$b.txt"; fi
done
[ $status = 0 ] && echo "all results identical after normalisation"
exit $status

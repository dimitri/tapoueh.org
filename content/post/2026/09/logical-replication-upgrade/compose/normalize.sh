#!/usr/bin/env bash
# Mask what legitimately differs between two runs, so that diff shows only real
# changes of behaviour. Reads stdin, writes stdout.
#   - [poll] lines (timing dependent samples) are dropped
#   - LSNs (X/XXXXXXXX), timestamps, pids in [..], md5 hashes
#   - every integer of 3 or more digits (row counts, ms, oids, xids, sequence
#     values: all depend on how much traffic the loop managed to write)
#   - table alignment (psql pads columns to the widest value)
#   - the few small counters that depend on timing (failed attempts, apply error
#     counts and how many times a failing worker was retried, log line counts)
grep -vE '^\[poll\]|is not a PostgreSQL backend process' |
sed -E '
  s/[0-9A-F]{1,8}\/[0-9A-F]{8}/LSN/g
  s/[0-9]{4}-[0-9]{2}-[0-9]{2}[ T][0-9:.+]+/TS/g
  s/[0-9]{8}T[0-9]{6}\.[0-9]+/TS/g
  s/^\[[0-9]+\]/[PID]/
  s/(^|[^0-9a-f])[0-9a-f]{32}([^0-9a-f]|$)/\1MD5\2/g
  /^\[time\]/ s/(: |\+)[0-9]+/\1N/g
  s/^(for scale: median gap between two commits )[0-9]+ ms, p99 [0-9]+ ms/\1N ms, p99 N ms/
  s/^([0-9]+ (old|new) (ok|fail)) [0-9]+ /\1 L /
  s/^(N (old|new) (ok|fail)) [0-9]+ /\1 L /
  s/[0-9]+ app sessions terminated/N app sessions terminated/
  s/^( *)[0-9]+ (ERROR: cannot execute INSERT)/\1N \2/
  s/^(new|old) (fail|ok) +[0-9]+$/\1 \2 N/
  s/^ *[0-9]+ (ERROR|LOG|CONTEXT|FATAL|WARNING|DETAIL)/ N \1/
  s/[0-9]{3,}/N/g
  s/(failed attempts inside the window: )[0-9]+ \(of [0-9]+/\1N (of N/
  s/(apply errors|apply_error_count).*//
  s/\| +[0-9]+ +\|( +[0-9]+ +)?$/| N |/
  s/[[:space:]]+/ /g
  s/-{2,}/--/g; s/\+-+/+-/g
  s/[[:space:]]+$//
  s/^( ?(app_sub|app_rev) \| )[0-9]+$/\1N/
'

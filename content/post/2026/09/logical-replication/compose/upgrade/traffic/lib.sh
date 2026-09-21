# shared helpers, sourced by the steps that run inside the "traffic" container
export PGPASSWORD=postgres PGUSER=postgres PGCONNECT_TIMEOUT=5
# admin sessions must not inherit the "frozen" read-only default of the database
export PGOPTIONS='-c default_transaction_read_only=off'
now_ms() { date +%s%3N; }
# q HOST DB "SQL": print the statement, then run it
q()  { echo "$1> $3"; psql -X -h "$1" -d "$2" -c "$3" 2>&1; }
# qa HOST DB "SQL": run quietly, unaligned, tuples only (for scripting)
qa() { psql -X -qAt -h "$1" -d "$2" -c "$3"; }

# stall_report FROM_HOST TO_HOST: app-visible write stall around a switch,
# computed from /tmp/traffic.log (lines: end_ms seq host ok|fail latency detail)
stall_report() {
  awk -v from="$1" -v to="$2" '
    $4 == "ok" && ph == from && $3 == to { last = pts; first = $1; found = 1 }
    $4 == "ok" { ph = $3; pts = $1 }
    { t[NR] = $1; s[NR] = $4 }
    END {
      if (!found) { print "no switch " from " -> " to " found in the log"; exit }
      f = 0; a = 0
      for (i = 1; i <= NR; i++) if (t[i] > last && t[i] <= first) { a++; if (s[i] == "fail") f++ }
      printf "last committed write on %s : %d ms\n", from, last
      printf "first committed write on %s: %d ms\n", to, first
      printf "WRITE STALL %s -> %s: %d ms\n", from, to, first - last
      printf "failed attempts inside the window: %d (of %d logged attempts, incl. the first success)\n", f, a
    }' /tmp/traffic.log
  awk '$4=="ok"{ if (pt) print $1-pt; pt=$1 }' /tmp/traffic.log | sort -n |
    awk '{ v[NR]=$1 } END { printf "for scale: median gap between two commits %d ms, p99 %d ms, over %d commits\n", v[int(NR/2)+1], v[int(NR*0.99)+1], NR+1 }'
}

# ---- cutover building blocks -------------------------------------------------
ms() { echo $(( $(now_ms) - T0 )); }          # ms since T0 (set by the caller)
freeze()   { qa "$1" app "ALTER DATABASE app SET default_transaction_read_only = on" >/dev/null
             qa "$1" app "SELECT count(pg_terminate_backend(pid)) FROM pg_stat_activity WHERE datname = 'app' AND usename = 'app'"; }
unfreeze() { qa "$1" app "ALTER DATABASE app RESET default_transaction_read_only" >/dev/null; }

# catch_up SRC DST SUB MARKER: SRC is frozen. Prove that DST (subscription SUB)
# has applied everything. Tries, and reports on, three ways of knowing:
#   marker    a row written on SRC after the freeze becomes visible on DST
#   remote_lsn  pg_replication_origin_status.remote_lsn >= pg_current_wal_lsn() on SRC
#   latest_end  pg_stat_subscription.latest_end_lsn     >= pg_current_wal_lsn() on SRC
# t1 = pg_current_wal_lsn() right after the freeze, t2 = after the marker commit
catch_up() {
  local src=$1 dst=$2 sub=$3 mk=$4 t1 t2 tries=0 row m a b c fm="" fa="" fb="" fc=""
  t1=$(qa "$src" app "SELECT pg_current_wal_lsn()")
  qa "$src" app "INSERT INTO cutover_markers (id) VALUES ('$mk')" > /dev/null
  t2=$(qa "$src" app "SELECT pg_current_wal_lsn()")
  while [ $tries -lt 500 ]; do
    tries=$((tries + 1))
    row=$(qa "$dst" app "SELECT
      EXISTS (SELECT 1 FROM cutover_markers WHERE id = '$mk'),
      coalesce((SELECT remote_lsn >= '$t1' FROM pg_replication_origin_status WHERE external_id = 'pg_' || (SELECT oid FROM pg_subscription WHERE subname = '$sub')), false),
      coalesce((SELECT remote_lsn >= '$t2' FROM pg_replication_origin_status WHERE external_id = 'pg_' || (SELECT oid FROM pg_subscription WHERE subname = '$sub')), false),
      coalesce((SELECT latest_end_lsn >= '$t1' FROM pg_stat_subscription WHERE subname = '$sub' AND relid IS NULL), false)")
    IFS='|' read -r m a b c <<< "$row"
    [ -z "$fm" ] && [ "$m" = t ] && fm=$(ms)
    [ -z "$fa" ] && [ "$a" = t ] && fa=$(ms)
    [ -z "$fb" ] && [ "$b" = t ] && fb=$(ms)
    [ -z "$fc" ] && [ "$c" = t ] && fc=$(ms)
    [ -n "$fm" ] && break
    sleep 0.01
  done
  echo "[time] +$(ms) ms $dst caught up (marker row visible: ${fm:-NEVER})"
  echo "[time] at that moment: remote_lsn >= t1(after freeze): ${fa:-no} | remote_lsn >= t2(after marker): ${fb:-no} | latest_end_lsn >= t1: ${fc:-no}"
  [ -n "$fm" ]
}

# copy_sequences SRC DST: setval() every sequence on DST from SRC's values
copy_sequences() {
  psql -X -qAt -h "$1" -d app -c "SELECT format('SELECT setval(%L, %s, true);', quote_ident(schemaname) || '.' || quote_ident(sequencename), last_value) FROM pg_sequences WHERE last_value IS NOT NULL ORDER BY 1" > /tmp/setval.sql
  psql -X -qAt -h "$2" -d app -f /tmp/setval.sql > /dev/null
}

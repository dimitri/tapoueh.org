# shellcheck shell=bash
# Helpers shared by run.sh and the sql/NN-*.sh steps.
# Project name, compose file and container user are fixed for reproducibility.
DC=(docker compose -p lrcons --profile standby -f "${ROOT:-$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)}/docker-compose.yml")

# sq SERVICE DB [psql args...] : run psql on a service (stdin is passed through)
# stderr is merged inside the container (one ordered stream: notices and errors stay in place)
sq() { local svc=$1 db=$2; shift 2; "${DC[@]}" exec -T "$svc" sh -c 'exec stdbuf -oL -eL psql -X -U postgres -d "$0" -v ON_ERROR_STOP=0 "$@" 2>&1' "$db" "$@"; }

# qt SERVICE DB SQL : tuples-only, unaligned scalar query
qt() { sq "$1" "$2" -qAt -c "$3" </dev/null; }

# show SERVICE DB SQL : echo the statement then run it, aligned output
show() { echo "-- $1/$2: $3"; sq "$1" "$2" -c "$3" </dev/null; }

# wait_until SERVICE DB SQL [TIMEOUT_S] : poll until the query returns 't'
wait_until() {
  local svc=$1 db=$2 sql=$3 t=${4:-60} i=0
  while [ "$(qt "$svc" "$db" "$sql" 2>/dev/null)" != "t" ]; do
    i=$((i+1)); [ "$i" -ge $((t*5)) ] && { echo "TIMEOUT waiting: $sql" >&2; return 1; }
    sleep 0.2
  done
}

# wait_ready SERVICE [DB]: pg_isready over TCP (not the init-time socket-only server)
wait_ready() {
  local svc=$1 db=${2:-postgres} i=0
  until "${DC[@]}" exec -T "$svc" pg_isready -h 127.0.0.1 -U postgres -d "$db" -q </dev/null; do
    i=$((i+1)); [ "$i" -ge 150 ] && { echo "TIMEOUT pg_isready $svc" >&2; return 1; }
    sleep 0.2
  done
}

# sync_all : wait until every enabled subscription of every warehouse database
# has finished its initial copy and has caught up with the publisher's current
# WAL position (polling, no fixed sleeps).
sync_all() {
  local db sub host lsn
  for db in $(qt warehouse postgres "select datname from pg_database where datallowconn and not datistemplate order by 1"); do
    while IFS='|' read -r sub host; do
      [ -z "$sub" ] && continue
      lsn=$(qt "$host" postgres "select pg_current_wal_lsn()") || continue
      wait_until warehouse "$db" "select (select count(*)=0 from pg_subscription_rel r where r.srsubid=s.oid and r.srsubstate<>'r') and coalesce((select bool_and(latest_end_lsn >= '$lsn') from pg_stat_subscription ss where ss.subid=s.oid and ss.relid is null and ss.pid is not null),false) from pg_subscription s where s.subname='$sub'" 60 \
        || echo "sync_all: $sub did not catch up" >&2
    done < <(qt warehouse "$db" "select subname||'|'||regexp_replace(subconninfo,'.*host=([^ ]+).*','\1') from pg_subscription where subenabled and subname not like 'broken\_%'")
  done
}

# normalize : strip volatile values (LSNs, timestamps, pids, xids) from raw output
normalize() {
  sed -E \
    -e 's/(^|[^0-9A-Za-z_])[0-9A-F]{1,8}\/[0-9A-F]{5,8}([^0-9A-Za-z_]|$)/\1<LSN>\2/g' \
    -e 's/(^|[^0-9A-Za-z_])[0-9A-F]{1,8}\/[0-9A-F]{5,8}([^0-9A-Za-z_]|$)/\1<LSN>\2/g' \
    -e 's/20[0-9]{2}-[0-9]{2}-[0-9]{2}[ T][0-9:.]+( UTC|[+-][0-9]{2})?/<TS>/g' \
    -e 's/\bPID [0-9]+/PID <pid>/g' \
    -e 's/\[[0-9]+\] /[<pid>] /g' \
    -e 's/process [0-9]+/process <pid>/g' \
    -e 's/in transaction [0-9]+/in transaction <xid>/g' \
    -e 's/xid horizon [0-9]+/xid horizon <xid>/g' \
    -e 's/worker "pg_[0-9]+"/origin "pg_<oid>"/g' \
    -e 's/pg_[0-9]{5,}/pg_<oid>/g' \
    -e 's/_sync_[0-9]+_[0-9]+/_sync_<oid>_<sysid>/g'
}

# wlog SERVICE PATTERN : matching server-log lines, timestamp and pid stripped
wlog() {
  "${DC[@]}" logs --no-log-prefix "$1" 2>&1 </dev/null | grep -E "$2" | sed -E 's/^[0-9-]+ [0-9:.]+ UTC \[[0-9]+\] //' | sort -u
}

# wait_file SERVICE FILE PATTERN [TIMEOUT_S] : poll until FILE (inside the container) matches PATTERN
wait_file() {
  local svc=$1 f=$2 pat=$3 t=${4:-60} i=0
  until "${DC[@]}" exec -T "$svc" grep -q -- "$pat" "$f" </dev/null 2>/dev/null; do
    i=$((i+1)); [ "$i" -ge $((t*5)) ] && { echo "TIMEOUT waiting for '$pat' in $f" >&2; return 1; }
    sleep 0.2
  done
}

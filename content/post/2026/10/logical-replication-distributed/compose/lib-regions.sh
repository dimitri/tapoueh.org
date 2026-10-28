# Shared helpers for the profiles that run pg_auto_failover groups.
# Every region is one group of its own: <region>-monitor and <region>-a, -b, -c.
# Source it from a script that has done `cd "$(dirname "$0")"`.

dc() { docker compose -p lrdist "$@"; }

# rmon <region> <sql>: query a region's monitor
rmon() { dc exec -T "$1-monitor" psql -X -At -h 127.0.0.1 -U postgres -d pg_auto_failover -c "$2"; }
# rpq <node> <sql>: query a node's application database
rpq() { dc exec -T "$1" psql -X -At -h 127.0.0.1 -U postgres -d app -c "$2"; }
# ruri / rconninfo <region>: connect to whichever node of the group is the primary
ruri() { echo "postgres://postgres@$1-a:5432,$1-b:5432,$1-c:5432/app?target_session_attrs=read-write&connect_timeout=2"; }
rconninfo() { echo "host=$1-a,$1-b,$1-c port=5432 dbname=app user=postgres target_session_attrs=read-write connect_timeout=2"; }

region_reset() { local r; for r in "$@"; do dc rm -sf "$r-monitor" "$r-a" "$r-b" "$r-c" >/dev/null 2>&1; done; }

# region_up <region>...: monitors and first nodes (the primaries) first, then the standbys.
# The caller sets COMPOSE_PROFILES.
region_up() {
  local r svc=()
  for r in "$@"; do svc+=("$r-monitor" "$r-a"); done
  dc up -d --wait "${svc[@]}" >/dev/null 2>&1 || return 1
  svc=(); for r in "$@"; do svc+=("$r-b" "$r-c"); done
  dc up -d "${svc[@]}" >/dev/null 2>&1
}

# region_wait <region>: one primary and two secondaries, every node at its goal state, and the
# primary carries both standbys' slots in synchronized_standby_slots.
# When both standbys report secondary before the primary has reported primary, the primary can
# keep the list it fetched with only one of them (seen on the pinned build, not corrected by
# itself). Re-setting a node's replication quorum makes the keeper read the monitor's value
# again, so after ten seconds of a short list this does exactly that, and writes the incident
# to results/nudges.log.
region_wait() {
  local i n p g short=0
  for i in $(seq 1 240); do
    n=$(rmon "$1" "select count(*) from pgautofailover.node where reportedstate = goalstate and reportedstate in ('primary','secondary')" 2>/dev/null)
    if [ "$n" = 3 ]; then
      p=$(region_primary "$1")
      g=$(rpq "$p" "show synchronized_standby_slots" 2>/dev/null)
      case "$g" in *,*) return 0 ;; esac
      short=$((short + 1))
      if [ "$short" = 10 ]; then
        echo "$(date -u +%FT%TZ) ${SCN:-?} $1: $p lists '$g', monitor says '$(dc exec -T "$1-monitor" pg_autoctl show synchronized-standby-slots --pgdata /data/pgdata 2>/dev/null | tr -d "'\r")'; nudged" >> results/nudges.log
        dc exec -T "$1-monitor" pg_autoctl set node replication-quorum true --pgdata /data/pgdata --name "$p" >/dev/null 2>&1
      fi
    fi
    sleep 1
  done
  return 1
}

# region_label <region>: the region label of all three nodes is the region's own name
region_label() {
  local n
  for n in a b c; do
    dc exec -T "$1-monitor" pg_autoctl set node region --pgdata /data/pgdata --name "$1-$n" "$1" >/dev/null 2>&1
  done
}

# region_primary <region>: the node that is the group's primary right now
region_primary() {
  rmon "$1" "select nodename from pgautofailover.node where reportedstate in ('primary','wait_primary','single') and goalstate in ('primary','wait_primary','single') order by nodeid limit 1"
}

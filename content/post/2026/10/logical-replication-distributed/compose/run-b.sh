#!/usr/bin/env bash
# Profile b: what synchronous_commit costs when the standby is a region away. A primary and a
# physical standby, a round-trip time added with tc netem on both containers, and pgbench
# running single-row insert transactions under each synchronous_commit setting.
#
#   ./run-b.sh            RTTS="0 20 70 150" DURATION=10
set -uo pipefail
cd "$(dirname "$0")"
export COMPOSE_PROFILES=b
dc() { docker compose -p lrdist "$@"; }
qa() { dc exec -T "$1" psql -X -At -U postgres -d app "${@:2}" 2>&1; }
PGDATA=/var/lib/postgresql/18/docker
OUT=results/b-sync-commit.out
RTTS=${RTTS:-"0 20 70 150"}
DURATION=${DURATION:-10}
mkdir -p results
dc rm -sf b-primary b-standby >/dev/null 2>&1
dc up -d --wait b-primary >/dev/null 2>&1
dc up -d b-standby >/dev/null 2>&1
{
echo "== primary b-primary, standby b-standby, synchronous_standby_names = 'ANY 1 (standby)' =="
dc exec -T b-primary sh -c "echo 'host replication all all trust' >> $PGDATA/pg_hba.conf"
qa b-primary -c "select pg_reload_conf()" >/dev/null
dc exec -T -u postgres b-standby sh -c "pg_basebackup -h b-primary -U postgres -D $PGDATA -X stream --write-recovery-conf >/dev/null 2>&1 \
  && printf \"primary_conninfo = 'host=b-primary user=postgres application_name=standby'\n\" >> $PGDATA/postgresql.auto.conf \
  && pg_ctl -D $PGDATA -l /tmp/standby.log -w start >/dev/null"
qa b-primary -c "alter system set synchronous_standby_names = 'ANY 1 (standby)'" -c "select pg_reload_conf()" >/dev/null
qa b-primary -c "create table t (id bigserial primary key, v int not null)" >/dev/null
dc exec -T b-primary sh -c "echo 'insert into t (v) values (1);' > /tmp/ins.sql"
echo "standby: $(qa b-primary -c "select state || ', ' || sync_state from pg_stat_replication")"
echo
printf '%-8s %-14s %12s %14s %14s\n' "RTT ms" "synchronous_commit" "1 client tps" "avg latency ms" "32 clients tps"
for rtt in $RTTS; do
  half=$((rtt / 2))
  for c in b-primary b-standby; do
    dc exec -T $c tc qdisc replace dev eth0 root netem delay ${half}ms >/dev/null 2>&1
  done
  for mode in off local remote_write on remote_apply; do
    one=$(dc exec -T -e PGOPTIONS="-c synchronous_commit=$mode" b-primary pgbench -n -c 1 -T $DURATION -f /tmp/ins.sql -U postgres app 2>&1)
    many=$(dc exec -T -e PGOPTIONS="-c synchronous_commit=$mode" b-primary pgbench -n -c 32 -j 4 -T $DURATION -f /tmp/ins.sql -U postgres app 2>&1)
    printf '%-8s %-14s %12s %14s %14s\n' "$rtt" "$mode" \
      "$(echo "$one" | awk '/^tps/ { printf "%d", $3 }')" \
      "$(echo "$one" | awk '/latency average/ { print $4 }')" \
      "$(echo "$many" | awk '/^tps/ { printf "%d", $3 }')"
  done
done
for c in b-primary b-standby; do dc exec -T $c tc qdisc del dev eth0 root >/dev/null 2>&1; done
} 2>&1 | tee "$OUT"

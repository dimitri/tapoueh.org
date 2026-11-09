#!/bin/bash
# A region as pg_auto_failover manages it, with a logical subscriber, and a failover under writes.
# ./run-pgaf-drill.sh <expression|uuidv7|regionseq> <skew in seconds> [RUN]
#   monitor + two nodes (synchronous standby, node 2 clock <skew> s behind), a writer on the formation's
#   multi-host URI, a subscriber with a failover slot (failover = true, synchronized_standby_slots and slot sync are
#   managed by pg_auto_failover).  The primary is killed after 6 s; pg_auto_failover promotes node 2.
#   Needs pk-pgaf (seqam/Dockerfile.pgaf).
cd "$(dirname "$0")"; . ./lib.sh; IMAGE=pk-pgaf
K=${1:?key}; SK=${2:?skew in seconds}; RUN=${3:-1}
# pg_autoctl confirms a promotion by the timeline of the last checkpoint, which the checkpoint after a promotion spreads
# over checkpoint_completion_target x checkpoint_timeout; CKPT=default keeps Postgres's 0.9 x 5 min, otherwise 0.
CKPTLINE='checkpoint_completion_target = 0'; WAIT=60
[ "$CKPT" = default ] && { CKPTLINE='# default checkpoint settings'; WAIT=420; }
case $K in
  expression) CREATE="create sequence s"; E="((((extract(epoch from clock_timestamp()) * 1000)::bigint - 1767225600000) << 22) | (1::bigint << 18) | (nextval('s') & 262143))"; COLTYPE=bigint; KEYTXT="lpad(id::text, 20, '0')" ;;
  uuidv7)     CREATE="select 1"; E="uuidv7()"; COLTYPE=uuid; KEYTXT="id::text" ;;
  regionseq)  CREATE="create extension regionseq; create sequence s using regionseq"; E="nextval('s')"; COLTYPE=bigint; KEYTXT="lpad(id::text, 20, '0')" ;;
  *) echo "unknown key $K"; exit 1 ;;
esac
NET=pknet; docker network create $NET >/dev/null 2>&1
MON=postgres://autoctl_node@pk-mon:5432/pg_auto_failover
LD=$(docker run --rm --entrypoint sh $IMAGE -c 'ls /usr/lib/*/faketime/libfaketime.so.1')
for c in pk-mon pk-n1 pk-n2 pk-sub; do docker rm -f $c >/dev/null 2>&1; done

node() {   # name skew [preinit]: node 1 starts from an initdb with our settings, node 2 is cloned from it by pg_autoctl
  docker run -d --name $1 --network $NET --tmpfs /var/lib/postgresql:rw,size=2g --entrypoint bash $IMAGE -c '
    D=/var/lib/postgresql/node; mkdir -p $D; chown -R postgres:postgres /var/lib/postgresql; chmod 700 $D
    if [ "'$3'" = preinit ]; then
      gosu postgres initdb -D $D --auth=trust -U postgres >/dev/null
      cat >> $D/postgresql.conf <<CONF
shared_preload_libraries = regionseq
regionseq.region_id = 1
max_wal_size = 2GB
'"$CKPTLINE"'
CONF
      echo "host all all all trust" >> $D/pg_hba.conf
    fi
    export LD_PRELOAD='$LD' FAKETIME="-'$2's"
    exec gosu postgres pg_autoctl create postgres --pgdata $D --pgport 5432 --hostname '$1' --monitor '$MON' --auth trust --no-ssl --run' >/dev/null
}
state() { docker exec -u postgres $1 pg_autoctl show state --pgdata /var/lib/postgresql/$2 2>/dev/null; }

docker run -d --name pk-mon --network $NET --tmpfs /var/lib/postgresql:rw,size=500m --entrypoint bash $IMAGE -c '
  mkdir -p /var/lib/postgresql/mon && chown -R postgres:postgres /var/lib/postgresql
  exec gosu postgres pg_autoctl create monitor --pgdata /var/lib/postgresql/mon --pgport 5432 --hostname pk-mon --auth trust --no-ssl --run' >/dev/null
until docker exec pk-mon pg_isready -q -h localhost -U postgres 2>/dev/null; do sleep 1; done
sleep 3
# failover in seconds, not in half a minute
docker exec pk-mon psql -X -q -U postgres -d pg_auto_failover -c "alter system set pgautofailover.health_check_period = 1000" \
  -c "alter system set pgautofailover.health_check_timeout = 1000" -c "alter system set pgautofailover.health_check_max_retries = 1" \
  -c "alter system set pgautofailover.health_check_retry_delay = 500" -c "alter system set pgautofailover.node_considered_unhealthy_timeout = 3000" -c "select pg_reload_conf()" >/dev/null
node pk-n1 0 preinit
until state pk-n1 node | grep -q "single"; do sleep 1; done
node pk-n2 $SK
echo "waiting for node 2 to become the synchronous standby..."
for i in $(seq 1 90); do
  S=$(docker exec pk-mon psql -X -At -U postgres -d pg_auto_failover -c "select string_agg(nodename || '=' || reportedstate, ' ' order by nodename) from pgautofailover.node")
  case "$S" in *"node_1=primary"*"node_2=secondary"*) break ;; esac; sleep 2
done
echo "state: $S"
PRIMARY=pk-n1
psqlx pk-n1 -c "$CREATE" -c "create table t (id $COLTYPE primary key, payload text not null)" -c "create publication p for table t"
IMAGE=$IMAGE pg_start pk-sub -c max_wal_size=2GB
psqlx pk-sub -c "create table t (id $COLTYPE primary key, payload text not null)" \
  -c "create subscription sub connection 'postgres://postgres@pk-n1:5432,pk-n2:5432/postgres?target_session_attrs=read-write&connect_timeout=3' publication p with (failover = true, copy_data = false)" >/dev/null
sleep 3
echo "settings on the primary: $(psqlx pk-n1 -At -c "select name || '=' || setting from pg_settings where name in ('synchronized_standby_slots','synchronous_standby_names','sync_replication_slots') order by 1" | tr '\n' ' ')"
echo "failover slot on the primary: $(psqlx pk-n1 -At -c "select slot_name || ' failover=' || failover from pg_replication_slots where slot_type = 'logical'")"
sleep 3
echo "synced on node 2: $(psqlx pk-n2 -At -c "select slot_name || ' synced=' || synced from pg_replication_slots where slot_type = 'logical'")"

cat > /tmp/pk-writer.sh <<WR
#!/bin/bash
URI="postgres://postgres@pk-n1:5432,pk-n2:5432/postgres?target_session_attrs=read-write&connect_timeout=2"
while [ ! -f /tmp/stop ]; do
  echo "CALL \$(date +%s%N)"
  psql "\$URI" -X -At -c "insert into t (id, payload) select $E, 'x' from generate_series(1, 1000) returning id" 2>&1
done
WR
docker cp /tmp/pk-writer.sh pk-sub:/tmp/writer.sh
docker exec pk-sub rm -f /tmp/stop
docker exec -d pk-sub bash -c "bash /tmp/writer.sh > /tmp/writer.log 2>&1"
sleep 6
echo "== killing the primary"; date +%s > /tmp/pk-kill.t
docker kill pk-n1 >/dev/null
for i in $(seq 1 $WAIT); do
  S=$(docker exec pk-mon psql -X -At -U postgres -d pg_auto_failover -c "select string_agg(nodename || '=' || reportedstate, ' ' order by nodename) from pgautofailover.node")
  case "$S" in *"node_2=primary"*|*"node_2=wait_primary"*) break ;; esac; sleep 1
done
echo "state after $i s: $S"
docker exec pk-mon psql -X -At -U postgres -d pg_auto_failover -c "select to_char(eventtime, 'HH24:MI:SS') || ' ' || description from (select * from pgautofailover.event order by eventid desc limit 12) e order by eventid" | cut -c1-200
sleep 12
docker exec pk-sub touch /tmp/stop; sleep 3
docker cp pk-sub:/tmp/writer.log /tmp/pk-writer.log
sleep 5
echo "== key=$K node 2 clock behind by ${SK}s run=$RUN"
python3 - <<'PY'
import re
calls=[]; cur=None
for l in open('/tmp/pk-writer.log', errors='replace'):
    l=l.rstrip('\n')
    if l.startswith('CALL '): cur={'t':int(l[5:])/1e9,'ids':[],'err':[]}; calls.append(cur)
    elif cur is not None:
        if re.fullmatch(r'[0-9a-f-]{20,}|\d{10,}', l): cur['ids'].append(l.zfill(20) if l.isdigit() else l)
        elif l.strip() and not l.startswith('INSERT'): cur['err'].append(l)
ok=[i for i,c in enumerate(calls) if c['ids']]
# the outage: the first call without ids after calls with ids, up to the next call with ids
first_fail=next(i for i in range(len(calls)) if not calls[i]['ids'] and i>ok[0] and any('duplicate' not in e for e in calls[i]['err']))
last_ok=max(i for i in ok if i<first_fail)
resume=next(i for i in ok if i>first_fail)
old=calls[:last_ok+1]; new=calls[resume:]
old_ids=[x for c in old for x in c['ids']]; new_ids=[x for c in new for x in c['ids']]
mx=max(old_ids)
dup=sum(1 for c in new for e in c['err'] if 'duplicate key' in e)
print(f"writes paused {calls[resume]['t']-calls[last_ok]['t']:.0f} s; ids acknowledged before: {len(old_ids)}, after: {len(new_ids)}")
print(f"batches refused with a duplicate key after the failover: {dup} (each of 1,000 rows)")
print(f"ids made after the failover below the highest one made before: {sum(1 for x in new_ids if x < mx)}")
open('/tmp/pk-ack.txt','w').write('\n'.join(old_ids+new_ids))
PY
psqlx pk-n2 -At -c "select id::text from t" > /tmp/pk-final.txt 2>/dev/null
python3 - <<'PY'
ack=set(l.strip() for l in open('/tmp/pk-ack.txt') if l.strip())
fin=set(l.strip().zfill(20) if l.strip().isdigit() else l.strip() for l in open('/tmp/pk-final.txt') if l.strip())
print(f"acknowledged and missing from the new primary: {len(ack-fin)} of {len(ack)}")
PY
echo "new primary holds $(psqlx pk-n2 -At -c 'select count(*) from t') rows; subscriber holds $(psqlx pk-sub -At -c 'select count(*) from t') rows"
psqlx pk-sub -At -F ' | ' -c "select 'subscription worker running', count(*) > 0 from pg_stat_subscription where pid is not null and subid = (select oid from pg_subscription where subname = 'sub')" -c "select 'apply errors', apply_error_count, 'insert_exists conflicts', coalesce(confl_insert_exists, 0) from pg_stat_subscription_stats"
[ -z "$KEEP" ] && for c in pk-mon pk-n1 pk-n2 pk-sub; do docker rm -f $c >/dev/null 2>&1; done

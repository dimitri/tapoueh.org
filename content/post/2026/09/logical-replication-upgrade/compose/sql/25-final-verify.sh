# @service: traffic
# Step 25: stop the traffic, let both directions drain, compare old and new
# table by table, and check that no acknowledged write was lost.
. /work/lib.sh
touch /tmp/traffic.stop
for i in $(seq 1 100); do [ -e /tmp/traffic.done ] && break; sleep 0.2; done
echo "traffic stopped: $([ -e /tmp/traffic.done ] && echo yes || echo NO)"
sleep 3   # both subscriptions drain (writes are quiet; latency is milliseconds)

echo; echo "### traffic summary (attempt counts depend on timing)"
awk '{ n[$3 " " $4]++ } END { for (k in n) printf "%-8s %d\n", k, n[k] }' /tmp/traffic.log | sort
echo "server versions that acknowledged writes:"
awk '$4 == "ok" { n[$3 " " $6]++ } END { for (k in n) print "  " k, n[k] }' /tmp/traffic.log | sort

echo; echo "### table by table: row count and content hash, old vs new"
tables="customers orders audit_log counters cutover_markers measurements documents late_table"
for t in $tables; do
  for h in old new; do
    v=$(qa $h app "SELECT count(*) || ' ' || md5(coalesce(string_agg(md5(x::text), '' ORDER BY md5(x::text)), '')) FROM $t x")
    printf '%-13s %-4s %s\n' "$t" "$h" "$v"
  done
  a=$(qa old app "SELECT md5(coalesce(string_agg(md5(x::text), '' ORDER BY md5(x::text)), '')) FROM $t x")
  b=$(qa new app "SELECT md5(coalesce(string_agg(md5(x::text), '' ORDER BY md5(x::text)), '')) FROM $t x")
  [ "$a" = "$b" ] && echo "    -> identical" || echo "    -> DIFFERENT"
done

echo; echo "### lost or ambiguous writes: acknowledged client_seq versus rows in orders"
awk '$4 == "ok" { print $2 }' /tmp/traffic.log > /tmp/acked.txt
for h in old new; do
  psql -X -h $h -d app <<SQL
CREATE TEMP TABLE acked (seq bigint);
\copy acked FROM '/tmp/acked.txt'
SELECT '$h' AS server,
       count(*) AS acked,
       count(*) FILTER (WHERE o.id IS NULL) AS acked_but_missing
  FROM acked a LEFT JOIN orders o ON o.client_seq = a.seq;
SELECT '$h' AS server, count(*) AS committed_but_not_acked
  FROM orders o WHERE o.client_seq > 0 AND NOT EXISTS (SELECT 1 FROM acked a WHERE a.seq = o.client_seq);
SQL
done

echo; echo "### the two switches"
stall_report old new | grep -E 'STALL|failed'
stall_report new old | grep -E 'STALL|failed'
echo; echo "### final state of both replication directions"
q old app "SELECT subname, subenabled, suborigin FROM pg_subscription"
q new app "SELECT subname, subenabled, suborigin FROM pg_subscription"
q old app "SELECT slot_name, active FROM pg_replication_slots ORDER BY 1"
q new app "SELECT slot_name, active FROM pg_replication_slots ORDER BY 1"
:

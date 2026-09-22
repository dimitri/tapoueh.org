# @service: traffic
# Step 17: watch the initial sync. Lines starting with [poll] are timing
# dependent and ignored by verify.sh.
. /work/lib.sh
t0=$(now_ms)
for _ in $(seq 1 240); do
  st=$(qa new app "SELECT string_agg(srsubstate::text || '=' || n, ' ' ORDER BY srsubstate) FROM (SELECT srsubstate, count(*) n FROM pg_subscription_rel GROUP BY 1) s")
  echo "[poll] +$(( $(now_ms) - t0 ))ms rel_states: $st"
  [[ $st =~ ^r=[0-9]+$ ]] && break
  sleep 0.5
done
echo "initial sync of all tables finished"
q new app "SELECT srrelid::regclass AS tbl, srsubstate, srsublsn IS NOT NULL AS has_lsn FROM pg_subscription_rel ORDER BY srrelid::regclass::text"
q new app "SELECT subname, worker_type, pid IS NOT NULL AS running, received_lsn IS NOT NULL AS has_received_lsn, latest_end_lsn IS NOT NULL AS has_latest_end_lsn FROM pg_stat_subscription"
q new app "SELECT subname, apply_error_count, sync_error_count FROM pg_stat_subscription_stats"
q old app "SELECT slot_name, plugin, slot_type, active, confirmed_flush_lsn IS NOT NULL AS has_confirmed_flush FROM pg_replication_slots"
q old app "SELECT application_name, state, sync_state FROM pg_stat_replication"

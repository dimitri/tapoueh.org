# @service: traffic
# Step 23: the app now writes to the NEW server. Chores that replication does
# not do (they do not block the app).
. /work/lib.sh
echo "### app is on: $(cat /tmp/target); last 3 log lines:"; tail -3 /tmp/traffic.log | cut -d' ' -f2-
echo; echo "### materialized view: refresh it on new (a snapshot, replication never maintains it)"
q new app "REFRESH MATERIALIZED VIEW CONCURRENTLY sales_by_day"
echo; echo "### large object: copied by hand with the same OID"
psql -X -qAt -h old -d app -c "SELECT format('SELECT lo_from_bytea(%s, %L);', oid, lo_get(oid)) FROM pg_largeobject_metadata" | psql -X -qAt -h new -d app
q new app "SELECT id, length(lo_get(body_oid)) AS lo_bytes FROM documents"
echo; echo "### the sequences carry on where the old ones stopped"
q new app "SELECT sequencename, last_value IS NOT NULL AS has_value FROM pg_sequences ORDER BY 1"
echo; echo "### writes made on new are flowing back to old through app_rev (origin = none)"
sleep 2
new_max=$(qa new app "SELECT max(client_seq) FROM orders"); old_max=$(qa old app "SELECT max(client_seq) FROM orders")
echo "[poll] newest client_seq new=$new_max old=$old_max"
q old app "SELECT subname, apply_error_count FROM pg_stat_subscription_stats"
q new app "SELECT subname, apply_error_count FROM pg_stat_subscription_stats"
:

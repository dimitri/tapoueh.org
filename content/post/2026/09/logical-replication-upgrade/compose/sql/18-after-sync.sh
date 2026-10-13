# @service: traffic
# Step 18: initial sync is done and the app is still writing to the old server.
# What is NOT on the new server?
. /work/lib.sh
echo "### sequences: old (live) vs new"
q old app "SELECT sequencename, last_value FROM pg_sequences ORDER BY 1"
q new app "SELECT sequencename, last_value FROM pg_sequences ORDER BY 1"
echo "### consequence: an INSERT on new would reuse ids already replicated"
psql -X -h new -d app <<'SQL' 2>&1
BEGIN;
INSERT INTO orders (customer_id, amount) VALUES (1, 1);
ROLLBACK;
SQL
echo; echo "### materialized view: created WITH NO DATA by the schema dump, and replication never fills it"
q new app "SELECT count(*) FROM sales_by_day"
q new app "REFRESH MATERIALIZED VIEW sales_by_day"
q new app "SELECT count(*) AS days FROM sales_by_day"
echo; echo "### large objects: the documents row was replicated, the object behind body_oid was not"
q old app "SELECT id, title, length(lo_get(body_oid)) AS lo_bytes FROM documents"
q new app "SELECT id, title, body_oid IS NOT NULL AS has_oid FROM documents"
q new app "SELECT count(*) AS large_objects FROM pg_largeobject_metadata"
q new app "SELECT length(lo_get(body_oid)) FROM documents"
echo; echo "### the ordinary data is there, generated column recomputed on the subscriber"
for h in old new; do
  q $h app "SELECT count(*) AS seed_orders, sum(vat) AS vat FROM orders WHERE client_seq < 0"
done
q new app "SELECT count(*) AS partition_rows_2026 FROM measurements_2026"
:

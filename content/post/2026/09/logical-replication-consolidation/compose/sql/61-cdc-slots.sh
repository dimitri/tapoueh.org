# Step 4b: create the consumer slots with the PG18 client tools, inside the warehouse container.
# cdc_td: test_decoding (human readable). cdc_pg: pgoutput (what Debezium uses).
"${DC[@]}" exec -T warehouse pg_recvlogical -U postgres -d warehouse --slot cdc_td --create-slot -P test_decoding </dev/null 2>&1; echo "exit=$?"
"${DC[@]}" exec -T warehouse pg_recvlogical -U postgres -d warehouse --slot cdc_pg --create-slot -P pgoutput </dev/null 2>&1; echo "exit=$?"
sq warehouse warehouse -c "select slot_name, plugin, slot_type, database, active, wal_status from pg_replication_slots order by 1" </dev/null
echo '--- slots are per database: a slot created in "postgres" sees nothing of "warehouse"'
"${DC[@]}" exec -T warehouse pg_recvlogical -U postgres -d postgres --slot cdc_other_db --create-slot -P test_decoding </dev/null 2>&1
sq warehouse warehouse -c "select slot_name, database from pg_replication_slots order by 1" </dev/null
sq warehouse warehouse -c "select pg_drop_replication_slot('cdc_other_db')" </dev/null >/dev/null

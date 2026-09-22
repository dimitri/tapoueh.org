# Step 4f: the real consumer tooling. pg_recvlogical streams until --endpos, here the current WAL end.
end=$(qt warehouse warehouse "select pg_current_wal_lsn()")
echo '--- test_decoding via pg_recvlogical (consumes the slot up to endpos):'
"${DC[@]}" exec -T warehouse pg_recvlogical -U postgres -d warehouse --slot cdc_td --start --endpos "$end" -f - \
  -o include-xids=0 -o skip-empty-xacts=1 </dev/null 2>&1
echo '--- pgoutput via pg_recvlogical (binary; only the relation names, origin names and two values are extracted):'
"${DC[@]}" exec -T warehouse sh -c "pg_recvlogical -U postgres -d warehouse --slot cdc_pg --start --endpos '$end' -f - -o proto_version=1 -o publication_names=cdc_pub </dev/null | grep -a -o -E '(shop|crm|billing)\\.[a-z_]+|pg_[0-9]+|Ivy|Umbrella'" 2>&1

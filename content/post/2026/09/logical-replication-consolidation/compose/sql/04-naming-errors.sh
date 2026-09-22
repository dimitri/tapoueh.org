# @nosync
# Step 1e: what does the subscriber say? Wait for both sync errors, then quote the log.
wait_until warehouse warehouse "select sync_error_count >= 2 from pg_stat_subscription_stats where subname='sub_naming_crm'" 60
echo '--- warehouse.public.customers (only the first source made it):'
sq warehouse warehouse -c "select * from public.customers order by id" </dev/null
echo '--- sub_naming_crm table states (d = data copy, retrying forever):'
sq warehouse warehouse -c "select srrelid::regclass as rel, srsubstate from pg_subscription_rel r join pg_subscription s on s.oid=r.srsubid where s.subname='sub_naming_crm' order by 1::text" </dev/null
echo '--- server log:'
wlog warehouse 'ERROR|DETAIL|CONTEXT' | grep -E 'customers|contacts|duplicate|missing|already exists' | grep -v 'STATEMENT'
echo '--- error counters:'
sq warehouse warehouse -c "select subname, apply_error_count, sync_error_count > 0 as has_sync_errors from pg_stat_subscription_stats where subname like 'sub_naming%' order by 1" </dev/null

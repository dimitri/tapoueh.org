# @nosync
# Layout (c) with OVERLAPPING keys: shop and crm both have customers 1 and 2.
echo '### 1. crm joins the same table: primary key (id) collides'
sq warehouse warehouse -qc "set role sub_crm" -c "create subscription sub_c_crm connection 'host=crm dbname=crm user=repl password=repl' publication pub_c" </dev/null
wait_until warehouse warehouse "select sync_error_count >= 1 from pg_stat_subscription_stats where subname='sub_c_crm'" 60
wlog warehouse 'duplicate key value|already exists' | grep -v customers_pkey_x
echo '### 2. widen the subscriber key to (source, id); the retry of the initial copy now succeeds and the trigger stamps crm'
sq warehouse warehouse -qc "update public.customers set source = 'shop' where source = 'unknown'" -c "alter table public.customers drop constraint customers_pkey, add primary key (source, id)" </dev/null
sync_all
sq warehouse warehouse -c "select source, id, name from public.customers where id < 10 order by source, id" </dev/null
echo '### 3. first UPDATE from the shop'
sq shop shop -qc "update public.customers set city = 'Marseille' where id = 1" </dev/null
wait_until warehouse warehouse "select apply_error_count >= 1 from pg_stat_subscription_stats where subname='sub_c_shop'" 60
wlog warehouse 'publisher did not send' | head -1
sq warehouse warehouse -c "select subname, apply_error_count > 0 as apply_is_failing from pg_stat_subscription_stats where subname in ('sub_c_shop','sub_c_crm') order by 1" </dev/null
echo '### 4. workaround attempt: REPLICA IDENTITY FULL on the subscriber and on the publishers'
sq warehouse warehouse -qc "alter table public.customers replica identity full" </dev/null
echo '(still failing after subscriber FULL only, apply_error_count keeps growing:)'
wait_until warehouse warehouse "select apply_error_count >= 3 from pg_stat_subscription_stats where subname='sub_c_shop'" 60
sq warehouse warehouse -qc "select 'still erroring: ' || (apply_error_count >= 3) from pg_stat_subscription_stats where subname='sub_c_shop'" -At </dev/null
echo '### 4b. the failed UPDATE was decoded with the old identity and will never apply: skip that transaction'
lsn=$(wlog warehouse 'finished at' | sed -E 's/.*finished at ([0-9A-F]+\/[0-9A-F]+).*/\1/' | sort -u | head -1)
echo "ALTER SUBSCRIPTION sub_c_shop SKIP (lsn = '<LSN of the failed transaction from the CONTEXT line>')"
sq warehouse warehouse -qc "alter subscription sub_c_shop skip (lsn = '$lsn')" </dev/null
sync_all
echo '### 5. FULL on both publishers + subscriber without the (source,id) key: try an index whose leading column is replicated'
sq shop shop -qc "alter table public.customers replica identity full" </dev/null
sq crm crm -qc "alter table public.customers replica identity full" </dev/null
sq warehouse warehouse -qc "alter table public.customers drop constraint customers_pkey" -c "create unique index customers_id_source on public.customers (id, source)" </dev/null
sync_all
sq crm crm -qc "update public.customers set city = 'Brest' where id = 1" </dev/null
sync_all
echo '--- subscriber content: the updates are NOT applied, and apply reports no error:'
sq warehouse warehouse -c "select source, id, name, city from public.customers where id < 10 order by source, id" </dev/null
sq warehouse warehouse -c "select subname, apply_error_count > 0 as had_apply_errors, confl_update_missing from pg_stat_subscription_stats where subname in ('sub_c_shop','sub_c_crm') order by 1" </dev/null
wlog warehouse 'conflict detected|Could not find the row' | sort -u

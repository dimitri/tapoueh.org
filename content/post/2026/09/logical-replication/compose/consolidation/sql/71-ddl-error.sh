# @nosync
# Step 5b: the apply worker fails and retries forever; the second order queues behind the first.
wait_until warehouse warehouse "select apply_error_count >= 2 from pg_stat_subscription_stats where subname='sub_shop'" 60
wlog warehouse 'shop.orders" is missing|for replication target relation "shop.orders"' | head -2
sq warehouse warehouse -c "select subname, apply_error_count > 0 as apply_is_failing from pg_stat_subscription_stats where subname = 'sub_shop'" </dev/null
sq warehouse warehouse -c "select count(*) as orders_after_note_insert from shop.orders where status = 'new' and amount in (5.00, 6.00)" </dev/null
sq shop shop -c "select slot_name, pg_wal_lsn_diff(pg_current_wal_lsn(), confirmed_flush_lsn) > 0 as unconfirmed_wal from pg_replication_slots where slot_name = 'sub_shop'" </dev/null
echo '--- fix: add the column on the subscriber; the worker retries by itself (wal_retrieve_retry_interval 5s)'
sq warehouse warehouse -qc "alter table shop.orders add column note text" </dev/null
sync_all
sq warehouse warehouse -c "select amount, note from shop.orders where amount in (5.00, 6.00) order by amount" </dev/null
echo '--- right order for the next additive change: subscriber first, then publisher'
sq warehouse warehouse -qc "alter table shop.orders add column priority int" </dev/null
sq shop shop -qc "alter table shop.orders add column priority int" -c "insert into shop.orders (customer_id, amount, status, tenant, priority) values (1, 7.00, 'new', 'eu', 1)" </dev/null
sync_all
sq warehouse warehouse -c "select amount, priority from shop.orders where amount = 7.00" </dev/null
echo '--- dropping a column: publisher first is fine, the subscriber keeps a NULL column'
sq shop shop -qc "alter table shop.orders drop column priority" -c "insert into shop.orders (customer_id, amount, status, tenant) values (2, 8.00, 'new', 'eu')" </dev/null
sync_all
sq warehouse warehouse -c "select amount, priority from shop.orders where amount in (7.00, 8.00) order by amount" </dev/null

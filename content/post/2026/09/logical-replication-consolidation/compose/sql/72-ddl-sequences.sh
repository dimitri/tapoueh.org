# Step 5c: sequence values are not replicated (PG18): the ids arrive, the sequence does not move.
echo '--- publisher (shop):'
sq shop shop -c "select last_value, is_called from shop.orders_id_seq" </dev/null
echo '--- subscriber (warehouse):'
sq warehouse warehouse -c "select last_value, is_called from shop.orders_id_seq" </dev/null
echo '--- a local insert on the subscriber (e.g. after a failover/cutover) reuses id 1:'
sq warehouse warehouse -c "insert into shop.orders (customer_id, amount, status, tenant) values (1, 1.00, 'local', 'eu')" </dev/null
sq warehouse warehouse -c "select last_value, is_called from shop.orders_id_seq" </dev/null

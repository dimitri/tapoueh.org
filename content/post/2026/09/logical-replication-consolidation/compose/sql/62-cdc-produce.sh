# Step 4c: activity on the sources, with recognisable transaction shapes.
# shop: ONE transaction touching two tables (customers and orders).
sq shop shop -c "begin; insert into shop.customers values (10, 3, 'Ivy', 'ivy@example.com', '+33 1 00 00 00 10', 'FR', 'eu'); insert into shop.orders (customer_id, amount, status, tenant) values (10, 42.00, 'new', 'eu'); commit;" </dev/null
# shop: two separate transactions on the same table.
sq shop shop -qc "insert into shop.orders (customer_id, amount, status, tenant) values (10, 1.00, 'new', 'eu')" </dev/null
sq shop shop -qc "update shop.orders set status = 'paid' where amount = 1.00" </dev/null
# crm and billing, one transaction each.
sq crm crm -qc "insert into crm.accounts values (4, 'Umbrella', 'gold')" </dev/null
sq billing billing -qc "insert into billing.invoices (account_id, amount, status) values (4, 99.00, 'open')" </dev/null
sync_all
# a change made directly on the warehouse (no replication origin): a local payment
sq warehouse warehouse -qc "insert into billing.payments (id, invoice_id, amount) values (900, 3, 1.00)" </dev/null
echo "produced"

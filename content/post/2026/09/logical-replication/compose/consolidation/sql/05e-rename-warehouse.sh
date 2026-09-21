# @nosync
# Step 1k: the subscription now finds shopapp.orders on both sides.
sq warehouse warehouse -c "create subscription sub_rename connection 'host=shop dbname=shop user=repl password=repl' publication pub_rename" </dev/null
wait_until warehouse warehouse "select count(*) = 1 and bool_and(srsubstate = 'r') from pg_subscription_rel r join pg_subscription s on s.oid = r.srsubid where s.subname = 'sub_rename'" 60
show warehouse warehouse "select id, customer_id, amount from shopapp.orders order by id"

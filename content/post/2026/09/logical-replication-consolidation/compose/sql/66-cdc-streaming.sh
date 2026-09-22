# Step 4g (iv): a LARGE source transaction with streaming on. What does the downstream slot see,
# and when? logical_decoding_work_mem is 64kB everywhere (docker-compose.yml) so 20000 rows exceed it.
sq shop shop -c "show logical_decoding_work_mem" </dev/null
sq warehouse warehouse -c "select subname, substream from pg_subscription where subname = 'sub_shop'" </dev/null

# downstream consumer with in-progress streaming enabled (test_decoding option stream-changes)
"${DC[@]}" exec -T warehouse sh -c 'rm -f /tmp/td.out; nohup pg_recvlogical -U postgres -d warehouse --slot cdc_td --start -f /tmp/td.out -o include-xids=0 -o skip-empty-xacts=1 -o stream-changes=1 >/tmp/td.err 2>&1 & echo $! > /tmp/td.pid' </dev/null

big_txn() {  # $1 = commit | rollback, $2 = status tag of the rows
  echo "### large source transaction, ending with $1"
  "${DC[@]}" exec -T warehouse sh -c ': > /tmp/td.out' </dev/null
  # barrier: session A takes advisory lock 4242 and sleeps; session B does the big insert, then blocks on the lock before ending the transaction
  "${DC[@]}" exec -d shop psql -X -U postgres -d shop -c "select pg_advisory_lock(4242)" -c "select pg_sleep(600)" </dev/null
  wait_until shop shop "select count(*) = 1 from pg_locks where locktype = 'advisory' and granted" 30
  "${DC[@]}" exec -d shop psql -X -U postgres -d shop -c "begin" \
     -c "insert into shop.orders (customer_id, amount, status, tenant) select 1, 1.00, '$2', 'eu' from generate_series(1, 20000)" \
     -c "select pg_advisory_lock(4242)" -c "$1" </dev/null
  wait_until shop shop "select count(*) = 1 from pg_locks where locktype = 'advisory' and not granted" 30
  # the source transaction is still open. Has anything reached the downstream slot?
  wait_file warehouse /tmp/td.out 'streaming change for transaction' 60
  echo "downstream already streams while the source transaction is open: yes"
  echo "rows tagged '$2' visible on the warehouse while the source transaction is open: $(qt warehouse warehouse "select count(*) from shop.orders where status = '$2'")"
  # release the barrier: the source transaction ends
  sq shop shop -qAt -c "select count(pg_terminate_backend(pid)) from pg_stat_activity where query like '%pg_sleep(600)%' and pid <> pg_backend_pid()" </dev/null >/dev/null
  if [ "$1" = commit ]; then pat='committing streamed transaction'; else pat='aborting streamed'; fi
  wait_file warehouse /tmp/td.out "$pat" 60
  echo "--- downstream output: distinct line kinds (a streamed transaction has no BEGIN line)"
  "${DC[@]}" exec -T warehouse sh -c "grep -v 'streaming change for transaction' /tmp/td.out | sort -u" </dev/null
  echo "first line: $("${DC[@]}" exec -T warehouse head -1 /tmp/td.out </dev/null)"
  echo "last line:  $("${DC[@]}" exec -T warehouse tail -1 /tmp/td.out </dev/null)"
  n=$("${DC[@]}" exec -T warehouse grep -c 'streaming change for transaction' /tmp/td.out </dev/null)
  echo "streamed changes delivered before the end of the transaction: $([ "$n" -gt 10000 ] && echo 'more than 10000' || echo "$n")"
  echo "streamed blocks: $([ "$("${DC[@]}" exec -T warehouse grep -c 'opening a streamed block' /tmp/td.out </dev/null)" -gt 1 ] && echo 'several' || echo 'one')"
  echo "rows tagged '$2' visible on the warehouse at the end: $(qt warehouse warehouse "select count(*) from shop.orders where status = '$2'")"
}
big_txn commit bulk_c
big_txn rollback bulk_r
"${DC[@]}" exec -T warehouse sh -c 'kill $(cat /tmp/td.pid)' </dev/null
sq warehouse warehouse -c "select slot_name, stream_txns > 0 as streamed, stream_count > 0 as stream_blocks from pg_stat_replication_slots where slot_name = 'cdc_td'" </dev/null

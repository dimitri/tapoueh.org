# @service: traffic
# Step 24: ROLLBACK DEMO. Pretend the new server misbehaves: switch the traffic
# back to the old one. Same procedure, mirrored: the reverse subscription
# (app_rev, on old) is now the one to wait for, and the sequences are copied
# from new to old.
. /work/lib.sh
T0=$(now_ms)

killed=$(freeze new)
echo "[time] +$(ms) ms new frozen, $killed app sessions terminated"
catch_up new old app_rev rollback-new-to-old
echo "caught up: yes"
unfreeze old                # old was frozen at step 22
copy_sequences new old
echo "[time] +$(ms) ms old unfrozen, sequences copied back ($(wc -l < /tmp/setval.sql) statements)"
echo old > /tmp/target
echo "[time] +$(ms) ms app switched back to old"
sleep 4
echo; echo "### what the application saw"
stall_report new old
echo; echo "### old is writable again and both directions still work"
q old app "SHOW default_transaction_read_only"
q old app "SELECT subname, apply_error_count FROM pg_stat_subscription_stats"
:

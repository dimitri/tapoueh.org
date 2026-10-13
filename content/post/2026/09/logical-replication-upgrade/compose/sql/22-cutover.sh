# @service: traffic
# Step 22: THE CUTOVER, timed. The traffic loop keeps running the whole time.
#  freeze   : ALTER DATABASE app SET default_transaction_read_only = on, then
#             pg_terminate_backend() on the app role's sessions. New sessions and
#             reconnects get "cannot execute INSERT in a read-only transaction";
#             reads still work; replication and the DBA (who overrides it in
#             his own session) are not affected.
#  catch up : see catch_up() in traffic/lib.sh (marker row + LSN comparisons)
#  sequences: setval() on new, statements generated from old.
#  switch   : the app target file now says "new".
. /work/lib.sh
T0=$(now_ms)

killed=$(freeze old)
echo "[time] +$(ms) ms old frozen, $killed app sessions terminated"
catch_up old new app_sub cutover-old-to-new
echo "caught up: yes"
copy_sequences old new
echo "[time] +$(ms) ms sequences copied ($(wc -l < /tmp/setval.sql) setval statements)"
echo new > /tmp/target
echo "[time] +$(ms) ms app switched to new"
sleep 3
echo; echo "### generated statements (numbers masked in the diff, they depend on traffic)"
cat /tmp/setval.sql
echo; echo "### what the application saw"
stall_report old new
:

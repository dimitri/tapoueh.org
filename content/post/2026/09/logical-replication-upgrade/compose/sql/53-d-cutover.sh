# @service: traffic2
# Step 53 (part D): THE CUTOVER, behind pgbouncer. The traffic loop keeps
# running against pgbouncer the whole time - its /tmp/target is never
# touched, unlike part A's step 22.
#  PAUSE    : the write barrier. Waits for every in-flight transaction on
#             old2 to finish, then holds new ones - this is the freeze()
#             of this architecture, and it has to come FIRST, same as
#             part A's freeze-before-sequences order: an earlier version of
#             this script ran copy_sequences before PAUSE and got
#             "duplicate key value violates unique constraint" after the
#             switch, from rows old2 kept committing in the gap.
#  catch up : same marker-row technique as part A (direct to old2/new2,
#             bypassing pgbouncer - this is the operator's own connection).
#  sequences: setval() on new2, statements generated from old2, now that
#             old2 can take no more writes.
#  cutover  : edit databases.ini old2 -> new2 (the bind mount shared with
#             the pgbouncer container); RELOAD; RESUME app. The client's
#             TCP connection to pgbouncer is never closed; only the
#             pgbouncer -> backend hop changes.
. /work/lib.sh
T0=$(now_ms)

FROM_V=$(qa old2 app "SELECT current_setting('server_version_num')")
TO_V=$(qa new2 app "SELECT current_setting('server_version_num')")

# PGOPTIONS from lib.sh (used by part A's freeze()) is not a parameter
# pgbouncer forwards: a connection carrying it is rejected outright unless
# it is listed in ignore_startup_parameters. Not needed here - drop it
# before the first pgbouncer-routed admin command.
unset PGOPTIONS

qa pgbouncer pgbouncer "PAUSE app" >/dev/null
echo "[time] +$(ms) ms PAUSE app (in-flight transactions finished, new ones held)"

catch_up old2 new2 app_sub2 cutover-d-marker
echo "caught up: yes"
copy_sequences old2 new2
echo "[time] +$(ms) ms sequences copied ($(wc -l < /tmp/setval.sql) setval statements)"

sed -i 's/host=old2/host=new2/' /pgbouncer-cfg/databases.ini
echo "[time] +$(ms) ms databases.ini edited (old2 -> new2)"
qa pgbouncer pgbouncer "RELOAD" >/dev/null
echo "[time] +$(ms) ms RELOAD"
qa pgbouncer pgbouncer "RESUME app" >/dev/null
echo "[time] +$(ms) ms RESUME app"
sleep 2

echo
echo "### generated statements (numbers masked in the diff, they depend on traffic)"
cat /tmp/setval.sql
echo
echo "### what the application saw - /tmp/target was never touched"
cat /tmp/target
echo
echo "### the backend behind pgbouncer, before and after (from the traffic log itself)"
stall_report_backend "$FROM_V" "$TO_V"
:

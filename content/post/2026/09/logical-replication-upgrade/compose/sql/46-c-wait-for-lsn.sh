# @service: pg19a
# Step 46 (part C, 19 BETA): WAIT FOR LSN on a logical SUBSCRIBER. It is a
# physical-standby feature; its 'primary_flush' mode looks at the LOCAL WAL.
# Uses the subscription sub_all of step 41 (pg19b, database seq).
. /work/lib.sh
B() { q pg19b "$1" "$2"; }
B seq "SELECT pg_is_in_recovery()"
pub=$(qa pg19a seq "SELECT pg_current_wal_lsn()")
echo "publisher LSN lower than the subscriber's own current LSN: $(qa pg19b seq "SELECT '$pub'::pg_lsn < pg_current_wal_lsn()") (values not shown)"
for mode in standby_replay standby_flush primary_flush; do
  B seq "WAIT FOR LSN '$pub' WITH (MODE '$mode', TIMEOUT '500ms')"
done
echo "### the answer 'success' above only means: the SUBSCRIBER's own WAL passed that number. Not that it applied the publisher's changes."
B seq "WAIT FOR LSN 'FFFF/0' WITH (MODE 'primary_flush', TIMEOUT '300ms')"
B seq "WAIT FOR LSN 'FFFF/0' WITH (MODE 'primary_flush', TIMEOUT '300ms', NO_THROW)"
:

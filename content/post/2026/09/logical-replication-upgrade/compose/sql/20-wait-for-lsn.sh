# @service: traffic
# Step 20: can WAIT FOR LSN tell us the subscriber has caught up? It is a
# physical-standby (recovery) feature; test it on the subscriber (18.x) and on
# the old primary (16.x).
. /work/lib.sh
q new app "SELECT version()"
q new app "SELECT pg_is_in_recovery()"
q new app "WAIT FOR LSN '0/1'"
q old app "WAIT FOR LSN '0/1'"
echo; echo "### the working alternative: compare positions"
q old app "SELECT pg_current_wal_lsn() IS NOT NULL AS old_position_available"
q new app "SELECT external_id, remote_lsn IS NOT NULL AS has_remote_lsn FROM pg_replication_origin_status ORDER BY 1"
q new app "SELECT subname, latest_end_lsn IS NOT NULL AS has_latest_end_lsn FROM pg_stat_subscription WHERE worker_type = 'apply'"
:

# (sync_all is called inside)
# Which identity does the apply worker have inside a trigger? Try the four combinations of
# subscription owner (regular role vs superuser) and run_as_owner (false is the default).
# One insert per case on the shop, then read what the trigger recorded.
id=10
for owner in sub_shop postgres; do
  for rao in false true; do
    sq warehouse warehouse -qc "alter subscription sub_c_shop owner to $owner" -c "alter subscription sub_c_shop set (run_as_owner = $rao)" </dev/null
    sq shop shop -qc "insert into public.customers values ($id, 'case owner=$owner run_as_owner=$rao', 'Test')" </dev/null
    sync_all
    id=$((id+1))
  done
done
sq warehouse warehouse -x -c "select id, source, name from public.customers where id >= 10 order by id" </dev/null
sq warehouse warehouse -c "select p.n, p.op, p.cur_user, p.sess_user, p.origin_is_setup as origin_setup, p.sub_from_pid, p.origin_from_pid from public.probe p order by n" </dev/null
sq warehouse warehouse -c "select n, application_name, role_guc, backend_type from public.probe order by n" </dev/null
echo '--- origin catalog on the subscriber:'
sq warehouse warehouse -c "select s.subname, 'pg_' || s.oid as expected_origin_name, o.roname = 'pg_' || s.oid as matches from pg_subscription s left join pg_replication_origin o on o.roname = 'pg_' || s.oid where s.subname in ('sub_c_shop') order by 1" </dev/null
sq warehouse warehouse -c "select column_name from information_schema.columns where table_name = 'pg_replication_origin_status' order by ordinal_position" </dev/null
echo '--- functions that mention the session origin:'
sq warehouse warehouse -c "select proname, pg_get_function_identity_arguments(oid) as args, pg_get_function_result(oid) as returns from pg_proc where proname like 'pg_replication_origin%' order by 1" </dev/null
sq warehouse warehouse -c "select p.proname, has_function_privilege('public', p.oid, 'execute') as public_can_execute from pg_proc p where p.proname in ('pg_replication_origin_session_is_setup','pg_replication_origin_session_progress','pg_replication_origin_oid') order by 1" </dev/null
# put the subscription back to its normal shape for the next steps
sq warehouse warehouse -qc "alter subscription sub_c_shop owner to sub_shop" -c "alter subscription sub_c_shop set (run_as_owner = false)" </dev/null

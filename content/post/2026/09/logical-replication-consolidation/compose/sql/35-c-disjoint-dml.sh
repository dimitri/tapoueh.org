sq warehouse warehouse -c "select source, id, name, city from public.customers order by id" </dev/null
echo '--- insert, update and delete on both sources'
sq shop shop -qc "insert into public.customers values (3, 'Eve (shop)', 'Nice')" -c "update public.customers set city = 'Marseille' where id = 1" -c "delete from public.customers where id = 2" </dev/null
sq crm crm -qc "insert into public.customers values (103, 'Fay (crm)', 'Metz')" -c "update public.customers set city = 'Brest' where id = 101" -c "delete from public.customers where id = 102" </dev/null
sync_all
sq warehouse warehouse -c "select source, id, name, city from public.customers order by id" </dev/null
echo '--- a local insert on the warehouse (not an apply worker) gets the default of its own session_user:'
sq warehouse warehouse -qc "insert into public.customers (id, name) values (900, 'local row')" </dev/null
sq warehouse warehouse -c "select source, id from public.customers where id = 900" </dev/null
sq warehouse warehouse -c "select subname, apply_error_count, confl_update_missing, confl_delete_missing from pg_stat_subscription_stats where subname in ('sub_c_shop','sub_c_crm') order by 1" </dev/null

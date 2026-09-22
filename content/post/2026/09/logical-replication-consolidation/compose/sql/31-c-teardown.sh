# @nosync
# Tear down the overlapping-keys experiment before showing the variant that does work.
for s in sub_c_shop sub_c_crm; do
  sq warehouse warehouse -qc "alter subscription $s disable" -c "alter subscription $s set (slot_name = none)" -c "drop subscription $s" </dev/null
done
for h in shop crm; do
  sq $h $h -qAt -c "select count(pg_drop_replication_slot(slot_name)) from pg_replication_slots where slot_name in ('sub_c_shop','sub_c_crm')" </dev/null >/dev/null
  sq $h $h -qc "drop publication pub_c" -c "drop table public.customers" </dev/null
done
sq warehouse warehouse -qc "drop table public.customers" </dev/null
echo "overlap experiment removed"

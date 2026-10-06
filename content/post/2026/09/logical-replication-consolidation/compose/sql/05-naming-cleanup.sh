# @nosync
# Step 1f: clean up the naming experiment (subscriptions that error must be disabled before DROP).
for s in sub_naming_crm sub_naming_shop; do
  sq warehouse warehouse -qc "alter subscription $s disable" -c "alter subscription $s set (slot_name = none)" </dev/null
  sq warehouse warehouse -qc "drop subscription $s" </dev/null
done
sq warehouse warehouse -qc "drop table public.customers, public.contacts" </dev/null
sq shop shop -qc "drop publication pub_naming" -c "drop table public.customers, public.contacts" </dev/null
sq crm crm -qc "drop publication pub_naming" -c "drop table public.customers, public.contacts" </dev/null
# slots were left on the publishers because of slot_name = none: drop them explicitly
for h in shop crm; do
  sq $h $h -qAt -c "select count(pg_drop_replication_slot(slot_name)) as dropped from pg_replication_slots where slot_name like 'sub_naming%' or slot_name like 'pg_%_sync_%'" </dev/null
done
echo "remaining slots on shop/crm:"
qt shop shop "select count(*) from pg_replication_slots"
qt crm crm "select count(*) from pg_replication_slots"

# @nosync
# Layout (b) cost: the source now streams every change twice (layout (a) + layout (b) subscriptions).
for h in shop crm billing; do
  echo "--- slots on $h:"
  sq $h $h -c "select slot_name, plugin, slot_type, active from pg_replication_slots order by 1" </dev/null
done
# drop layout (b): we keep layout (a) for the rest of the demo
for d in wh_shop wh_crm wh_billing; do
  s=sub_b_${d#wh_}
  sq warehouse $d -qc "alter subscription $s disable" -c "alter subscription $s set (slot_name = none)" -c "drop subscription $s" </dev/null
  sq ${d#wh_} ${d#wh_} -qAt -c "select count(pg_drop_replication_slot('$s'))" </dev/null > /dev/null
done
sq warehouse postgres -qc "drop database wh_shop" -c "drop database wh_crm" -c "drop database wh_billing" </dev/null
echo "--- layout (b) removed"

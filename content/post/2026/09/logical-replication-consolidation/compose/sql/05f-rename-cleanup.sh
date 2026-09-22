# @nosync
# Step 1l: clean up the rename experiment.
sq warehouse warehouse -qc "drop subscription sub_rename" -c "drop schema shopapp cascade" </dev/null
sq shop shop -qc "drop publication pub_rename" -c "drop schema shopapp cascade" -c "drop owned by app_shop" -c "drop role app_shop" </dev/null
echo "remaining slots on shop:"
qt shop shop "select count(*) from pg_replication_slots"

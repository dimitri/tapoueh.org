# @nosync
# Step 1j: the application, connecting with its own role and its own unqualified SQL, is unchanged.
echo "-- as app_shop:  show search_path;  select count(*) from orders;"
"${DC[@]}" exec -T shop psql -X -U app_shop -d shop -c "show search_path" -c "select count(*) from orders" </dev/null 2>&1

# @service: traffic2
# @nosync
# Step 52 (part D): subscribe new2 to old2, wait for the initial copy, then
# start the traffic loop against pgbouncer - and only pgbouncer, for the
# whole of part D. /tmp/target is set once and never touched again.
# Not sourcing lib.sh here: its PGOPTIONS (used by part A's freeze() to let
# the admin bypass read-only) breaks the pgbouncer connection below - unless
# a startup option is in pgbouncer's ignore_startup_parameters, pgbouncer
# rejects the connection outright instead of forwarding it.
export PGPASSWORD=postgres PGUSER=postgres
psql -X -h new2 -d app -c "
  CREATE SUBSCRIPTION app_sub2
         CONNECTION 'host=old2 dbname=app user=repl password=repl'
         PUBLICATION app_pub
         WITH (copy_data = true);"
for i in $(seq 1 300); do
  n=$(psql -X -qAt -h new2 -d app -c "SELECT count(*) FROM pg_subscription_rel WHERE srsubstate <> 'r'")
  [ "$n" = "0" ] && break
  sleep 0.1
done
echo "initial sync: $(psql -X -qAt -h new2 -d app -c "SELECT count(*) FROM pg_subscription_rel WHERE srsubstate <> 'r'") tables not yet ready"
psql -X -h new2 -d app -c "SELECT count(*) FROM customers" -c "SELECT count(*) FROM orders"

echo "### confirming traffic reaches the database through pgbouncer, not directly"
PGPASSWORD=app PGUSER=app psql -X -h pgbouncer -d app -c "SELECT current_setting('server_version_num') AS backend_version"

nohup /work/traffic.sh pgbouncer >/tmp/traffic-start.log 2>&1 &
sleep 2
echo "traffic loop started against pgbouncer, pid $!"

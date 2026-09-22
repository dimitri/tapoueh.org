# @service: traffic
# Step 13: schema-only dump/restore old -> new, roles first. The dump runs with
# the NEW major version's pg_dump (18) against the old server (16).
. /work/lib.sh
pg_dump --version; pg_dumpall --version

echo; echo "### roles: pg_dumpall --roles-only (old) piped into new, errors NOT hidden"
pg_dumpall -h old --roles-only | psql -X -q -h new -d postgres 2>&1 | sed 's/^psql:<stdin>:[0-9]*: //'

echo; echo "### roles now on new"
q new postgres "SELECT rolname, rolcanlogin, rolreplication, rolsuper FROM pg_roles WHERE rolname !~ '^pg_' ORDER BY 1"
q new postgres "SELECT r.rolname AS role, m.rolname AS member_of FROM pg_auth_members a JOIN pg_roles r ON r.oid = a.member JOIN pg_roles m ON m.oid = a.roleid WHERE r.rolname = 'repl'"

echo; echo "### database + schema-only restore"
q new postgres "CREATE DATABASE app OWNER app"
pg_dump -h old -d app --schema-only | psql -X -q -v ON_ERROR_STOP=1 -h new -d app 2>&1 | sed 's/^psql:<stdin>:[0-9]*: //'
echo "restore exit status: ${PIPESTATUS[1]}"

echo; echo "### what arrived"
q new app "SELECT c.relname, c.relkind, c.relreplident, c.relispopulated FROM pg_class c JOIN pg_namespace n ON n.oid = c.relnamespace WHERE n.nspname = 'public' AND c.relkind IN ('r','p','m','S') ORDER BY c.relkind, 1"
q new app "SELECT pubname, puballtables FROM pg_publication"
q new app "SELECT count(*) AS rows_in_orders FROM orders"
q new app "SELECT * FROM sales_by_day"
:

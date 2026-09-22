# @service: pg19a
# Step 45 (part C, 19 BETA): CREATE SUBSCRIPTION ... SERVER (connection through
# a foreign server + user mapping instead of a conninfo string).
. /work/lib.sh
A() { q pg19a "$1" "$2"; }; B() { q pg19b "$1" "$2"; }
qa pg19a postgres "CREATE DATABASE srv" >/dev/null; qa pg19b postgres "CREATE DATABASE srv" >/dev/null
qa pg19a srv "CREATE TABLE t (id int PRIMARY KEY); INSERT INTO t VALUES (1), (2); CREATE PUBLICATION p FOR ALL TABLES" >/dev/null
qa pg19b srv "CREATE TABLE t (id int PRIMARY KEY)" >/dev/null
B srv "CREATE SUBSCRIPTION s_bad SERVER nosuch PUBLICATION p"
B srv "SELECT fdwname, fdwhandler::regproc, fdwconnection::regproc FROM pg_foreign_data_wrapper"
B srv "CREATE EXTENSION postgres_fdw"
B srv "SELECT fdwname, fdwconnection::regproc FROM pg_foreign_data_wrapper"
B srv "CREATE SERVER pub_srv FOREIGN DATA WRAPPER postgres_fdw OPTIONS (host 'pg19a', port '5432', dbname 'srv')"
B srv "CREATE USER MAPPING FOR postgres SERVER pub_srv OPTIONS (user 'postgres', password 'postgres')"
B srv "CREATE SUBSCRIPTION s_srv SERVER pub_srv PUBLICATION p"
for i in $(seq 1 60); do [ "$(qa pg19b srv "SELECT count(*) FROM pg_subscription_rel WHERE srsubstate <> 'r'")" = 0 ] && break; sleep 0.5; done
B srv "SELECT * FROM t ORDER BY 1"
B srv "SELECT subname, subserver::regclass IS NULL AS x FROM pg_subscription" >/dev/null 2>&1
B srv "SELECT s.subname, f.srvname AS server, s.subconninfo IS NULL AS no_conninfo FROM pg_subscription s JOIN pg_foreign_server f ON f.oid = s.subserver"
echo; echo "### the connection parameters now live in the SERVER: changing them redirects the subscription"
B srv "ALTER SERVER pub_srv OPTIONS (SET host 'nonexistent-host')"
B srv "ALTER SUBSCRIPTION s_srv DISABLE"
B srv "ALTER SUBSCRIPTION s_srv ENABLE"
sleep 6
B srv "SELECT pid IS NOT NULL AS apply_running FROM pg_stat_subscription WHERE subname = 's_srv'"
B srv "ALTER SERVER pub_srv OPTIONS (SET host 'pg19a')"
for i in $(seq 1 40); do [ "$(qa pg19b srv "SELECT count(*) FROM pg_stat_subscription WHERE subname = 's_srv' AND pid IS NOT NULL")" = 1 ] && break; sleep 0.5; done
B srv "SELECT pid IS NOT NULL AS apply_running FROM pg_stat_subscription WHERE subname = 's_srv'"
:

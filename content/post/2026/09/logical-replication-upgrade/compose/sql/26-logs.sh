# @service: host
# Step 26: the notable server-side log lines of the whole run (part A):
# errors, warnings and their CONTEXT, with pids/xids/LSNs masked, counted.
cd "$(dirname "$0")/.." && export COMPOSE_PROFILES=a
for s in old new; do
  echo "### $s"
  docker compose logs --no-log-prefix "$s" 2>&1 | grep -E 'ERROR|FATAL|WARNING|CONTEXT:  processing remote|exited with exit code' |
    grep -vE 'STATEMENT|already exists|has not been populated|is not a PostgreSQL backend|message type "COMMIT"|terminating connection due to administrator command' |
    sed -E 's/^\[[0-9]+\] //; s/in transaction [0-9]+, finished at [0-9A-F]+\/[0-9A-F]+/in transaction N, finished at LSN/; s/pg_[0-9]+/pg_OID/; s/PID [0-9]+/PID N/' | sort | uniq -c
done

# @service: host
# Step 47: the notable part C log lines
cd "$(dirname "$0")/.." && export COMPOSE_PROFILES=c
echo "### pg19b (subscriber)"
docker compose logs --no-log-prefix pg19b 2>&1 | grep -E 'conflict detected|Could not find|was deleted locally|sequence synchronization|retain|will stop' | sed -E 's/^\[[0-9]+\] //; s/ at [0-9-]+ [0-9:.+]+ *$//; s/in transaction [0-9]+/in transaction N/; s/finished at [0-9A-F]+\/[0-9A-F]+/finished at LSN/; s/pg_[0-9]+/pg_OID/' | sort | uniq -c | sort -rn
echo "### pg19c (wal_level = replica)"
docker compose logs --no-log-prefix pg19c 2>&1 | grep -E 'logical decoding' | sed -E 's/^\[[0-9]+\] //' | uniq -c

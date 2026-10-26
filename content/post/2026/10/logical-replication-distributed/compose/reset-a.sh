#!/usr/bin/env bash
# Full teardown of profile a: containers and their data volumes.
set -uo pipefail
cd "$(dirname "$0")"
docker compose -p lrdist rm -sfv monitor pg-a-primary pg-a-standby a-client >/dev/null 2>&1
docker volume rm lrdist_monitor-data lrdist_pg-a-primary-data lrdist_pg-a-standby-data >/dev/null 2>&1
echo "reset a done"

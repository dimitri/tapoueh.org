# @service: host
# Step 15: what the new server logged for the privilege experiments of step 14
cd "$(dirname "$0")/.." && export COMPOSE_PROFILES=a
docker compose logs --no-log-prefix new | grep -E 'scratch|cannot SET ROLE|password is required' | grep -v '^\[[0-9]*\] STATEMENT'

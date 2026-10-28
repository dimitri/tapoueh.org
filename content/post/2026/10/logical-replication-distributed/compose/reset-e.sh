#!/usr/bin/env bash
# Profile e keeps everything on tmpfs: recreating the containers is the reset.
set -uo pipefail
cd "$(dirname "$0")"
docker compose -p lrdist rm -sf e-hub e-w1 e-w2 e-s1 >/dev/null 2>&1
COMPOSE_PROFILES=e docker compose -p lrdist up -d --wait e-hub e-w1 e-w2

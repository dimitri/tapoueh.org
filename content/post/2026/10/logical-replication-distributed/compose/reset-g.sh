#!/usr/bin/env bash
# Teardown of profile g: both groups, the client and the pgcopydb container.
cd "$(dirname "$0")"
. ./lib-regions.sh
region_reset us-east us-west
dc rm -sf client pgcopydb-g pgcopydb-gf >/dev/null 2>&1
echo "reset g done"

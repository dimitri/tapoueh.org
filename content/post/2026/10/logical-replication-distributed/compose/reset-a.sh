#!/usr/bin/env bash
# Teardown of the us-east group (profiles a and f's first region). Nodes keep their data on tmpfs.
cd "$(dirname "$0")"
. ./lib-regions.sh
region_reset us-east
dc rm -sf client >/dev/null 2>&1
echo "reset a done"

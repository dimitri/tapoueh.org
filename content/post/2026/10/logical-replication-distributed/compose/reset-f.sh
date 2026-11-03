#!/usr/bin/env bash
# Teardown of profile f: both groups and the client.
cd "$(dirname "$0")"
. ./lib-regions.sh
region_reset us-east us-west
dc rm -sf client >/dev/null 2>&1
echo "reset f done"

#!/bin/sh
# allow replication connections (pg_basebackup) from the compose network
echo "host replication all all scram-sha-256" >> "$PGDATA/pg_hba.conf"

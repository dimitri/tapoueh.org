#!/bin/bash
# Same contract as the official image for what the lab needs: initdb on an empty $PGDATA, trust auth.
set -e
mkdir -p "$PGDATA"; chown postgres:postgres "$PGDATA"; chmod 700 "$PGDATA"
if [ ! -s "$PGDATA/PG_VERSION" ]; then
  gosu postgres initdb -U postgres --auth=trust >/dev/null
  echo "listen_addresses = '*'" >> "$PGDATA/postgresql.conf"
  echo "host all all all trust" >> "$PGDATA/pg_hba.conf"
fi
if [ "$1" = postgres ]; then exec gosu postgres "$@"; fi
exec "$@"

#!/bin/bash
set -e
mkdir -p "$PGDATA"; chown postgres:postgres "$PGDATA"; chmod 700 "$PGDATA"
if [ ! -s "$PGDATA/PG_VERSION" ]; then
  gosu postgres initdb -U postgres --auth=trust >/dev/null
  echo "listen_addresses = '*'" >> "$PGDATA/postgresql.conf"
  echo "host all all all trust" >> "$PGDATA/pg_hba.conf"
fi
echo "+0s" > /tmp/ft; chmod 666 /tmp/ft
if [ "$1" = postgres ]; then
  export LD_PRELOAD=$(ls /usr/lib/*/faketime/libfaketime.so.1) FAKETIME_TIMESTAMP_FILE=/tmp/ft FAKETIME_NO_CACHE=1
  exec gosu postgres "$@"
fi
exec "$@"

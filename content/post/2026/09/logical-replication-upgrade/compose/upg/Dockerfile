# Helper image for part B: PostgreSQL 18 (official image) + the PG 17 server
# binaries from the PGDG apt repository that the official image already
# configures. Both major versions are needed side by side by pg_upgrade.
FROM postgres:18
RUN apt-get update \
 && apt-get install -y --no-install-recommends postgresql-17 \
 && rm -rf /var/lib/apt/lists/*
# a volume mounted here inherits this ownership when it is first created
RUN mkdir /pgdata && chown postgres:postgres /pgdata

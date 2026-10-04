FROM postgres:14
# The official Debian image already has the PGDG apt repository configured.
RUN apt-get update \
 && apt-get install -y --no-install-recommends postgresql-14-pglogical \
 && rm -rf /var/lib/apt/lists/*

# PostgreSQL master of 2026-05-14 with the "Sequence Access Methods, round two" patches (v29),
# which include contrib/snowflake. Built without assertions so that timings can be compared.
FROM debian:trixie-slim
RUN apt-get update && apt-get install -y --no-install-recommends \
      build-essential meson ninja-build bison flex pkg-config git curl ca-certificates gosu perl \
      libreadline-dev zlib1g-dev libicu-dev libssl-dev libxml2-dev python3 \
    && rm -rf /var/lib/apt/lists/*
ARG PG_COMMIT=27bdae84137f35af42ca5ae4f0507dfb8c69db60
ARG PATCHES=https://www.postgresql.org/message-id/attachment
WORKDIR /src
RUN git clone --shallow-since=2026-05-01 --single-branch https://github.com/postgres/postgres.git pg \
    && cd pg && git checkout -q $PG_COMMIT && git config user.email t@example.org && git config user.name t
COPY compose/seqam/patches.txt /src/patches.txt
RUN cd pg && while read -r p; do curl -sfL "$PATCHES/$p" -o "/src/$(basename $p)" && git am --3way "/src/$(basename $p)"; done < /src/patches.txt
RUN cd pg && meson setup build --prefix=/usr/local/pgsql --buildtype=release -Dcassert=false \
    && ninja -C build && ninja -C build install
# extension/: our demo sequence access method, built the way it would be shipped, out of tree
# with PGXS against the installed server.  Build from the article's directory:
#   docker build -t pk-seqam -f compose/seqam/Dockerfile .
COPY extension /src/regionseq
RUN make -C /src/regionseq PG_CONFIG=/usr/local/pgsql/bin/pg_config && make -C /src/regionseq PG_CONFIG=/usr/local/pgsql/bin/pg_config install
RUN useradd -m postgres && chown -R postgres /src
ENV PATH=/usr/local/pgsql/bin:$PATH PGDATA=/var/lib/postgresql/data
COPY compose/seqam/entrypoint.sh /entrypoint.sh
ENTRYPOINT ["/entrypoint.sh"]
CMD ["postgres"]

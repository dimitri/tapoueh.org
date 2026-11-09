# Shared helpers: one throwaway Postgres 19rc1 container per scenario.
IMAGE=${IMAGE:-postgres:19rc1}

pg_start() {   # name [extra postgres args...]
    local name=$1; shift
    docker rm -f "$name" >/dev/null 2>&1
    docker run -d --name "$name" --network "${NET:-bridge}" \
        --tmpfs /var/lib/postgresql:rw,size=${TMPFS:-5g} \
        -e POSTGRES_PASSWORD=x -e POSTGRES_HOST_AUTH_METHOD=trust \
        "$IMAGE" postgres "$@" >/dev/null
    until docker exec "$name" pg_isready -q -U postgres 2>/dev/null; do sleep 1; done
    sleep 2
}

psqlx() { local name=$1; shift; docker exec -i "$name" psql -X -q -v ON_ERROR_STOP=1 -U postgres "$@"; }

# Key variants for the region mesh: sets DDL and EXPR for region index $2 (0-based) of variant $1.
# One sequence "s" per node.
variant() {
  local v=$1 i=$2
  PAYLOAD="'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx'"
  case $v in
    naive)     DDL="create sequence s; create table t (id bigint primary key, region smallint not null, payload text not null);"; EXPR="nextval('s')" ;;
    seqstep)   DDL="create sequence s increment 10 start $((i+1)); create table t (id bigint primary key, region smallint not null, payload text not null);"; EXPR="nextval('s')" ;;
    composite) DDL="create sequence s; create table t (region smallint not null, id bigint not null, payload text not null, primary key (region, id));"; EXPR="nextval('s')" ;;
    uuidv4)    DDL="create table t (id uuid primary key, region smallint not null, payload text not null);"; EXPR="gen_random_uuid()" ;;
    uuidv7)    DDL="create table t (id uuid primary key, region smallint not null, payload text not null);"; EXPR="uuidv7()" ;;
    snowflake) DDL="create sequence s; create table t (id bigint primary key, region smallint not null, payload text not null);"
               EXPR="(((extract(epoch from clock_timestamp()) * 1000)::bigint - 1767225600000) << 22) | ($i::bigint << 18) | (nextval('s') & 262143)" ;;
    *) echo "unknown variant $v"; return 1 ;;
  esac
}

# helpers for part B, run as the postgres user inside the "upg" container
B17=/usr/lib/postgresql/17/bin
B18=/usr/lib/postgresql/18/bin
export PGHOST=/tmp
P17=5441 S17=5442 P18=5441 S18=5442      # a node keeps its port across the upgrade
# psql against a node by port; -X, no psqlrc
pq() { local port=$1 db=$2; shift 2; psql -X -p "$port" -d "$db" "$@"; }
# q PORT "SQL": print then run
qq() { echo "[$1]> $2"; psql -X -p "$1" -d app -c "$2" 2>&1; }
qa() { psql -X -qAt -p "$1" -d app -c "$2"; }

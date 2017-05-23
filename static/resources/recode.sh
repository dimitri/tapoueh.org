#! /bin/bash

TMPDIR=/var/backups/migration
mkdir -p $TMPDIR/recode

function usage() {
    prog=`basename $0`
    echo "$prog [-npdf0TI] [-U user ] -s schema [-m mintable] pattern"
    echo "        -d    debug"
    echo "        -n    dry run, only print table names and expected files"
    echo "        -p    connect to bdd.allopass (production)"
    echo "        -s    schema"
    echo "        -m    mintable, to skip already processed once"
    echo "        -U    connect to PostgreSQL as user"
    echo "        -f    force table loading even when export files do exist"
    echo "        -0    only (re)load tables with zero-sized copy files"
    echo "        -T    Truncate the tables before COPYing recoded data"
    echo "        -I    Temporarily drop the indexes of the table while COPYing"
    echo "   pattern    ^board_log_, e.g."
    exit 1
}

DEBUG=0
DRYRUN=0
PROD=0
ZERO=0
FORCE=0
TRUNCATE=0
INDEXES=0
PGUSER="webadmin"

while getopts "hs:m:U:ndpf0TI" optionName; do
    case "$optionName" in
	h) usage;;
	s) schema="$OPTARG";;
	m) mintable="$OPTARG";;
	U) PGUSER="$OPTARG";;
	n) DRYRUN=1;;
	p) PROD=1;;
	d) DEBUG=1;;
	T) TRUNCATE=1;;
	0) ZERO=1;;
	f) FORCE=1;;
	I) INDEXES=1;;
	[?]) usage;;
    esac
done
shift $(($OPTIND - 1)) 

tablepattern="$1"

test -z "$schema" && usage
test -z "$tablepattern" && usage

s_dsn="--cluster 8.3/main -U $PGUSER allopass_archives"
d_dsn="--cluster 8.4/main -U $PGUSER allopass_db"

if [ $PROD -eq 1 ]
then
    s_dsn="-h bdd -p 5229 -U $PGUSER allopass_db"
fi

if [ $DEBUG -eq 1 ]; then
    echo "source dsn: $s_dsn"
    echo "target dsn: $d_dsn"
fi

table_list_sql="select '$schema.' || tablename"
table_list_sql="$table_list_sql from pg_tables "
table_list_sql="$table_list_sql where schemaname = '$schema' "
table_list_sql="$table_list_sql and tablename ~ '$tablepattern' "
if [ -n "$mintable" ]; then
    table_list_sql="$table_list_sql and tablename >= '$mintable' "
fi
table_list_sql="$table_list_sql order by tablename"

if [ $DEBUG -eq 1 ]; then
    echo $table_list_sql
fi

for t in `psql $s_dsn -At -c "$table_list_sql"`
do
    echo $t

    csv=$TMPDIR/$t.csv
    utf=$TMPDIR/recode/$t.utf8.csv
    sql="\copy $t to $csv"

    if [ $FORCE -eq 1 ]; then
	if [ $DRYRUN -eq 1 ]; then
	    test $DEBUG -eq 1 && echo "#rm -f $csv $utf"
	else
	    test $DEBUG -eq 1 && echo "rm -f $csv $utf"
	    rm -f $csv $utf
	fi
    else
	if [ $ZERO -eq 1 ]; then
	    if [ -s $csv ]; then
		echo "skipping $t"
		continue
	    fi
	    # either file not exists or its size is zero
	    # if size is not zero, ignore it
	    test -f $csv && rm $csv || continue
	    rm -f $utf
	fi
    fi

    if [ $INDEXES -eq 1 ]; then
	sqli="SELECT pg_catalog.pg_get_indexdef(i.indexrelid, 0, true) || '; '"
	sqli="$sqli FROM pg_catalog.pg_class c, pg_catalog.pg_index i"
	sqli="$sqli WHERE c.oid = '${t}'::regclass "
	sqli="$sqli   AND c.oid = i.indrelid;"

	test $DEBUG -eq 1 && echo $sqli

	psql -At $d_dsn -c "$sqli" > $TMPDIR/$t.idx.sql
	drop='{printf "DROP INDEX %s;\n", $2 == "UNIQUE" ? $4 : $3}'
	(cd $TMPDIR && awk "${drop}" < $t.idx.sql > $t.idx.drop.sql)

	sql_drop_index="\i $TMPDIR/$t.idx.drop.sql"
    else
	sql_drop_index=""
    fi

    sql_truncate=""
    test $TRUNCATE -eq 1 && sql_truncate="truncate $t;"

    cat > $TMPDIR/$t.copy.sql <<EOF
BEGIN;
set datestyle to 'Postgres, MDY';
set search_path to ${schema}, pg_catalog;
$sql_truncate
$sql_drop_index
\copy $t from $utf
COMMIT;
EOF

    if [ $DRYRUN -eq 1 ]; then
	test $DEBUG -eq 1 && cat $TMPDIR/$t.copy.sql
	echo
	continue
    fi

    test -f $csv || (echo $csv; psql $s_dsn -c "$sql" > $csv)
    test -f $utf || (echo $utf; recode l1..utf8 < $csv > $utf)
    echo "copy $t from $utf"

    test $DEBUG -eq 1 && opts="-Aat" || opts="-At"

    psql $opts $d_dsn -f $TMPDIR/$t.copy.sql

    # create indexes in parallel
    if [ $INDEXES -eq 1 ]; then
	while read idx
	do
	    psql $opts $d_dsn -c "$idx" &
	done < $TMPDIR/$t.idx.sql
    fi

    psql $opts $d_dsn -c "select count(*), '$t' from $t;"
    wc -l $utf
    echo
done

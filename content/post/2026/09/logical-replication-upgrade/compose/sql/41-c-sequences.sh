# @service: pg19a
# Step 41 (part C, 19 BETA): sequence replication. pg19a publishes, pg19b subscribes.
. /work/lib.sh
A() { q pg19a "$1" "$2"; }; B() { q pg19b "$1" "$2"; }
qa pg19a postgres "CREATE DATABASE seq" >/dev/null; qa pg19b postgres "CREATE DATABASE seq" >/dev/null
DDL="CREATE TABLE t (id bigint GENERATED ALWAYS AS IDENTITY PRIMARY KEY, v text); CREATE SEQUENCE s1 START 100; CREATE TABLE other (id int PRIMARY KEY)"
qa pg19a seq "$DDL" >/dev/null; qa pg19b seq "$DDL" >/dev/null
echo "### what can be published: only ALL SEQUENCES (no per-sequence form in this build)"
A seq "CREATE PUBLICATION p_one FOR SEQUENCE s1"
A seq "CREATE PUBLICATION p_mixed FOR TABLE t, SEQUENCE s1"
A seq "CREATE PUBLICATION p_all FOR ALL TABLES, ALL SEQUENCES"
A seq "CREATE PUBLICATION p_seqonly FOR ALL SEQUENCES"
A seq "SELECT pubname, puballtables, puballsequences FROM pg_publication ORDER BY 1"
A seq "SELECT relid::regclass FROM pg_get_publication_sequences('p_all') ORDER BY 1"

echo; echo "### publisher side: advance the sequences, look at them"
A seq "SELECT nextval('s1') FROM generate_series(1, 5)" >/dev/null
A seq "INSERT INTO t (v) SELECT 'x' FROM generate_series(1, 20)" >/dev/null
A seq "SELECT sequencename, last_value FROM pg_sequences ORDER BY 1"
A seq "SELECT last_value, is_called, page_lsn IS NOT NULL AS has_page_lsn FROM pg_get_sequence_data('s1')"

echo; echo "### subscriber: the initial sync copies the sequences too"
B seq "CREATE SUBSCRIPTION sub_all CONNECTION 'host=pg19a dbname=seq user=postgres password=postgres' PUBLICATION p_all"
sleep 4
B seq "SELECT srrelid::regclass AS rel, c.relkind, srsubstate FROM pg_subscription_rel r JOIN pg_class c ON c.oid = r.srrelid ORDER BY 1"
B seq "SELECT sequencename, last_value FROM pg_sequences ORDER BY 1"

echo; echo "### the publisher moves on by 100 values; is the subscriber updated by itself? (waiting 20 s)"
A seq "SELECT nextval('s1') FROM generate_series(1, 100)" >/dev/null
sleep 20
B seq "SELECT sequencename, last_value FROM pg_sequences ORDER BY 1"
A seq "SELECT sequencename, last_value FROM pg_sequences ORDER BY 1"

echo; echo "### only REFRESH SEQUENCES brings them up to date"
B seq "ALTER SUBSCRIPTION sub_all REFRESH SEQUENCES"
sleep 3
B seq "SELECT sequencename, last_value FROM pg_sequences ORDER BY 1"
B seq "SELECT last_value, is_called FROM pg_get_sequence_data('s1')"
B seq "SELECT srrelid::regclass AS rel, srsubstate FROM pg_subscription_rel r JOIN pg_class c ON c.oid = r.srrelid WHERE c.relkind = 'S' ORDER BY 1"

echo; echo "### a sequence created later is NOT picked up until REFRESH PUBLICATION"
A seq "CREATE SEQUENCE later_seq"
B seq "CREATE SEQUENCE later_seq"
A seq "SELECT nextval('later_seq') FROM generate_series(1, 3)" >/dev/null
B seq "ALTER SUBSCRIPTION sub_all REFRESH SEQUENCES"
sleep 2
B seq "SELECT sequencename, last_value FROM pg_sequences WHERE sequencename = 'later_seq'"
B seq "ALTER SUBSCRIPTION sub_all REFRESH PUBLICATION"
B seq "ALTER SUBSCRIPTION sub_all REFRESH SEQUENCES"
sleep 3
B seq "SELECT sequencename, last_value FROM pg_sequences WHERE sequencename = 'later_seq'"
:

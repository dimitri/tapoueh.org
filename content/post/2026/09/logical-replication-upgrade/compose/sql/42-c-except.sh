# @service: pg19a
# Step 42 (part C, 19 BETA): CREATE PUBLICATION ... FOR ALL TABLES EXCEPT (TABLE ...)
. /work/lib.sh
A() { q pg19a "$1" "$2"; }; B() { q pg19b "$1" "$2"; }
qa pg19a postgres "CREATE DATABASE exc" >/dev/null; qa pg19b postgres "CREATE DATABASE exc" >/dev/null
DDL="CREATE TABLE keep (id int PRIMARY KEY); CREATE TABLE audit (id int PRIMARY KEY, x text); CREATE TABLE scratch_a (id int PRIMARY KEY);
     CREATE TABLE ev (id int, d date, PRIMARY KEY (id, d)) PARTITION BY RANGE (d);
     CREATE TABLE ev_2026 PARTITION OF ev FOR VALUES FROM ('2026-01-01') TO ('2027-01-01')"
qa pg19a exc "$DDL" >/dev/null; qa pg19b exc "$DDL" >/dev/null
A exc "CREATE PUBLICATION p_ex FOR ALL TABLES EXCEPT (TABLE audit, scratch_a)"
A exc "SELECT tablename FROM pg_publication_tables WHERE pubname = 'p_ex' ORDER BY 1"
A exc "\\dRp+ p_ex"
A exc "SELECT prrelid::regclass, prexcept FROM pg_publication_rel"
echo "### data flow: excluded tables stay empty on the subscriber"
A exc "INSERT INTO keep VALUES (1); INSERT INTO audit VALUES (1, 'not replicated'); INSERT INTO scratch_a VALUES (1); INSERT INTO ev VALUES (1, '2026-06-01')"
B exc "CREATE SUBSCRIPTION s_ex CONNECTION 'host=pg19a dbname=exc user=postgres password=postgres' PUBLICATION p_ex"
sleep 4
for t in keep audit scratch_a ev; do B exc "SELECT '$t' AS tbl, count(*) FROM $t"; done
echo; echo "### a table created later on the publisher is published (it is not in the EXCEPT list)"
A exc "CREATE TABLE created_later (id int PRIMARY KEY)"
A exc "SELECT tablename FROM pg_publication_tables WHERE pubname = 'p_ex' ORDER BY 1"
echo; echo "### changing the list, and the forms that are refused"
A exc "ALTER PUBLICATION p_ex SET ALL TABLES EXCEPT (TABLE scratch_a)"
A exc "SELECT tablename FROM pg_publication_tables WHERE pubname = 'p_ex' ORDER BY 1"
A exc "ALTER PUBLICATION p_ex ADD TABLE audit"
A exc "CREATE PUBLICATION p_bad FOR TABLE keep EXCEPT (TABLE audit)"
A exc "CREATE PUBLICATION p_part FOR ALL TABLES EXCEPT (TABLE ev)"
A exc "SELECT tablename FROM pg_publication_tables WHERE pubname = 'p_part' ORDER BY 1"
A exc "CREATE PUBLICATION p_part_root FOR ALL TABLES EXCEPT (TABLE ev) WITH (publish_via_partition_root = true)"
A exc "SELECT tablename FROM pg_publication_tables WHERE pubname = 'p_part_root' ORDER BY 1"
:

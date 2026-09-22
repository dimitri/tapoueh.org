-- @service: hub
-- Step 5b. The one case that needs origin = none: the SAME table replicated both ways.
-- Two-node test, hub <-> worker1, driven from the hub (worker1 side through dblink).
-- Three rounds: origin = any (default) without and with a primary key, then origin = none.
CREATE TABLE settings_nopk (v text);
CREATE TABLE settings_pk (k text PRIMARY KEY, v text);
CREATE PUBLICATION pub_set FOR TABLE settings_nopk, settings_pk;
SELECT lr.on('worker1', 'CREATE TABLE settings_nopk (v text)');
SELECT lr.on('worker1', 'CREATE TABLE settings_pk (k text PRIMARY KEY, v text)');
SELECT lr.on('worker1', 'CREATE PUBLICATION pub_set FOR TABLE settings_nopk, settings_pk');

-- Round 1: origin = any, table without primary key. Insert ONE row on the hub.
CREATE SUBSCRIPTION sub_set_from_w1 CONNECTION 'host=worker1 dbname=app user=postgres'
  PUBLICATION pub_set WITH (copy_data = false);
SELECT lr.on('worker1', $$CREATE SUBSCRIPTION sub_set_from_hub CONNECTION 'host=hub dbname=app user=postgres'
  PUBLICATION pub_set WITH (copy_data = false)$$);
SELECT subname, suborigin FROM pg_subscription WHERE subname = 'sub_set_from_w1';
INSERT INTO settings_nopk VALUES ('one row, inserted once');
SELECT lr.wait_for($$SELECT count(*) >= 10 FROM settings_nopk$$);
-- the row is bouncing between the two nodes forever: stop it
ALTER SUBSCRIPTION sub_set_from_w1 DISABLE;
SELECT lr.on('worker1', 'ALTER SUBSCRIPTION sub_set_from_hub DISABLE');
SELECT count(*) > 1 AS ping_pong_multiplied_the_row FROM settings_nopk;
SELECT lr.wait_for($$SELECT NOT active FROM dblink(lr.conn('worker1'), $q$SELECT active FROM pg_replication_slots
  WHERE slot_name = 'sub_set_from_w1'$q$) t(active bool)$$);
SELECT lr.wait_for($$SELECT NOT active FROM pg_replication_slots WHERE slot_name = 'sub_set_from_hub'$$);
DROP SUBSCRIPTION sub_set_from_w1;
SELECT lr.on('worker1', 'DROP SUBSCRIPTION sub_set_from_hub');
TRUNCATE settings_nopk;
SELECT lr.on('worker1', 'TRUNCATE settings_nopk');

-- Round 2: origin = any, table WITH a primary key: the boomerang row collides with itself.
CREATE SUBSCRIPTION sub_set_from_w1 CONNECTION 'host=worker1 dbname=app user=postgres'
  PUBLICATION pub_set WITH (copy_data = false);
SELECT lr.on('worker1', $$CREATE SUBSCRIPTION sub_set_from_hub CONNECTION 'host=hub dbname=app user=postgres'
  PUBLICATION pub_set WITH (copy_data = false)$$);
INSERT INTO settings_pk VALUES ('currency', 'EUR');
SELECT lr.wait_for($$SELECT apply_error_count >= 1 FROM pg_stat_subscription_stats WHERE subname = 'sub_set_from_w1'$$);
SELECT subname, apply_error_count >= 1 AS erroring, confl_insert_exists >= 1 AS insert_exists
FROM pg_stat_subscription_stats WHERE subname = 'sub_set_from_w1';
SELECT DISTINCT lr.norm(l) AS log_line FROM lr.log_lines('settings_pk') l ORDER BY 1;
ALTER SUBSCRIPTION sub_set_from_w1 DISABLE;
SELECT lr.on('worker1', 'ALTER SUBSCRIPTION sub_set_from_hub DISABLE');
SELECT lr.wait_for($$SELECT NOT active FROM dblink(lr.conn('worker1'), $q$SELECT active FROM pg_replication_slots
  WHERE slot_name = 'sub_set_from_w1'$q$) t(active bool)$$);
SELECT lr.wait_for($$SELECT NOT active FROM pg_replication_slots WHERE slot_name = 'sub_set_from_hub'$$);
DROP SUBSCRIPTION sub_set_from_w1;
SELECT lr.on('worker1', 'DROP SUBSCRIPTION sub_set_from_hub');
TRUNCATE settings_pk, settings_nopk;
SELECT lr.on('worker1', 'TRUNCATE settings_pk, settings_nopk');

-- Round 3: origin = none. Only changes that originated locally are sent.
CREATE SUBSCRIPTION sub_set_from_w1 CONNECTION 'host=worker1 dbname=app user=postgres'
  PUBLICATION pub_set WITH (copy_data = false, origin = none);
SELECT lr.on('worker1', $$CREATE SUBSCRIPTION sub_set_from_hub CONNECTION 'host=hub dbname=app user=postgres'
  PUBLICATION pub_set WITH (copy_data = false, origin = none)$$);
SELECT subname, suborigin FROM pg_subscription WHERE subname = 'sub_set_from_w1';
INSERT INTO settings_nopk VALUES ('from hub');
INSERT INTO settings_pk VALUES ('timezone', 'UTC');
SELECT lr.on('worker1', $$INSERT INTO settings_nopk VALUES ('from worker1')$$);
SELECT lr.on('worker1', $$INSERT INTO settings_pk VALUES ('locale', 'fr_FR')$$);
SELECT lr.wait_for($$SELECT (SELECT count(*) FROM settings_nopk) = 2 AND (SELECT count(*) FROM settings_pk) = 2$$);
SELECT lr.wait_caught_up();
SELECT lr.wait_for($$SELECT c = 2 FROM dblink(lr.conn('worker1'), 'SELECT count(*) FROM settings_nopk') t(c bigint)$$);
SELECT 'hub' AS node, (SELECT count(*) FROM settings_nopk) AS nopk_rows, (SELECT count(*) FROM settings_pk) AS pk_rows
UNION ALL
SELECT 'worker1', a, b FROM dblink(lr.conn('worker1'), $$SELECT (SELECT count(*) FROM settings_nopk), (SELECT count(*) FROM settings_pk)$$) t(a bigint, b bigint);
SELECT subname, apply_error_count FROM pg_stat_subscription_stats WHERE subname = 'sub_set_from_w1';

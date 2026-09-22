-- @service: hub
-- Step 8. Reading pg_stat_subscription_stats (PG18): one table, one subscription, every conflict kind
-- provoked on purpose, then how to read the counters and what to do about each one.

-- A small table replicated from worker1, with a secondary unique column so that we can also
-- provoke update_exists. Set up, subscribe, wait for the initial sync.
SELECT lr.on('worker1', $$CREATE TABLE conf_demo (id int PRIMARY KEY, code text UNIQUE, note text)$$);
SELECT lr.on('worker1', 'CREATE PUBLICATION pub_conf FOR TABLE conf_demo');
CREATE TABLE conf_demo (id int PRIMARY KEY, code text UNIQUE, note text);
CREATE SUBSCRIPTION sub_conf_w1
  CONNECTION 'host=worker1 dbname=app user=postgres' PUBLICATION pub_conf;
SELECT lr.wait_for($$SELECT count(*) = 1 FROM pg_subscription_rel r JOIN pg_subscription s ON s.oid = r.srsubid
                    WHERE s.subname = 'sub_conf_w1' AND r.srsubstate = 'r'$$);

-- Start from zero: the counters are cumulative, so reset them before an experiment and read the delta.
SELECT pg_stat_reset_subscription_stats(oid) IS NOT NULL AS reset FROM pg_subscription WHERE subname = 'sub_conf_w1';

-- The whole view, one row per subscription. Everything is zero.
\x on
SELECT subname, apply_error_count, sync_error_count, confl_insert_exists, confl_update_origin_differs,
       confl_update_exists, confl_update_missing, confl_delete_origin_differs, confl_delete_missing,
       confl_multiple_unique_conflicts
FROM pg_stat_subscription_stats WHERE subname = 'sub_conf_w1';
\x off

-- Five rows written on the worker, replicated to the hub.
SELECT lr.on('worker1', $$INSERT INTO conf_demo SELECT g, 'code-' || g, 'from worker' FROM generate_series(1, 5) g$$);
SELECT lr.wait_for($$SELECT count(*) = 5 FROM conf_demo$$);

-- 1. update_missing: the hub deleted row 1, the worker updates it. Not an error, the change is dropped.
DELETE FROM conf_demo WHERE id = 1;
SELECT lr.on('worker1', $$UPDATE conf_demo SET note = 'updated' WHERE id = 1$$);
-- 2. delete_missing: the hub deleted row 2, the worker deletes it too. Not an error.
DELETE FROM conf_demo WHERE id = 2;
SELECT lr.on('worker1', $$DELETE FROM conf_demo WHERE id = 2$$);
-- 3. update_origin_differs: the hub modified row 3 itself, then the worker updates it. Not an error,
--    the remote change is applied over the local one.
UPDATE conf_demo SET note = 'edited on the hub' WHERE id = 3;
SELECT lr.on('worker1', $$UPDATE conf_demo SET note = 'edited on the worker' WHERE id = 3$$);
-- 4. delete_origin_differs: the hub modified row 4, the worker deletes it. Not an error, the delete is applied.
UPDATE conf_demo SET note = 'edited on the hub' WHERE id = 4;
SELECT lr.on('worker1', $$DELETE FROM conf_demo WHERE id = 4$$);
SELECT lr.wait_for($$SELECT confl_update_missing = 1 AND confl_delete_missing = 1
                            AND confl_update_origin_differs = 1 AND confl_delete_origin_differs = 1
                     FROM pg_stat_subscription_stats WHERE subname = 'sub_conf_w1'$$);

\x on
SELECT apply_error_count, confl_update_missing, confl_delete_missing,
       confl_update_origin_differs, confl_delete_origin_differs
FROM pg_stat_subscription_stats WHERE subname = 'sub_conf_w1';
\x off
-- The apply worker never stopped, and the data says who won:
SELECT id, note FROM conf_demo ORDER BY id;

-- 5. insert_exists: the hub inserted row 6 itself, the worker inserts a row 6 too, on the primary key
--    only (the codes differ). This one stops the apply worker.
INSERT INTO conf_demo VALUES (6, 'hub-6', 'inserted on the hub');
SELECT lr.on('worker1', $$INSERT INTO conf_demo VALUES (6, 'worker-6', 'inserted on the worker')$$);
SELECT lr.wait_for($$SELECT apply_error_count >= 2 AND confl_insert_exists >= 2
                     FROM pg_stat_subscription_stats WHERE subname = 'sub_conf_w1'$$);
-- The counters keep growing at every retry, until somebody fixes the data or skips the transaction.
SELECT subname, apply_error_count >= 2 AS retrying, confl_insert_exists >= 2 AS insert_exists_repeats
FROM pg_stat_subscription_stats WHERE subname = 'sub_conf_w1';
-- The log names the relation, the key and the two rows:
WITH x AS (
  SELECT lr.norm(l) AS line, row_number() OVER () AS n
  FROM lr.log_lines('conflict=insert_exists|DETAIL:.*Key already exists|^[[:space:]]+Key|CONTEXT.*conf_demo.*INSERT') l WHERE l ~ 'conf_demo|hub-[0-9]|worker-[0-9]|code-[0-9]')
SELECT line FROM (SELECT line, min(n) AS mn FROM x GROUP BY line) s ORDER BY mn;
-- Fix: the hub's row is the one to give up. The next retry succeeds, and the counters stop moving.
DELETE FROM conf_demo WHERE id = 6;
SELECT lr.wait_for($$SELECT note = 'inserted on the worker' FROM conf_demo WHERE id = 6$$);

-- 6. multiple_unique_conflicts: the incoming row collides on the primary key AND on the unique code.
--    Stops the apply worker as well, and it is counted under its own name, not as insert_exists.
INSERT INTO conf_demo VALUES (7, 'code-7', 'inserted on the hub');
SELECT lr.on('worker1', $$INSERT INTO conf_demo VALUES (7, 'code-7', 'inserted on the worker')$$);
SELECT lr.wait_for($$SELECT confl_multiple_unique_conflicts >= 2 FROM pg_stat_subscription_stats WHERE subname = 'sub_conf_w1'$$);
WITH x AS (
  SELECT lr.norm(l) AS line, row_number() OVER () AS n
  FROM lr.log_lines('conflict=multiple_unique_conflicts|DETAIL:.*Key already exists|^[[:space:]]+Key|CONTEXT.*conf_demo.*INSERT') l WHERE l ~ 'conf_demo|hub-[0-9]|worker-[0-9]|code-[0-9]')
SELECT line FROM (SELECT line, min(n) AS mn FROM x WHERE line NOT LIKE '%worker-6%' AND line NOT LIKE '%hub-6%' GROUP BY line) s ORDER BY mn;
DELETE FROM conf_demo WHERE id = 7;
SELECT lr.wait_for($$SELECT note = 'inserted on the worker' FROM conf_demo WHERE id = 7$$);

-- 7. update_exists: the worker changes row 5's code to a value the hub already has on another row.
--    Stops the apply worker as well.
INSERT INTO conf_demo VALUES (8, 'code-8', 'inserted on the hub');
SELECT lr.on('worker1', $$UPDATE conf_demo SET code = 'code-8' WHERE id = 5$$);
SELECT lr.wait_for($$SELECT confl_update_exists >= 2 FROM pg_stat_subscription_stats WHERE subname = 'sub_conf_w1'$$);
WITH x AS (
  SELECT lr.norm(l) AS line, row_number() OVER () AS n
  FROM lr.log_lines('conflict=update_exists|DETAIL:.*Key|^[[:space:]]+Key|CONTEXT.*conf_demo.*UPDATE') l WHERE l ~ 'conf_demo|hub-[0-9]|worker-[0-9]|code-[0-9]')
SELECT line FROM (SELECT line, min(n) AS mn FROM x WHERE line LIKE '%code-8%' OR line LIKE '%update_exists%' OR line LIKE '%conf_demo_code_key%' OR line LIKE '%UPDATE%' GROUP BY line) s ORDER BY mn;
DELETE FROM conf_demo WHERE id = 8;
SELECT lr.wait_for($$SELECT code = 'code-8' FROM conf_demo WHERE id = 5$$);

-- The final picture: which kinds happened, and which of them stop replication.
-- The blocking kinds raised an ERROR (apply_error_count moved), the others were logged and passed.
SELECT c.kind, c.stops_apply, c.what_it_means
FROM pg_stat_subscription_stats s
CROSS JOIN LATERAL (VALUES
  ('insert_exists',            s.confl_insert_exists,            true,  'a row with this key exists locally: fix or delete one side, or SKIP the transaction'),
  ('update_exists',            s.confl_update_exists,            true,  'the new value violates a unique index on the subscriber: fix the local row that holds it'),
  ('multiple_unique_conflicts', s.confl_multiple_unique_conflicts, true, 'the incoming row violates more than one unique index: fix the local row(s) holding those keys'),
  ('update_missing',           s.confl_update_missing,           false, 'the row to update is not here: the change was dropped, the data diverged'),
  ('delete_missing',           s.confl_delete_missing,           false, 'the row to delete is not here: harmless if it was deleted on purpose'),
  ('update_origin_differs',    s.confl_update_origin_differs,    false, 'the row was changed locally: the remote change won'),
  ('delete_origin_differs',    s.confl_delete_origin_differs,    false, 'the row was changed locally: the delete was applied')
) AS c(kind, n, stops_apply, what_it_means)
WHERE s.subname = 'sub_conf_w1' AND c.n > 0
ORDER BY c.stops_apply DESC, c.kind;
-- and the subscription is healthy again: no worker error is left pending
SELECT lr.wait_caught_up();
SELECT pid IS NOT NULL AS apply_worker_running FROM pg_stat_subscription WHERE subname = 'sub_conf_w1' AND worker_type = 'apply';

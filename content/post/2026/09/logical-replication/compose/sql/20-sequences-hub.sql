-- @service: hub
-- Step 6. Sequences are not replicated (PG18): only table rows travel.
CREATE SEQUENCE invoice_no;
SELECT lr.on('worker1', 'CREATE SEQUENCE invoice_no');
-- The publication grammar has no sequence support in PG18:
CREATE PUBLICATION pub_seq FOR ALL SEQUENCES;
CREATE PUBLICATION pub_seq FOR TABLE invoice_no;

SELECT nextval('invoice_no') FROM generate_series(1, 5);
SELECT last_value FROM invoice_no;
-- on worker1 the sequence with the same name is untouched, however busy the hub is
SELECT lr.wait_caught_up();
SELECT v AS worker1_nextval FROM dblink(lr.conn('worker1'), $$SELECT nextval('invoice_no')$$) t(v bigint);

-- The same happens to identity columns of replicated tables. worker1 holds customer rows
-- copied from the hub with their ids, but its own identity sequence never moved:
SELECT sequencename, last_value FROM dblink(lr.conn('worker1'),
  $$SELECT sequencename::text, last_value FROM pg_sequences WHERE sequencename LIKE 'customers%'$$)
  AS t(sequencename text, last_value bigint);
-- so a row minted locally on a worker (e.g. after promoting it, or a stray local INSERT)
-- takes the first id, which the replicated data already uses:
SELECT lr.on('worker1', $$INSERT INTO customers (name, worker_id, plan_id) VALUES ('minted-on-worker', 1, 1)$$);

-- Consequence for ids minted on workers: they must never share a key space with hub ids.
-- That is why usage events use (worker_id, event_id) and the hub never mints event ids.
-- Manual workaround when a sequence value must travel: setval() from your own tooling.
SELECT v AS worker1_after_setval FROM dblink(lr.conn('worker1'),
  $$SELECT setval('invoice_no', (SELECT 5))$$) t(v bigint);

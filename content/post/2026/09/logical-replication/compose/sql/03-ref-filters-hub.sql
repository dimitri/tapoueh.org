-- @service: hub
-- Step 1c. Row filters (PG15+) and a column list (PG15+): one publication per worker.
SELECT format($f$CREATE PUBLICATION ref_w%1$s FOR TABLE plans, prices,
  customers (customer_id, name, worker_id, plan_id) WHERE (worker_id = %1$s)$f$, n)
FROM generate_series(1, 3) n \gexec

SELECT p.pubname, pt.tablename, pt.attnames, pt.rowfilter
FROM pg_publication p JOIN pg_publication_tables pt USING (pubname)
WHERE p.pubname LIKE 'ref_w%' AND pt.tablename = 'customers' ORDER BY 1;

-- The trap: the filter uses worker_id, which is not part of the replica identity (the PK).
-- The publisher refuses UPDATE on the table (at UPDATE time, not at CREATE PUBLICATION time):
UPDATE customers SET worker_id = 2 WHERE customer_id = 3;
-- Not even an unrelated column is safe: any UPDATE is refused
UPDATE customers SET name = 'renamed' WHERE customer_id = 3;
SELECT relreplident FROM pg_class WHERE oid = 'customers'::regclass;

-- Fix: a unique index over NOT NULL columns that includes the filter column,
-- used as replica identity (cheaper than REPLICA IDENTITY FULL).
CREATE UNIQUE INDEX customers_rid ON customers (customer_id, worker_id);
ALTER TABLE customers REPLICA IDENTITY USING INDEX customers_rid;
SELECT relreplident FROM pg_class WHERE oid = 'customers'::regclass;
UPDATE customers SET name = 'renamed' WHERE customer_id = 3;
UPDATE customers SET name = 'customer-3' WHERE customer_id = 3;

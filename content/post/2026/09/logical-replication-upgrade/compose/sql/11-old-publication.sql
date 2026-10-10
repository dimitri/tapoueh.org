-- @service: old
-- Step 11: publication FOR ALL TABLES on the old server, and the table that has
-- no primary key. Publishing does not change anything until UPDATE/DELETE hit
-- a table with no replica identity.
SELECT c.relname
  FROM pg_class c JOIN pg_namespace n ON n.oid = c.relnamespace
 WHERE n.nspname = 'public' AND c.relkind IN ('r', 'p')
   AND NOT EXISTS (SELECT 1 FROM pg_index i WHERE i.indrelid = c.oid AND i.indisprimary)
 ORDER BY 1;

CREATE PUBLICATION app_pub FOR ALL TABLES;

-- INSERT is fine, UPDATE and DELETE are refused on the publisher
INSERT INTO audit_log (event) VALUES ('after-publication');
UPDATE audit_log SET payload = 'x' WHERE event = 'after-publication';
DELETE FROM audit_log WHERE event = 'after-publication';

-- fix: REPLICA IDENTITY FULL (whole old row is logged), or add a unique index
-- on NOT NULL columns and use REPLICA IDENTITY USING INDEX
ALTER TABLE audit_log REPLICA IDENTITY FULL;
UPDATE audit_log SET payload = 'x' WHERE event = 'after-publication';
DELETE FROM audit_log WHERE event = 'after-publication';

SELECT relname, relreplident FROM pg_class
 WHERE relname IN ('audit_log', 'orders', 'measurements', 'measurements_2026') ORDER BY 1;
SELECT pubname, puballtables, pubinsert, pubupdate, pubdelete, pubtruncate, pubviaroot
  FROM pg_publication;
SELECT schemaname, tablename FROM pg_publication_tables ORDER BY 2;
SELECT slot_name, plugin, active FROM pg_replication_slots;

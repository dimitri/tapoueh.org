-- @service: pg19a
-- @db: postgres
-- Step 40 (part C, PostgreSQL 19 BETA): which build, and the syntax this build documents.
-- Everything in part C is beta behaviour and may change or be reverted before GA.
SELECT version();
SHOW wal_level;
SHOW effective_wal_level;
\h CREATE PUBLICATION
\h ALTER SUBSCRIPTION
\h CREATE SUBSCRIPTION
\h WAIT

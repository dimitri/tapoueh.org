-- Test scaffolding, loaded silently by run.sh on every node before each step.
CREATE SCHEMA IF NOT EXISTS lr;
CREATE EXTENSION IF NOT EXISTS dblink;

-- Poll a boolean SQL expression until true (fresh snapshot and stats every round).
CREATE OR REPLACE FUNCTION lr.wait_for(cond text, timeout_s int DEFAULT 60)
RETURNS boolean LANGUAGE plpgsql AS $$
DECLARE ok boolean; t0 timestamptz := clock_timestamp();
BEGIN
  LOOP
    PERFORM pg_stat_clear_snapshot();
    EXECUTE 'SELECT coalesce((' || cond || '), false)' INTO ok;
    IF ok THEN RETURN true; END IF;
    IF clock_timestamp() - t0 > timeout_s * interval '1 second' THEN
      RAISE EXCEPTION 'lr.wait_for timed out: %', cond;
    END IF;
    PERFORM pg_sleep(0.05);
  END LOOP;
END $$;

-- On a publisher: wait until every walsender has confirmed the WAL position
-- that was current when we were called.
CREATE OR REPLACE FUNCTION lr.wait_caught_up(timeout_s int DEFAULT 60)
RETURNS boolean LANGUAGE plpgsql AS $$
DECLARE target pg_lsn := pg_current_wal_lsn();
BEGIN
  RETURN lr.wait_for(format(
    'SELECT coalesce(bool_and(coalesce(replay_lsn, ''0/0'') >= %L::pg_lsn), true) FROM pg_stat_replication', target),
    timeout_s);
END $$;

CREATE OR REPLACE FUNCTION lr.log_lines(pat text) RETURNS SETOF text LANGUAGE sql AS $$
  SELECT l FROM regexp_split_to_table(pg_read_file('log/pg.log'), E'\n') WITH ORDINALITY t(l, n)
  WHERE l ~ pat ORDER BY n $$;

-- LSN following "finished at" in the last log line matching pat.
CREATE OR REPLACE FUNCTION lr.log_last_lsn(pat text) RETURNS pg_lsn LANGUAGE sql AS $$
  SELECT (regexp_match(l, 'finished at ([0-9A-F]+/[0-9A-F]+)'))[1]::pg_lsn
  FROM regexp_split_to_table(pg_read_file('log/pg.log'), E'\n') WITH ORDINALITY t(l, n)
  WHERE l ~ pat ORDER BY n DESC LIMIT 1 $$;

-- Replace the unstable parts (LSNs, xids, timestamps) so outputs can be diffed.
CREATE OR REPLACE FUNCTION lr.norm(t text) RETURNS text LANGUAGE sql IMMUTABLE AS $$
  SELECT regexp_replace(regexp_replace(regexp_replace(regexp_replace(regexp_replace(t,
    'pglogical (apply|manager) [0-9]+(:[0-9]+)?', 'pglogical \1 DB:SUB', 'g'),
    '[0-9A-F]+/[0-9A-F]+', 'X/X', 'g'),
    '(transaction|xid) [0-9]+', '\1 N', 'g'),
    '[0-9]{4}-[0-9]{2}-[0-9]{2} [0-9:.]+[+-][0-9]+', 'TS', 'g'),
    'origin "?pg_[0-9]+"?', 'origin pg_OID', 'g') $$;

-- Run a statement on another node of the demo (dblink, autocommit).
CREATE OR REPLACE FUNCTION lr.on(svc text, stmt text) RETURNS text LANGUAGE sql AS $$
  SELECT dblink_exec(format('host=%s dbname=app user=postgres', svc), stmt) $$;

CREATE OR REPLACE FUNCTION lr.conn(svc text) RETURNS text LANGUAGE sql IMMUTABLE AS $$
  SELECT format('host=%s dbname=app user=postgres', svc) $$;

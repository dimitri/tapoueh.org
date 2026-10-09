-- @service: hub
-- Step 3. One big transaction on worker1 (300000 rows), applied on the hub with
-- streaming = off, then = parallel. All driven from the hub through dblink so the
-- timeline is observable from a single session:
--   'holder' takes an advisory lock, 'big' inserts then blocks on that lock right before
--   COMMIT, so the transaction stays open while we look at both sides.
-- The publisher only streams an in-progress transaction once it exceeds
-- logical_decoding_work_mem (minimum 64kB), so lower it on worker1.
SELECT lr.on('worker1', 'ALTER SYSTEM SET logical_decoding_work_mem = ''64kB''');
SELECT pg_reload_conf() AS hub_reloaded,
       (SELECT ok FROM dblink(lr.conn('worker1'), 'SELECT pg_reload_conf()') t(ok bool)) AS worker1_reloaded;

SHOW max_parallel_apply_workers_per_subscription;
-- PG18 default when the option is not given:
SELECT subname, substream FROM pg_subscription WHERE subname = 'sub_usage_w1';

CREATE OR REPLACE FUNCTION lr.bigtxn(nrows int, expect_parallel boolean)
RETURNS TABLE (fact text) LANGUAGE plpgsql AS $fn$
DECLARE
  base bigint; t_commit timestamptz;
  max_par int := 0; n int; target pg_lsn;
BEGIN
  SELECT count(*) INTO n FROM pg_stat_subscription WHERE subname = 'sub_naive_w1' AND worker_type = 'parallel apply';
  fact := 'sub_naive_w1 parallel apply workers before the transaction: ' || n; RETURN NEXT;
  SELECT count(*) INTO base FROM usage_events WHERE worker_id = 1;
  PERFORM dblink_connect('holder', lr.conn('worker1'));
  PERFORM dblink_connect('big',    lr.conn('worker1'));
  PERFORM x FROM dblink('holder', 'SELECT pg_advisory_lock(42)::text') t(x text);
  PERFORM dblink_send_query('big', format(
    'BEGIN; INSERT INTO usage_events (customer_id, meter, qty) SELECT 1, ''bulk'', g FROM generate_series(1, %s) g;
     SELECT pg_advisory_xact_lock(42); COMMIT', nrows));
  -- 'big' is now blocked on the lock: all rows written, not committed
  PERFORM lr.wait_for($q$SELECT c = 1 FROM dblink('host=worker1 dbname=app user=postgres',
     $$SELECT count(*) FROM pg_stat_activity WHERE wait_event = 'advisory'$$) t(c bigint)$q$);
  -- Let the publisher decode and send everything written so far, and the hub receive it.
  SELECT l INTO target FROM dblink(lr.conn('worker1'), 'SELECT pg_current_wal_lsn()') t(l pg_lsn);
  PERFORM lr.wait_for(format($q$SELECT ok FROM dblink('host=worker1 dbname=app user=postgres',
     $$SELECT sent_lsn >= '%s' FROM pg_stat_replication WHERE application_name = 'sub_usage_w1'$$) t(ok bool)$q$, target));
  PERFORM lr.wait_for(format($q$SELECT received_lsn >= '%s' FROM pg_stat_subscription
     WHERE subname = 'sub_usage_w1' AND worker_type = 'apply'$q$, target));
  -- Only the parallel workers that belong to sub_usage_w1 (its apply worker was just restarted
  -- by ALTER SUBSCRIPTION, so its pool of parallel workers starts empty).
  SELECT count(*) INTO n FROM pg_stat_subscription WHERE subname = 'sub_usage_w1' AND worker_type = 'parallel apply';
  fact := 'sub_usage_w1 parallel apply workers before COMMIT: ' || n; RETURN NEXT;
  SELECT count(*) INTO n FROM usage_events WHERE worker_id = 1;
  fact := 'rows of worker 1 visible on hub before COMMIT: ' || (n - base); RETURN NEXT;
  SELECT 'publisher slot sub_usage_w1: stream_txns>0=' || (stream_txns > 0) || ' spill_txns>0=' || (spill_txns > 0)
    INTO fact FROM dblink(lr.conn('worker1'), $$SELECT stream_txns, spill_txns FROM pg_stat_replication_slots
      WHERE slot_name = 'sub_usage_w1'$$) t(stream_txns bigint, spill_txns bigint);
  RETURN NEXT;
  -- Release the lock: COMMIT happens now
  t_commit := clock_timestamp();
  PERFORM x FROM dblink('holder', 'SELECT pg_advisory_unlock(42)::text') t(x text);
  LOOP
    PERFORM pg_stat_clear_snapshot();
    SELECT count(*) INTO n FROM pg_stat_subscription WHERE subname = 'sub_usage_w1' AND worker_type = 'parallel apply';
    max_par := greatest(max_par, n);
    SELECT count(*) INTO n FROM usage_events WHERE worker_id = 1;
    EXIT WHEN n = base + nrows;
    IF clock_timestamp() - t_commit > interval '120 seconds' THEN RAISE EXCEPTION 'timeout'; END IF;
    PERFORM pg_sleep(0.01);
  END LOOP;
  fact := 'sub_usage_w1 parallel apply workers seen: ' || max_par; RETURN NEXT;
  SELECT count(*) INTO n FROM pg_stat_subscription WHERE subname = 'sub_naive_w1' AND worker_type = 'parallel apply';
  fact := 'sub_naive_w1 (does not publish usage_events) parallel apply workers: ' || n; RETURN NEXT;
  fact := 'commit_to_visible_ms_unstable: ' || round(extract(epoch FROM clock_timestamp() - t_commit) * 1000); RETURN NEXT;
  PERFORM x FROM dblink_get_result('big') t(x text);
  PERFORM dblink_disconnect('holder'); PERFORM dblink_disconnect('big');
END $fn$;

-- (a) streaming = off: the publisher spills to disk, the subscriber receives the whole
-- transaction only at COMMIT, and one apply worker replays it.
ALTER SUBSCRIPTION sub_usage_w1 SET (streaming = off);
SELECT lr.wait_for($$SELECT substream = 'f' AND (SELECT count(*) = 1 FROM pg_stat_subscription WHERE subname = 'sub_usage_w1' AND pid IS NOT NULL AND worker_type='apply') FROM pg_subscription WHERE subname = 'sub_usage_w1'$$);
SELECT * FROM lr.bigtxn(300000, false);

-- (b) streaming = parallel
ALTER SUBSCRIPTION sub_usage_w1 SET (streaming = parallel);
SELECT lr.wait_for($$SELECT substream = 'p' AND (SELECT count(*) = 1 FROM pg_stat_subscription WHERE subname = 'sub_usage_w1' AND pid IS NOT NULL AND worker_type='apply') FROM pg_subscription WHERE subname = 'sub_usage_w1'$$);
SELECT * FROM lr.bigtxn(300000, true);

-- Cleanup: remove the bulk rows on worker1 (another big transaction, replicated as DELETEs).
SELECT lr.on('worker1', $$DELETE FROM usage_events WHERE meter = 'bulk'$$);
SELECT lr.wait_for($$SELECT count(*) = 24 FROM usage_events$$, 120);
SELECT count(*) AS events_on_hub FROM usage_events;

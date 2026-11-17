CREATE EXTENSION regionseq;

SHOW regionseq.region_id;

CREATE SEQUENCE rs USING regionseq;
SELECT * FROM rs;

-- The parts of an id: the region from the setting, a counter that starts at 0
CREATE TEMP TABLE ids AS SELECT n, nextval('rs') AS id FROM generate_series(1, 5000) n;
SELECT regionseq_region(id) AS region, min(regionseq_counter(id)) AS first_counter
  FROM ids GROUP BY 1;
-- the timeline of the server, 1 on a cluster that was never promoted
SELECT regionseq_timeline(id) AS timeline, count(*) FROM ids GROUP BY 1;
-- strictly increasing, and unique
SELECT count(*) AS ids, count(distinct id) AS distinct_ids,
       bool_and(id > lag) AS increasing
  FROM (SELECT id, lag(id) OVER (ORDER BY n) AS lag FROM ids) s WHERE lag IS NOT NULL;
SELECT lastval() = (SELECT max(id) FROM ids) AS lastval_is_last;
SELECT abs(extract(epoch from now() - regionseq_time(max(id)))) < 60 AS time_is_now FROM ids;

-- A bulk load takes ids faster than 4,096 a millisecond: the ids run ahead of the
-- clock by the difference, and nothing is refused
CREATE TEMP TABLE bulk AS SELECT nextval('rs') AS id FROM generate_series(1, 300000);
SELECT count(distinct id) AS distinct_ids, max(id) > min(id) AS increasing,
       regionseq_time(max(id)) - clock_timestamp() < interval '5 seconds' AS less_than_5_s_ahead FROM bulk;

-- An identity column
SET default_sequence_access_method = 'regionseq';
CREATE TABLE t (id bigint GENERATED ALWAYS AS IDENTITY PRIMARY KEY, v int);
INSERT INTO t (v) SELECT generate_series(1, 3);
SELECT regionseq_region(id) AS region, v FROM t ORDER BY id;
RESET default_sequence_access_method;

-- 32-bit columns fail at once, instead of keeping a part of the id
SET default_sequence_access_method = 'regionseq';
CREATE TABLE t_int (a int GENERATED ALWAYS AS IDENTITY);
INSERT INTO t_int DEFAULT VALUES;
CREATE TABLE t_small (a smallint GENERATED ALWAYS AS IDENTITY);
INSERT INTO t_small DEFAULT VALUES;
RESET default_sequence_access_method;

-- Options that mean nothing for ids from the clock
CREATE SEQUENCE rs_step INCREMENT 10 USING regionseq;
SELECT nextval('rs_step');
CREATE SEQUENCE rs_cache CACHE 20 USING regionseq;
SELECT nextval('rs_cache');
CREATE SEQUENCE rs_max MAXVALUE 1000 USING regionseq;
SELECT nextval('rs_max');

-- setval and is_called: the stored state is an id, and the next ids are above it
SELECT setval('rs', 77777::bigint << 22, true) = 77777::bigint << 22 AS setval_returns;
SELECT last_ms, counter, is_called FROM rs;
SELECT regionseq_counter(nextval('rs')) AS counter_after_a_called_setval
  FROM (SELECT 1) s;
SELECT is_called FROM rs;

-- The clock is ahead of the last id: back to counter 0.  The clock is behind
-- it (here by 5 seconds): the ids go on from the last one, counter after counter.
SELECT setval('rs', (((extract(epoch from clock_timestamp()) * 1000)::bigint
                      - 1767225600000 + 5000) << 22), true) > 0 AS set_5_s_ahead;
SELECT regionseq_counter(nextval('rs')) AS first_counter,
       regionseq_counter(nextval('rs')) AS second_counter,
       regionseq_counter(nextval('rs')) AS third_counter;
SELECT last_ms - (extract(epoch from clock_timestamp()) * 1000)::bigint + 1767225600000 > 4000
       AS still_in_the_future FROM rs;

-- A clock more than regionseq.max_clock_regression behind: an error
SELECT setval('rs', (((extract(epoch from clock_timestamp()) * 1000)::bigint
                      - 1767225600000 + 120000) << 22), true) > 0 AS set_2_min_ahead;
DO $$
BEGIN
  PERFORM nextval('rs');
EXCEPTION WHEN object_not_in_prerequisite_state THEN
  RAISE NOTICE 'refused: %', sqlstate;
END $$;
SET regionseq.max_clock_regression = '3min';
SELECT regionseq_counter(nextval('rs')) AS counter_with_a_larger_limit;
RESET regionseq.max_clock_regression;

-- A restart goes back to the clock
ALTER SEQUENCE rs RESTART;
SELECT last_ms, counter, is_called FROM rs;
SELECT regionseq_counter(nextval('rs')) AS counter_after_restart;

-- Persistence: logged and temporary only
CREATE UNLOGGED SEQUENCE rs_unlogged USING regionseq;
ALTER SEQUENCE rs SET UNLOGGED;
CREATE TEMP SEQUENCE rs_temp USING regionseq;
SELECT regionseq_region(nextval('rs_temp')) AS temp_region;
ALTER SEQUENCE rs SET LOGGED;
SELECT relpersistence FROM pg_class WHERE relname = 'rs';

DROP TABLE t, t_int, t_small;
DROP SEQUENCE rs, rs_step, rs_cache, rs_max, rs_temp;
DROP EXTENSION regionseq;

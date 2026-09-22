-- @service: hub
-- Step 7. Workers minting ids without coordination: modulo offsets, and UUIDv7.
-- Both are shown against a table each; every worker writes, the hub subscribes to all three.

-- A. Modulo offset. Worker N owns the ids N, N+10, N+20, ... (room for ten workers).
SELECT lr.on('worker' || n, format('CREATE SEQUENCE mod_seq START %s INCREMENT 10', n))
  FROM generate_series(1, 3) n;
SELECT lr.on('worker' || n, $$CREATE TABLE usage_mod (
                                event_id bigint PRIMARY KEY DEFAULT nextval('mod_seq'),
                                qty      int    NOT NULL)$$)
  FROM generate_series(1, 3) n;
SELECT lr.on('worker' || n, 'CREATE PUBLICATION pub_mod FOR TABLE usage_mod')
  FROM generate_series(1, 3) n;

CREATE TABLE usage_mod (event_id bigint PRIMARY KEY, qty int NOT NULL);
SELECT format($f$CREATE SUBSCRIPTION sub_mod_w%1$s
  CONNECTION 'host=worker%1$s dbname=app user=postgres' PUBLICATION pub_mod$f$, n)
FROM generate_series(1, 3) n \gexec
SELECT lr.wait_for($$SELECT count(*) = 3 FROM pg_subscription_rel r JOIN pg_subscription s ON s.oid = r.srsubid
                    WHERE s.subname LIKE 'sub_mod%' AND r.srsubstate = 'r'$$);

-- every worker takes three ids; nothing coordinates them
SELECT lr.on('worker' || n, 'INSERT INTO usage_mod (qty) SELECT 1 FROM generate_series(1, 3)')
  FROM generate_series(1, 3) n;
SELECT lr.wait_for($$SELECT count(*) = 9 FROM usage_mod$$);

SELECT event_id, event_id % 10 AS minted_by_worker FROM usage_mod ORDER BY event_id;
SELECT subname, apply_error_count FROM pg_stat_subscription_stats WHERE subname LIKE 'sub_mod%' ORDER BY 1;

-- The limit: a worker number above the increment collides. "Worker 11" would start at 11,
-- an id that worker 1 already used and that is on the hub.
SELECT lr.on('worker2', $$CREATE SEQUENCE eleven START 11 INCREMENT 10$$);
SELECT v AS worker11_first_id FROM dblink(lr.conn('worker2'), $$SELECT nextval('eleven')$$) t(v bigint);
SELECT event_id, event_id % 10 AS minted_by_worker FROM usage_mod WHERE event_id = 11;

-- B. UUIDv7 (PG18+): time-ordered, no coordination and no headroom to plan.
SELECT lr.on('worker' || n, $$CREATE TABLE usage_uuid (
                                event_id uuid PRIMARY KEY DEFAULT uuidv7(),
                                worker   int  NOT NULL,
                                qty      int  NOT NULL)$$)
  FROM generate_series(1, 3) n;
SELECT lr.on('worker' || n, 'CREATE PUBLICATION pub_uuid FOR TABLE usage_uuid')
  FROM generate_series(1, 3) n;

CREATE TABLE usage_uuid (event_id uuid PRIMARY KEY, worker int NOT NULL, qty int NOT NULL);
SELECT format($f$CREATE SUBSCRIPTION sub_uuid_w%1$s
  CONNECTION 'host=worker%1$s dbname=app user=postgres' PUBLICATION pub_uuid$f$, n)
FROM generate_series(1, 3) n \gexec
SELECT lr.wait_for($$SELECT count(*) = 3 FROM pg_subscription_rel r JOIN pg_subscription s ON s.oid = r.srsubid
                    WHERE s.subname LIKE 'sub_uuid%' AND r.srsubstate = 'r'$$);

-- the workers insert one after the other, round robin, a few milliseconds apart
SELECT lr.on('worker' || (g % 3 + 1), format('INSERT INTO usage_uuid (worker, qty) VALUES (%s, 1)', g % 3 + 1))
  FROM generate_series(1, 9) g;
SELECT lr.wait_for($$SELECT count(*) = 9 FROM usage_uuid$$);

-- sorted by id, the hub sees the events in the order they were written, whichever worker wrote them
SELECT worker, uuid_extract_version(event_id) AS version FROM usage_uuid ORDER BY event_id;
-- and the timestamp is inside the id
SELECT bool_and(uuid_extract_timestamp(event_id) > now() - interval '5 minutes') AS timestamps_are_recent
  FROM usage_uuid;
SELECT lr.wait_caught_up();
SELECT subname, apply_error_count FROM pg_stat_subscription_stats WHERE subname LIKE 'sub_uuid%' ORDER BY 1;

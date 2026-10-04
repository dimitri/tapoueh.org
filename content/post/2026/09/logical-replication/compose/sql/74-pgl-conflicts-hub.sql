-- @service: pghub
-- Step 7e. Conflict resolution. pglogical.conflict_resolution = last_update_wins (server setting,
-- needs track_commit_timestamp = on). Same key written on both sides.
SHOW pglogical.conflict_resolution;
SHOW track_commit_timestamp;

-- Case A: the hub writes key (1, 9001) first, worker1 writes the same key later.
INSERT INTO usage_events VALUES (1, 9001, 1, 'conflict-A', 1);
SELECT lr.on('pgw1', $$INSERT INTO usage_events_w1 VALUES (1, 9001, 1, 'conflict-A', 2)$$);
SELECT lr.wait_for($$SELECT qty = 2 FROM usage_events WHERE worker_id = 1 AND event_id = 9001$$);
SELECT 'hub' AS node, meter, qty FROM usage_events WHERE event_id = 9001;   -- the later write (remote) won

-- Case B: worker1 writes key (1, 9002) first while the subscription is disabled, then the
-- hub writes it (later), then the subscription is enabled: the local row is the newer one.
SELECT pglogical.alter_subscription_disable('sub_usage_w1', immediate := true);
SELECT lr.on('pgw1', $$INSERT INTO usage_events_w1 VALUES (1, 9002, 1, 'conflict-B', 1)$$);
INSERT INTO usage_events VALUES (1, 9002, 1, 'conflict-B', 2);
SELECT pglogical.alter_subscription_enable('sub_usage_w1', immediate := true);
SELECT lr.on('pgw1', $$INSERT INTO usage_events_w1 VALUES (1, 9003, 1, 'marker', 1)$$);   -- arrives after the conflict
SELECT lr.wait_for($$SELECT count(*) = 1 FROM usage_events WHERE event_id = 9003$$);
SELECT 'hub' AS node, qty FROM usage_events WHERE event_id = 9002
UNION ALL
SELECT 'worker1', qty FROM dblink(lr.conn('pgw1'), 'SELECT qty FROM usage_events_w1 WHERE event_id = 9002') t(qty int);

-- What the log says
SELECT DISTINCT lr.norm(l) AS log_line FROM lr.log_lines('CONFLICT') l ORDER BY 1;

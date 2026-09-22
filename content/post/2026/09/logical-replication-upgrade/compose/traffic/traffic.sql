-- one application transaction: 7 writes touching every table kind
BEGIN;
WITH c AS (
  INSERT INTO customers (email) VALUES ('t' || :seq || '@example.com') RETURNING id
)
INSERT INTO orders (customer_id, amount, client_seq)
  SELECT id, (:seq % 100) + 0.99, :seq FROM c;
INSERT INTO audit_log (event, payload) VALUES ('order', 'seq=' || :seq);
INSERT INTO measurements (ts, sensor, value) VALUES (now(), :seq % 10, random());
UPDATE counters SET n = n + 1 WHERE name = 'orders';
COMMIT;
SELECT current_setting('server_version_num');

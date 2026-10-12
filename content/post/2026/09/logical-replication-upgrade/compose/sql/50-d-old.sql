-- @service: old2
-- @db: postgres
-- Step 50 (part D): schema trimmed to what traffic.sql needs. The gaps
-- (sequences, matview, large objects) are already covered in part A; this
-- demo is about the cutover mechanism, not the gaps.
\set ON_ERROR_STOP on
CREATE ROLE app  LOGIN PASSWORD 'app';
CREATE ROLE repl LOGIN REPLICATION PASSWORD 'repl';
GRANT pg_read_all_data TO repl;
CREATE DATABASE app OWNER app;
\c app
SET ROLE app;

CREATE TABLE customers (
  id    bigint GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
  email text NOT NULL UNIQUE
);
CREATE TABLE orders (
  id          bigserial PRIMARY KEY,
  customer_id bigint NOT NULL REFERENCES customers (id),
  amount      numeric(10,2) NOT NULL,
  client_seq  bigint
);
CREATE TABLE audit_log (
  at      timestamptz NOT NULL DEFAULT now(),
  event   text NOT NULL,
  payload text
);
CREATE TABLE measurements (
  ts     timestamptz NOT NULL,
  sensor int NOT NULL,
  value  float8 NOT NULL
);
CREATE TABLE counters (name text PRIMARY KEY, n bigint NOT NULL);
CREATE TABLE cutover_markers (id text PRIMARY KEY, at timestamptz NOT NULL DEFAULT now());

INSERT INTO customers (email) SELECT 'seed' || g || '@example.com' FROM generate_series(1, 2000) g;
INSERT INTO orders (customer_id, amount, client_seq) SELECT 1 + (g % 2000), (g % 500) + 0.99, -g FROM generate_series(1, 5000) g;
INSERT INTO counters VALUES ('orders', 0);

RESET ROLE;
ANALYZE;
CREATE PUBLICATION app_pub FOR ALL TABLES;
\dt+

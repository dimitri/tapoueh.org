-- @service: new2
-- @db: postgres
-- Step 51 (part D): the matching empty schema on the subscriber.
\set ON_ERROR_STOP on
CREATE ROLE app  LOGIN PASSWORD 'app';
CREATE ROLE repl LOGIN REPLICATION PASSWORD 'repl';
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

RESET ROLE;

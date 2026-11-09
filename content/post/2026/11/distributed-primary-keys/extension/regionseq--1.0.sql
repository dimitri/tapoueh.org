/* contrib/regionseq/regionseq--1.0.sql */

-- complain if script is sourced in psql, rather than via CREATE EXTENSION
\echo Use "CREATE EXTENSION regionseq" to load this file. \quit

CREATE FUNCTION regionseq_sequenceam_handler(internal)
  RETURNS sequence_am_handler
  AS 'MODULE_PATHNAME'
  LANGUAGE C;

CREATE ACCESS METHOD regionseq
  TYPE SEQUENCE HANDLER regionseq_sequenceam_handler;
COMMENT ON ACCESS METHOD regionseq IS 'regionseq sequence access method';

-- The parts of an id.  Region and counter are shifts and masks of the id;
-- a publication's row filter can say the same with bit operators:
--   WHERE ((id >> 16) & 63) = 2
CREATE FUNCTION regionseq_region(id int8) RETURNS int4
  LANGUAGE sql IMMUTABLE PARALLEL SAFE STRICT
  AS $$ SELECT ((id >> 16) & 63)::int4 $$;

CREATE FUNCTION regionseq_time(id int8) RETURNS timestamptz
  LANGUAGE sql IMMUTABLE PARALLEL SAFE STRICT
  AS $$ SELECT to_timestamp(((id >> 22) + 1767225600000)::float8 / 1000) $$;

CREATE FUNCTION regionseq_counter(id int8) RETURNS int4
  LANGUAGE sql IMMUTABLE PARALLEL SAFE STRICT
  AS $$ SELECT (id & 65535)::int4 $$;

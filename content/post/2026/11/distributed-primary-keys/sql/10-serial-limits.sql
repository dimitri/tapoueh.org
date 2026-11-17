-- Which width does the sequence behind each kind of key have?
create table t_serial    (id serial primary key);
create table t_bigserial (id bigserial primary key);
create table t_ident     (id int    generated always as identity primary key);
create table t_identbig  (id bigint generated always as identity primary key);

create sequence s_plain;
create table t_plain     (id int primary key default nextval('s_plain'));

select c.relname as "table", a.atttypid::regtype as column_type,
       s.seqtypid::regtype as sequence_type, s.seqmax as sequence_max
  from pg_class c
  join pg_attribute a on a.attrelid = c.oid and a.attname = 'id'
  join pg_sequence s on s.seqrelid =
       coalesce(pg_get_serial_sequence(c.relname, 'id'), 's_plain')::regclass
 where c.relname like 't\_%' and c.relkind = 'r'
 order by 1;

-- The serial column: the sequence stops first, at its own maximum.
select setval('t_serial_id_seq', 2147483646);
insert into t_serial default values;
insert into t_serial default values;

-- The plain sequence in front of an int column: the column stops first.
select setval('s_plain', 2147483647);
insert into t_plain default values;
insert into t_plain default values;
select last_value from s_plain;

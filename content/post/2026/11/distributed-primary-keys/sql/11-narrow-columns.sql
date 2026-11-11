-- Integer columns whose owned sequence is wider than the column: the sequence can pass 2147483647, the column can't.
select c.oid::regclass as "table", a.attname as "column", a.atttypid::regtype as column_type,
       q.oid::regclass as sequence, s.seqtypid::regtype as sequence_type, s.seqmax
  from pg_sequence s
  join pg_class q on q.oid = s.seqrelid
  join pg_depend d on d.objid = q.oid and d.classid = 'pg_class'::regclass
                  and d.refclassid = 'pg_class'::regclass and d.deptype in ('a', 'i')
  join pg_class c on c.oid = d.refobjid
  join pg_attribute a on a.attrelid = c.oid and a.attnum = d.refobjsubid
 where a.atttypid in ('int2'::regtype, 'int4'::regtype)
   and s.seqmax > case a.atttypid when 'int2'::regtype then 32767 else 2147483647 end;

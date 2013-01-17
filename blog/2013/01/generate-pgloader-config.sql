with reformat as (
   select relname, attnum, attname, typname,
          case typname
               when 'timestamptz'
               then attname || ':mynull:timestamp'
               when 'date'
               then attname || ':mynull:date'
           end as reformat
      from pg_class c
           join pg_namespace n on n.oid = c.relnamespace
           left join pg_attribute a on c.oid = a.attrelid
           join pg_type t on t.oid = a.atttypid
     where c.relkind = 'r'
           and attnum > 0
           and n.nspname = 'public'
),
 config_reformat as (
  select relname,
	 '['||relname||']' || E'\n' ||
	 'table = ' || relname || E' \n' ||
	 'filename = /path/to/csv/' || relname || E'.csv\n' ||
	 'format = csv' || E'\n' ||
	 'field_sep = \t' || E'\n' ||
	 'columns = *' || E' \n' ||
         'reformat = ' || array_to_string(array_agg(reformat), ', ')
         || E'\n' as config
    from reformat
   where reformat is not null
group by relname
),
 noreformat as (
   select relname, bool_and(reformat is null) as noreformating
     from reformat
 group by relname
),
 config_noreformat as (
  select relname,
	 '['||relname||']' || E'\n' ||
	 'table = ' || relname || E' \n' ||
	 'filename = /path/to/csv/' || relname || E'.csv\n' ||
	 'format = csv' || E'\n' ||
	 'field_sep = \t' || E'\n' ||
	 'columns = *' || E' \n'
         || E'\n' as config
    from reformat join noreformat using (relname)
   where noreformating
group by relname
),
allconfs as (
    select relname, config from config_reformat
 union all
    select relname, config from config_noreformat
)
select config
  from allconfs
 where relname not in ('tables', 'wedont', 'wantto', 'load')
 order by relname;

+++
date = "2010-07-26T17:00:00.000000+02:00"
title = "Partitioning: relation size per “group”"
tags = ["PostgreSQL", "Catalogs", "YeSQL"]
categories = ["PostgreSQL","YeSQL"]
thumbnailImage = "/img/old/repart-shapeopt.png"
thumbnailImagePosition = "left"
coverImage = "/img/old/repart-shapeopt.png"
coverSize = "partial"
coverMeta = "out"
aliases = ["/blog/2010/07/26-partitioning-relation-size-per-group",
           "/blog/2010/07/26-partitioning-relation-size-per-group.html"]
+++

This time, we are trying to figure out where is the bulk of the data on
disk. The trick is that we're using 
[DDL partitioning](http://www.postgresql.org/docs/current/static/ddl-partitioning.html), but we want a “nice”
view of size per 
*partition set*. Meaning that if you have for example a
parent table 
`foo` with partitions 
`foo_201006` and 
`foo_201007`, you would want
to see a single category 
`foo` containing the accumulated size of all the
partitions underneath 
`foo`.

Here we go:

~~~
select groupe, pg_size_pretty(sum(bytes)::bigint) as size, sum(bytes)
  from (
select relkind as k, nspname, relname, tablename, bytes,
         case when relkind = 'r' and relname ~ '[0-9]{6}$'
              then substring(relname from 1 for length(relname)-7)

	      when relkind = 'i' and  tablename ~ '[0-9]{6}$'
              then substring(tablename from 1 for length(tablename)-7)

              else 'core' 
          end as groupe
  from (
  select nspname, relname,
         case when relkind = 'i'
              then (select relname
                      from pg_index x 
                           join pg_class xc on x.indrelid = xc.oid
                           join pg_namespace xn on xc.relnamespace = xn.oid
                     where x.indexrelid = c.oid
                    )
              else null
           end as tablename,
         pg_size_pretty(pg_relation_size(c.oid)) as relation,
         pg_total_relation_size(c.oid) as bytes,
	 relkind
    from pg_class c join pg_namespace n on c.relnamespace = n.oid 
   where c.relkind in ('r', 'i') 
         and nspname in ('public', 'archive')
         and pg_total_relation_size(c.oid) > 32 * 1024
order by 5 desc
       ) as s
       ) as t
group by 1
order by 3 desc;
~~~


Note that by simply removing those last two lines here, you will get a
detailed view of the 
*indexes* and 
*tables* that are taking the most volume on
disk at your place.

Now, what about using 
[window functions](http://www.postgresql.org/docs/8.4/static/functions-window.html) here so that we get some better
detailed view of historic changes on each partition? With some evolution
figure in percentage from the previous partition of the same year,
accumulated size per partition and per year, yearly sum, you name it. Here's
another one you might want to try, ready for some tuning (schema name, table
name, etc):

~~~
WITH s AS (
  select relname, 
         pg_relation_size(c.oid) as size,
         pg_total_relation_size(c.oid) as tsize,
         substring(substring(relname from '[0-9]{6}$') for 4)::bigint as year
    from pg_class c 
         join pg_namespace n on n.oid = c.relnamespace 
   where c.relkind = 'r'
     -- and n.nspname = 'public'
     -- and c.relname ~ 'stats' 
     and substring(substring(relname from '[0-9]{6}$') for 4)::bigint >= 2008
order by relname
),
     sy AS (
  select relname, 
         size,
	 tsize,
         year,
         (sum(size) over w_year)::bigint as ysize,
         (sum(size) over w_month)::bigint as cumul,
	 (lag(size) over (order by relname))::bigint as previous 
    from s
  window w_year  as (partition by year),
         w_month as (partition by year order by relname)
),
     syp AS (
  select relname, 
         size, 
	 tsize,
	 rank() over (partition by year order by size desc) as rank,
         case when ysize = 0 then ysize 
	      else round(size / ysize::numeric * 100, 2) end as yp, 
         case when previous = 0 then previous
	      else round((size / previous::numeric - 1.0) * 100, 2) end as evol, 
         cumul, 
         year, 
         ysize
    from sy
)
  SELECT relname, 
         pg_size_pretty(size) as size,
         pg_size_pretty(tsize) as "+indexes",
         evol, yp as "% annuel", rank,
         pg_size_pretty(cumul) as cumul, year,
         pg_size_pretty(ysize) as "yearly sum", 
         pg_size_pretty((sum(size) over())::bigint) as total
    FROM syp
ORDER BY relname;
~~~


Hope you'll find it useful, I certainly do!

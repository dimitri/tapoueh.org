+++
date = "2011-07-27T22:35:00.000000+02:00"
title = "Next month partitions"
tags = ["PostgreSQL", "tricks", "catalogs"]
categories = ["PostgreSQL","Catalogs"]
thumbnailImage = "/img/old/library-card-catalogs.small.jpg"
thumbnailImagePosition = "left"
coverImage = "/img/old/library-card-catalogs.small.jpg"
coverSize = "partial"
coverMeta = "out"
aliases = ["/blog/2011/07/27-check-parts-for-next-month",
           "/blog/2011/07/27-check-parts-for-next-month.html"]
+++

When you do partition your tables monthly, then comes the question of when
to create next partitions.  I tend to create them just the week before next
month and I have some nice 
[nagios](http://www.nagios.org/) scripts to alert me in case I've forgotten
to do so.  How to check that by hand in the end of a month?

Here's a catalog query to help you there:

~~~
=> select * 
->   from
->   (
(>   select 'previous parts' as schemaname, count(*)::text as tablename
(>     from pg_tables
(>    where schemaname not in ('pg_catalog','information_schema')
(>      and tablename like to_char(now(), '%YYYYMM')
(>   
(>   union
(>   
(>   select schemaname, substring(tablename,1,length(tablename)-6) || '201108' 
(>     from pg_tables
(>    where schemaname not in ('pg_catalog','information_schema')
(>      and tablename like to_char(now(), '%YYYYMM')
(>   
(>   except
(>   
(>   select schemaname, tablename 
(>     from pg_tables
(>    where schemaname not in ('pg_catalog','information_schema')
(>      and tablename like to_char(now() + interval '1 month', '%YYYYMM')
(>   ) as t
-> order by schemaname <> 'previous parts', schemaname;
   schemaname   |       tablename        
----------------+------------------------
 previous parts | 1
 central        | stats_entrantes_201108
(2 rows)
~~~


As you see, our partitions are named 
`_YYYYMM` so that's it's easy to match
them in our queries, but I guess about everyone does about the same here.
Then the 
`to_char` expressions only allow to not enter manually 
`'%201108'` in
the query text.  And there's a trick so that we display how many partitions
we have this month, adding a line to the result...

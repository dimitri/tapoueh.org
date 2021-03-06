+++
date = "2010-03-17T13:35:00.000000+01:00"
title = "Finding orphaned sequences"
tags = ["PostgreSQL", "catalogs", "YeSQL"]
categories = ["PostgreSQL","YeSQL"]
thumbnailImage = "/img/old/nbt.2022-F1.640.jpg"
thumbnailImagePosition = "left"
coverImage = "/img/old/nbt.2022-F1.640.jpg"
coverSize = "partial"
coverMeta = "out"
aliases = ["/blog/2010/03/17-finding-orphaned-sequences",
           "/blog/2010/03/17-finding-orphaned-sequences.html"]
+++

This time we're having a database where 
*sequences* were used, but not
systematically as a 
*default value* of a given column. It's mainly an historic
bad idea, but you know the usual excuse with bad ideas and bad code: the
first 6 months it's experimental, after that it's historic.

<center>*Not talking about genome orphaned sequences here, though*</center>

Still, here's a query for 
`8.4` that will allow you to list those 
*sequences*
you have that are not used as a default value in any of your tables:

~~~
WITH seqs AS (
  SELECT n.nspname, relname as seqname
    FROM pg_class c
         JOIN pg_namespace n on n.oid = c.relnamespace
   WHERE relkind = 'S'
),
     attached_seqs AS (
  SELECT n.nspname, 
         c.relname as tablename,
         (regexp_matches(pg_get_expr(d.adbin, d.adrelid),
                         '''([^'']+)'''))[1] as seqname
    FROM pg_class c
         JOIN pg_namespace n on n.oid = c.relnamespace
         JOIN pg_attribute a on a.attrelid = c.oid
         JOIN pg_attrdef d on d.adrelid = a.attrelid
                            and d.adnum = a.attnum
                            and a.atthasdef
  WHERE relkind = 'r' and a.attnum > 0
        and pg_get_expr(d.adbin, d.adrelid) ~ '^nextval'
)

 SELECT nspname, seqname, tablename
   FROM seqs s
        LEFT JOIN attached_seqs a USING(nspname, seqname)
  WHERE a.tablename IS NULL;
~~~


I hope you don't need the query...

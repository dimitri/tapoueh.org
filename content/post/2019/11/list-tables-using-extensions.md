+++
title = "List PostgreSQL tables using extensions"
date = "2019-11-12T19:30:00+02:00"
tags = ["PostgreSQL","YeSQL","Extensions","Catalog"]
categories = ["PostgreSQL","YeSQL"]
coverImage = "/img/barn-images-t5YUoHW6zRo-unsplash.jpg"
coverMeta = "out"
coverSize = "partial"
thumbnailImage = "/img/workshop-logo-outils.png.jpg"
thumbnailImagePosition = "left"

+++

Postgres has extensions, and that's awesome! Of course as the author of
`CREATE EXTENSION` I'm a little biased… just remember that the ability to
extend Postgres is way more than just this command. The whole database
system has been design from the ground up to allow for extensibility. Parts
of the design is to be found in the way you can register new objects at
runtime: functions of course, and also data types, operators, index support
structures such as operator classes and families, even index access methods!

Today's article shows a query that you can use to list those tables in your
schemas that are using a data type which is provided by an extension.

<!--more-->

I came up with the following query, that scans through our `pg_depend`
catalog to find the data types provided by installed extensions, and then
scans through `pg_depend` again to find tables that have attributes
depending on those data types.

~~~ sql
with etypes as
 (
  select classid::regclass,
         objid,
         deptype,
         e.extname
    from pg_depend
         join pg_extension e
           on refclassid = 'pg_extension'::regclass
          and refobjid = e.oid
  where classid = 'pg_type'::regclass
 )
 select etypes.extname,
        etypes.objid::regtype as type,
        n.nspname as schema,
        c.relname as table,
        attname as column

  from pg_depend
  
       join etypes
         on etypes.classid = pg_depend.refclassid
        and etypes.objid = pg_depend.refobjid
        
       join pg_class c on c.oid = pg_depend.objid
       
       join pg_namespace n on n.oid = c.relnamespace
       
       join pg_attribute attr
         on attr.attrelid = pg_depend.objid
        and attr.attnum = pg_depend.objsubid
 where pg_depend.classid = 'pg_class'::regclass;
~~~

Here, the result is quite simple:

~~~
 extname │   type    │ schema  │  table  │  column  
─────────┼───────────┼─────────┼─────────┼──────────
 ip4r    │ ipaddress │ tweet   │ visitor │ ipaddr
 ip4r    │ ip4r      │ geolite │ blocks  │ iprange
 hll     │ hll       │ tweet   │ uniques │ visitors
 hstore  │ hstore    │ moma    │ audit   │ after
 hstore  │ hstore    │ moma    │ audit   │ before
(5 rows)
~~~

The reason I'm working on that is to provide the readers of my book [The Art
of PostgreSQL](https://theartofpostgresql.com) with an easier way to restore
the database used throughout the book. In a previous version of it, I tried
to be smarter that I should and the result isn't easy enough to use… I got
feedback about that, so let's try and improve things!

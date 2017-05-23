+++
date = "2011-05-04T11:45:00.000000+02:00"
title = "Tables and Views dependencies"
tags = ["PostgreSQL", "catalogs", "YeSQL"]
categories = ["PostgreSQL","YeSQL"]
thumbnailImage = "/img/old/stunning-view-table-mansion.jpg"
thumbnailImagePosition = "left"
coverImage = "/img/old/stunning-view-table-mansion.jpg"
coverSize = "partial"
coverMeta = "out"
aliases = ["/blog/2011/05/04-tables-and-views-dependencies",
           "/blog/2011/05/04-tables-and-views-dependencies.html"]
+++

Let's say you need to 
`ALTER TABLE foo ALTER COLUMN bar TYPE bigint;`, and
[PostgreSQL](http://postgresql.org) is helpfully telling you that no you can't because such and such
*views* depend on the column.  The basic way to deal with that is to copy
paste from the error message the names of the views involved, then prepare a
script wherein you first 
`DROP VIEW ...;` then 
`ALTER TABLE` and finally 
`CREATE
VIEW` again, all in the same transaction.


*A nice view from this table...*

So you have to copy paste also the view definitions.  With large view
definitions, it quickly gets cumbersome to do so.  Well when you're working
on operations, you have to bear in mind that cumbersome is a synonym for
*error prone*, in fact — so you want another solution if possible.

Oh, and the other drawback of this solution is that 
`ALTER TABLE` will first
take a 
`LOCK` on the table, locking out any activity.  And more than that, the
lock acquisition will queue behind current activity on the table, which
means waiting for a fairly long time and damaging the service quality on a
moderately loaded server.

It's possible to abuse the 
[system catalogs](http://www.postgresql.org/docs/current/static/catalogs.html) in order to find all 
*views* that
depend on a given table, too.  For that, you have to play with 
`pg_depend` and
you have to know that internally, a 
*view* is in fact a 
*rewrite rule*.  Then
here's how to produce the two scripts that we need:

~~~
=# \t
Showing only tuples.

=# \o /tmp/drop.sql
=# select 'DROP VIEW ' || views || ';'
     from (select distinct(r.ev_class::regclass) as views
            from pg_depend d join pg_rewrite r on r.oid = d.objid 
           where refclassid = 'pg_class'::regclass
             and refobjid = 'SCHEMA.TABLENAME'::regclass
             and classid = 'pg_rewrite'::regclass 
             and pg_get_viewdef(r.ev_class, true) ~ 'COLUMN_NAME') as x;

=# \o /tmp/create.sql
=# select 'CREATE VIEW ' || views || E' AS \n'
       || pg_get_viewdef(views, true) || ';' 
     from (select distinct(r.ev_class::regclass) as views 
          from pg_depend d join pg_rewrite r on r.oid = d.objid
         where refclassid = 'pg_class'::regclass
           and refobjid = 'SCHEMA.TABLENAME'::regclass
           and classid = 'pg_rewrite'::regclass
           and pg_get_viewdef(r.ev_class, true) ~ 'COLUMN_NAME') as x;

=# \o
~~~


Replace 
`SCHEMA.TABLENAME` and 
`COLUMN_NAME` with your targets here and the
first query should give you one row per candidate view.  Well if you're not
using the 
`\o` trick, that is — if you do, check out the generated file
instead, with 
`\! cat /tmp/drop.sql` for example.

Please note that this catalog query is not accurate, as it will select as a
candidate any view that will by chance both depend on the target table and
contain the 
`column_name` in its text definition.  So either filter out the
candidates properly (by proper proof reading then another 
`WHERE` clause), or
just accept that you might 
`DROP` then 
`CREATE` again more 
*views* than need be.

If you need some more details about the 
`\t \o` sequence you might be
interested in this older article about 
[resetting sequences](http://tapoueh.org/articles/blog/_Resetting_sequences._All_of_them,_please!.html).

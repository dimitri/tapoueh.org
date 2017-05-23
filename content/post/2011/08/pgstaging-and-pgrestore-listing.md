+++
date = "2011-08-29T18:05:00.000000+02:00"
title = "pg_restore -L & pg_staging"
tags = ["PostgreSQL", "pg_staging", "restore"]
categories = ["Projects","pg_staging"]
thumbnailImage = "/img/postgresql-512.jpg"
thumbnailImagePosition = "left"
coverImage = "/img/postgresql-512.jpg"
coverSize = "partial"
coverMeta = "out"
aliases = ["/blog/2011/08/29-pgstaging-and-pgrestore-listing",
           "/blog/2011/08/29-pgstaging-and-pgrestore-listing.html"]
+++

On the 
[PostgreSQL Hackers](http://archives.postgresql.org/pgsql-hackers) mailing lists, 
[Andrew Dunstan](http://people.planetpostgresql.org/andrew/) just proposed some
new options for 
`pg_dump` and 
`pg_restore` to ease our lives.  One of the
answers was talking about some scripts available to exploit the 
[pg_restore](http://www.postgresql.org/docs/9.0/static/app-pgrestore.html)
listing that you play with using options 
`-l` and 
`-L`, or the long name
versions 
`--list` and 
`--use-list`.  The 
[pg_staging](../../../pgsql/pgstaging.html) tool allows you to easily
exploit those lists too.

The 
`pg_restore` list is just a listing of one object per line of all objects
contained into a 
*custom* dump, that is one made with 
`pg_dump -Fc`.  You can
then tweak this listing in order to comment out some objects (prepending a 
`;`
to the line where you find it), and give your hacked file back to 
`pg_restore
--use-list` so that it will skip them.

What's pretty useful here, among other things, is that a table will have in
fact more than one line in the listing.  One is for the 
`TABLE` definition,
another one for the 
`TABLE DATA`.  So that 
`pg_staging` is able to provide you
with options for only restoring some 
*schemas*, some 
*schemas_nodata* and even
some 
*tablename_nodata_regexp*, to use directly the configuration options
names.

How to do a very simple exclusion of some table's data when restoring a
dump, will you ask me?  There we go.  Let's first prepare an environment,
where I have only a 
[PostgreSQL](http://www.postgresql.org/) server running.

~~~
$ git clone git://github.com/dimitri/pg_staging.git                                                         
$ git clone git://github.com/dimitri/pgloader.git
$ for s in */*.sql; do psql -f $s; done
$ pg_dump -Fc > pgloader.dump
~~~


Now I have a dump with some nearly random SQL objects in it, let's filter
out the tables named 
*reformat* and 
*parallel* from that.  We will take the
sample setup from the 
`pg_staging` project.  Going the quick route, we will
not even change the default sample database name that's used, which is
`postgres`.  After all, the 
`catalog` command of 
`pg_staging` that we're using
here is a 
*developer* command, you're supposed to be using 
`pg_staging` for a
lot more services that just this one.

~~~
$ cp pg_staging/pg_staging.ini .            
$ (echo "schemas = public";
   echo "tablename_nodata_regexp = parallel,reformat") \
  >> pg_staging.ini            
$ echo "catalog postgres pgloader.dump" \
   | python pg_staging/pg_staging.py -c pg_staging.ini
 ; Archive created at Mon Aug 29 17:17:49 2011
 ;
 ; [EDITED OUTPUT]
 ;
 ; Selected TOC Entries:
 ;
3; 2615 2200 SCHEMA - public postgres
1864; 0 0 COMMENT - SCHEMA public postgres
1536; 1259 174935 TABLE public parallel dimitri
1537; 1259 174943 TABLE public partial dimitri
1538; 1259 174951 TABLE public reformat dimitri
;1853; 0 174935 TABLE DATA public parallel dimitri
1854; 0 174943 TABLE DATA public partial dimitri
;1855; 0 174951 TABLE DATA public reformat dimitri
1834; 2606 174942 CONSTRAINT public parallel_pkey dimitri
1836; 2606 174950 CONSTRAINT public partial_pkey dimitri
1838; 2606 174955 CONSTRAINT public reformat_pkey dimitri
~~~


We can see that the objects indeed are skipped, now how to really go about
the 
`pg_restore` is like that:

~~~
$ createdb foo
$ echo "catalog postgres pgloader.dump" \
 |python pg_staging/pg_staging.py -c pg_staging.ini > short.list
$ pg_restore -L short.list -d foo pgloader.dump                                                              
~~~


The little bonus with using 
`pg_staging` is that when filtering out a 
*schema*
it will track all tables and triggers from that schema, and also the
functions used in the trigger definition.  Which is not as easy as it
sounds, believe me!

The practical use case is when filtering out 
`PGQ` and 
`Londiste`, then the 
`PGQ`
triggers will automatically be skipped by 
`pg_staging` rather than polluting
the 
`pg_restore` logs because the 
`CREATE TRIGGER` command could not find the
necessary implementation procedure.

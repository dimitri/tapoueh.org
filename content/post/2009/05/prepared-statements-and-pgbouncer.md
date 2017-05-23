+++
date = "2009-05-14T00:00:00.000000+02:00"
title = "Prepared Statements and pgbouncer"
tags = ["PostgreSQL", "release", "preprepare"]
categories = ["Projects","preprepare"]
thumbnailImage = "/img/postgresql-512.jpg"
thumbnailImagePosition = "left"
coverImage = "/img/postgresql-512.jpg"
coverSize = "partial"
coverMeta = "out"
aliases = ["/blog/2009/05/14-prepared-statements-and-pgbouncer",
           "/blog/2009/05/14-prepared-statements-and-pgbouncer.html"]
+++

On the performance mailing list, a recent 
[thread](http://archives.postgresql.org/pgsql-performance/2009-05/msg00026.php) drew my attention. It
devired to be about using a connection pool software and prepared statements
in order to increase scalability of PostgreSQL when confronted to a lot of
concurrent clients all doing simple 
`select` queries. The advantage of the
*pooler* is to reduce the number of 
*backends* needed to serve the queries, thus
reducing PostgreSQL internal bookkeeping. Of course, my choice of software
here is clear: 
[PgBouncer](https://developer.skype.com/SkypeGarage/DbProjects/PgBouncer) is an excellent top grade solution, performs real
well (it won't parse queries), reliable, flexible.

The problem is that while conbining 
`pgbouncer` and 
[prepared statements](http://www.postgresql.org/docs/current/static/sql-prepare.html) is
possible, it requires the application to check at connection time if the
statements it's interrested in are already prepared. This can be done by a
simple catalog query of this kind:

~~~
SELECT name
    FROM pg_prepared_statements 
   WHERE name IN ('my', 'prepared', 'statements');
~~~


Well, this is simple but requires to add some application logic. What would
be great would be to only have to 
`EXECUTE my_statement(x, y, z)` and never
bother if the 
`backend` connection is a fresh new one or an existing one, as
to avoid having to check if the application should 
`prepare`.

The 
[preprepare](http://preprepare.projects.postgresql.org/) pgfoundry project is all about this: it comes with a
`prepare_all()` function which will take all statements present in a given
table (
`SET preprepare.relation TO 'schema.the_table';`) and prepare them for
you. If you now tell 
`pgbouncer` to please call the function at 
`backend`
creation time, you're done (see 
`connect_query`).

There's even a detailed 
[README](http://preprepare.projects.postgresql.org/README.html) file, but no release yet (check out the code
in the 
[CVS](http://cvs.pgfoundry.org/cgi-bin/cvsweb.cgi/preprepare/preprepare/), 
`pgfoundry` project page has 
[clear instruction](http://pgfoundry.org/scm/?group_id=1000442) about how to do so.

+++
title = "from MySQL to PostgreSQL"
date = "2017-07-06T17:30:11+02:00"
tags = ["PostgreSQL","MySQL","pgloader"]
categories = ["PostgreSQL","pgloader"]
coverImage = "/img/old/type-casting-machine.640.jpg"
coverMeta = "out"
coverSize = "partial"
thumbnailImage = "/img/postgresql-elephant.png"
thumbnailImagePosition = "left"

+++

Today
[pgloader v3.4.1](https://github.com/dimitri/pgloader/releases/tag/v3.4.1)
is released and available! This new release comes with 110 commits as show
in
[github compare view](https://github.com/dimitri/pgloader/compare/v3.3.2...v3.4.1).

This release of [pgloader](http://pgloader.io) is following the tradition of
simplifying things for users, or if you allow me to
quote [Alan Kay](https://en.wikiquote.org/wiki/Alan_Kay), I believe that if
*simple things should be simple, complex things should be possible.*

<!--more-->

{{< alert info >}}

This article mentions pgloader features that are new as of version 3.4.1,
check which version you're running with `pgloader --version`. It is easy
enough to build pgloader from sources, and pgloader is packaged in debian
and derivatives, and in RPM based distributions too.

Have a look at <http://apt.postgresql.org> and <https://yum.postgresql.org/>
for up to date packaged options in your distribution of choice and your
PostgreSQL version of choice.

{{< /alert >}}

<!--toc-->

# Migrate from MySQL to PostgreSQL

An example of pgloader simplicity is that you can migrate a whole MySQL
database, including its schema definition of tables and indexes, primary key
and foreign key constraints, comments and default values, even when they
require installing a trigger in PostgreSQL, all of that in a single command
line:

~~~ bash
$ pgloader mysql://root@localhost/f1db pgsql:///f1db
2017-07-06T17:57:04.679000+02:00 LOG report summary reset
               table name       read   imported     errors      total time
-------------------------  ---------  ---------  ---------  --------------
          fetch meta data         33         33          0          0.206s 
           Create Schemas          0          0          0          0.026s 
         Create SQL Types          0          0          0          0.007s 
            Create tables         26         26          0          0.057s 
           Set Table OIDs         13         13          0          0.005s 
-------------------------  ---------  ---------  ---------  --------------
            f1db.circuits         73         73          0          0.093s 
  f1db.constructorresults      11011      11011          0          0.139s 
        f1db.constructors        208        208          0          0.047s 
             f1db.drivers        841        841          0          0.073s 
f1db.constructorstandings      11766      11766          0          0.237s 
            f1db.laptimes     413578     413578          0          2.643s 
     f1db.driverstandings      31420      31420          0          0.392s 
            f1db.pitstops       5796       5796          0          0.132s 
               f1db.races        976        976          0          0.076s 
             f1db.seasons         68         68          0          0.053s 
          f1db.qualifying       7257       7257          0          0.091s 
             f1db.results      23514      23514          0          0.431s 
              f1db.status        133        133          0          0.137s 
-------------------------  ---------  ---------  ---------  --------------
  COPY Threads Completion          4          4          0          3.265s 
           Create Indexes         20         20          0          1.760s 
   Index Build Completion         20         20          0          0.715s 
          Reset Sequences         10         10          0          0.029s 
             Primary Keys         13         13          0          0.010s 
      Create Foreign Keys          0          0          0          0.000s 
          Create Triggers          0          0          0          0.000s 
         Install Comments          0          0          0          0.000s 
-------------------------  ---------  ---------  ---------  --------------
        Total import time     506641     506641          0          4.888s 
~~~

You can easily reproduce that at home with
the [Ergast database](http://ergast.com/mrd/db/), a historical record of
motor racing data for non-commercial purposes. Download the *f1db.sql.gz*
MySQL 5.1 database dump from there, install it in some MySQL instance and
then run the command above.

To be complete, let's mention that pgloader will not create the target
database, so you have to do that yourself. Also, currently pgloader doesn't
change the *search_path* of the target database, so you might want to do
that yourself. Which means you need the following 3 commands if we wanted to
be exhaustive (the migration itself only happens at the pgloader step):

~~~ bash
$ createdb f1db
$ pgloader mysql://root@localhost/f1db pgsql:///f1db
$ psql -d f1db -c 'ALTER DATABASE f1db SET search_path TO f1db, public;'
~~~

[pgloader](http://pgloader.io) needs a live MySQL database to operate its
magic. It is less error prone to query system's catalogs than to learn how
to parse all those SQL dialects.

You might have seen that the automated migration targets a PostgreSQL schema
with the same name as the MySQL database. That's because in MySQL there's
not really a notion of SQL standard catalogs, which are called *databases*
by most of the industry. What MySQL names a *database* actually is what the
standard (and PostgreSQL) refers to as a *schema*.

## MySQL spatial indexes over geometry

Starting in this new release of pgloader, MySQL spatial keys are
automatically converted to PostgreSQL GiST indexes. In previous versions of
pgloader, when playing with
the [Sakila Sample Database](https://dev.mysql.com/doc/sakila/en/) you would
have the following error:

~~~
ERROR PostgreSQL Database error 42704: data type point has no default operator class for access method "btree"
HINT: You must specify an operator class for the index or define a default operator class for the data type.
QUERY: CREATE INDEX idx_188012_idx_location ON pagila.address (location);
~~~

In that case what we should do is issue to PostgreSQL the following *CREATE
INDEX* command instead:

~~~ sql
CREATE INDEX idx_188012_idx_location ON pagila.address USING gist(location);
~~~

And guess what? yes of course this is a paste from a run of current
pgloader's version, where this is implemented in a transparent way for the
users. Also, rather than hardcoding which PostgreSQL types need a special
treatment here, pgloader runs the following SQL query against the PostgreSQL
catalogs, as found in the source tree
at
[src/pgsql/sql/list-typenames-without-btree-support.sql](https://github.com/dimitri/pgloader/blob/master/src/pgsql/sql/list-typenames-without-btree-support.sql):

~~~ sql
select typname,
       array_agg(amname order by amname <> 'gist', amname <> 'gin')
  from pg_type
       join pg_opclass on pg_opclass.opcintype = pg_type.oid
       join pg_am on pg_am.oid = pg_opclass.opcmethod
 where substring(typname from 1 for 1) <> '_'
       and not exists
       (
         select amname
           from pg_am am
                join pg_opclass c on am.oid = c.opcmethod
                join pg_type t on c.opcintype = t.oid
          where amname = 'btree' and t.oid = pg_type.oid
       )
group by typname;
~~~

The typical result of the query is following. The important part of
discovering data types without btree support at run time is that pgloader
then will support locally installed extensions it knows nothing about, and
that you can target in the user-defined casting rules (see below).

~~~ psql
  typname   |      array_agg       
------------+----------------------
 aclitem    | {hash}
 box        | {gist,brin,spgist}
 cid        | {hash}
 circle     | {gist}
 int2vector | {hash}
 point      | {gist,spgist,spgist}
 polygon    | {gist}
 xid        | {hash}
(8 rows)
~~~

In version 3.4.1 of pgloader this feature is limited to single column
indexes. If you need to support multiple-columns indexes, let me know by
[opening an issue](https://github.com/dimitri/pgloader/issues).

# Migrate from SQLite to PostgreSQL

In much the same vein it is possible to migrate a full SQLite database into
PostgreSQL in a single command line. This time, as SQLite databases can be
distributed in a single file, we can even have pgloader download a file over
HTTP for us. Here's a full example using
the [chinook database](https://github.com/lerocha/chinook-database), where
again pgloader takes care of the schema definition, indexes, primary keys,
foreign keys, default values, comments... the whole thing:

~~~ bash
$ pgloader https://github.com/lerocha/chinook-database/raw/master/ChinookDatabase/DataSources/Chinook_Sqlite_AutoIncrementPKs.sqlite pgsql:///chinook
2017-07-06T18:16:52.256000+02:00 LOG Fetching 'https://github.com/lerocha/chinook-database/raw/master/ChinookDatabase/DataSources/Chinook_Sqlite_AutoIncrementPKs.sqlite'
2017-07-06T18:16:54.118000+02:00 ERROR PostgreSQL Database error 42P16: multiple primary keys for table "playlisttrack" are not allowed
QUERY: ALTER TABLE playlisttrack ADD PRIMARY KEY USING INDEX idx_189226_sqlite_autoindex_playlisttrack_1;
2017-07-06T18:16:54.119000+02:00 LOG report summary reset
             table name       read   imported     errors      total time
-----------------------  ---------  ---------  ---------  --------------
                  fetch          0          0          0          1.305s 
        fetch meta data         33         33          0          0.034s 
         Create Schemas          0          0          0          0.001s 
       Create SQL Types          0          0          0          0.006s 
          Create tables         22         22          0          0.096s 
         Set Table OIDs         11         11          0          0.007s 
-----------------------  ---------  ---------  ---------  --------------
                  album        347        347          0          0.020s 
                 artist        275        275          0          0.017s 
               customer         59         59          0          0.024s 
                  genre         25         25          0          0.024s 
            invoiceline       2240       2240          0          0.049s 
               employee          8          8          0          0.018s 
                invoice        412        412          0          0.032s 
               playlist         18         18          0          0.026s 
              mediatype          5          5          0          0.030s 
          playlisttrack       8715       8715          0          0.064s 
                  track       3503       3503          0          0.131s 
-----------------------  ---------  ---------  ---------  --------------
COPY Threads Completion          4          4          0          0.188s 
         Create Indexes         22         22          0          0.168s 
 Index Build Completion         22         22          0          0.066s 
        Reset Sequences          0          0          0          0.029s 
           Primary Keys         12         11          1          0.013s 
    Create Foreign Keys         11         11          0          0.042s 
        Create Triggers          0          0          0          0.001s 
       Install Comments          0          0          0          0.000s 
-----------------------  ---------  ---------  ---------  --------------
      Total import time      15607      15607          0          2.123s 
~~~

The SQLite catalog inspection isn't as straightforward as one would want to,
and it's been difficult to understand when exactly is an index a primary key
index. The documentation
of
[ROWIDs and the INTEGER PRIMARY KEY](http://www.sqlite.org/lang_createtable.html#rowid) did
sched some light on the issues and we're good now, it seems. Well I'm not
too sure about the error you see above, complaining that *multiple primary
keys for table "playlisttrack" are not allowed*. We might still have some
SQLite catalogs introspection hacking to do to be all good.

# Migrate from MS SQL to PostgreSQL

The new release of pgloader also comes with lots of small improvements to
the MS SQL support, where it now mostly just works. That's thanks to users
who did provide me with a test case to reproduce then fix the bugs. That
really is a life changer, in particular with proprietay software, where I
can't have a local copy running for my tests.

# Load CSV files

At the beginning, [pgloader](http://pgloader.io/howto/quickstart.html) was
meant to load data from CSV files into PostgreSQL. For a large and flexible
definition of CSV. Now we include support for the COPY file format, fixed
width files, dBase files and IXF files.

# The load command and new options

As mentioned earlier simple things are simple, and sometimes you need to do
more complex things, and the advanced use cases are handled by the pgloader
command language. This is
a [DSL](https://en.wikipedia.org/wiki/Domain-specific_language) compiled to
binary at pgloader's run time.

## The ORM case: migrating to an existing schema

A classic complex case is when you're using an ORM that handles your
database schema for your application. It usually makes migrating easier, and
every thing else more difficult, so I don't recommend using ORMs. See
my [How to write SQL](/blog/2017/06/anonsql/) article in this blog for more
details. And maybe the old and
good
[The Vietnam of Computer Science](http://blogs.tedneward.com/post/the-vietnam-of-computer-science/) too,
if you haven't already.

Anyway, disliking ORMs is not reason enough to leave users out in the cold.
Of course pgloader supports your use case, and with the *data* only option
you can have it run a migration by discovering both the source and target
schemas and then working on a *merge* with the data from the source.

New in this release is the ability to target a pre-existing schema and still
benefit from the pgloader indexes processing, which is a very nice boost.
When loading lots of data into PostgreSQL it's best to:

  1. remove existing indexes, thus foreign keys that depend on them,
  2. load data,
  3. create indexes again, all at once in parallel,
  4. upgrade unique indexes into primary keys with *ALTER TABLE*,
  5. reinstall foreign key constraints with indexes in place.

And of couse, as it's the best way to do things, that's exactly what
pgloader is doing. Now, even when the indexes are pre-defined in the target
schema, provided that you use the *drop indexes* option to the *WITH* clause.

Note that you can still use it on the command line, without a command file,
as in this example:

~~~ bash
$ pgloader --with "data only"          \
           --with truncate             \
           --with "drop indexes"       \
           --verbose                   \
           mysql://root@localhost/f1db \
           pgsql:///f1db
~~~

For this command to work, the f1db PostgreSQL target database must have been
defined before, with the right set of SQL objects of course. Which is
exactly what ORMs are doing for you, I've been told.

## Schema mapping options 

In the command language it is possible to map schema names and table names
from your source to your target database, and to add options to the load
scenario, as in the following example:

~~~
load database
     from      mysql://root@localhost/sakila
     into postgresql:///sakila

 ALTER TABLE NAMES MATCHING 'sales_by_store' RENAME TO 'sales_by_store_list'
 ALTER TABLE NAMES MATCHING 'film' RENAME TO 'films'
 ALTER TABLE NAMES MATCHING ~/./ SET (fillfactor='40')

 ALTER SCHEMA 'sakila' RENAME TO 'pagila'
~~~

Here the *ALTER* commands are not executed in any database, neither the
source not the target. They are used as a mapping specifications from the
source to the target, so that we create tables found in the *sakila* schema
in the source into the *pagila* schema in the target.

Also you can see that we change
the
[fillfactor](https://www.postgresql.org/docs/current/static/sql-createtable.html#SQL-CREATETABLE-STORAGE-PARAMETERS) for
all tables in the target. The *ALTER* clauses in the pgloader command are
realized over the internal pgloader catalogs, so that the actual
implementation is done at *CREATE TABLE* time in PostgreSQL.

## User defined casting

The new version is not adding things to the capability of pgloader to allow
for user defined type casting from the source to the target database. Some
default cast rules have been improved, and you can
read
[pgloader reference documentation](http://pgloader.io/howto/pgloader.1.html)
for details.

## MySQL multiple readers support

An experimental feature made it to this release of pgloader, and it's
currently only available for MySQL source type. It is now possible for
pgloader to have several concurrent readers working on the same source
table.

The idea is that they split the reading by using *WHERE* clauses, each over
a range of the data. That allows for MySQL Index Range Scan as documented at
their
[Range Optimization](https://dev.mysql.com/doc/refman/5.7/en/range-optimization.html) documentation page.

In my testing I didn't have access to a machine with enough CPUs to make
this optimisation worthwile, but it might still be useful. Have a try if you
need it, and tell me if it helped!

Here's an example of a setup that uses the *multiple readers per thread*
feature, new in pgloader v3.4.1:

~~~
load database
     from      mysql://root@localhost/sakila
     into postgresql:///sakila

 WITH concurrency = 2, workers = 6,
      prefetch rows = 25000,
      multiple readers per thread, rows per range = 50000;
~~~

# Conclusion

{{< image classes="fig25 right dim-margin"
              src="/img/old/sql-logo.png"
           title="PostgreSQL is YeSQL" >}}

Now that you're using PostgreSQL, enjoy a powerful SQL dialect with plenty
of advanced functions to process your data exactly the way you need it!

You might be interested
into [Exploring a Data Set in SQL](/blog/2017/06/exploring-a-data-set/) if
you're new to PostgreSQL or the database you've just migrated.

Also if it makes sense for your business to contribute financially to
pgloader's development, consider buying
a [pgloader Moral License](http://pgloader.io/pgloader-moral-license.html).
Cases when it makes sense include successful migrations on a low budget and
within the time constraints thanks to using pgloader! You may then (at your
choice) join the [pgloader sponsors page](http://pgloader.io/sponsors.html)!

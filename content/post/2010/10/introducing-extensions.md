+++
date = "2010-10-21T13:45:00.000000+02:00"
title = "Introducing Extensions"
tags = ["PostgreSQL", "Extensions", "release", "ip4r", "plpgsql", "backup", "restore", "prefix", "9.1"]
categories = ["Projects","prefix"]
thumbnailImage = "/img/postgresql-512.jpg"
thumbnailImagePosition = "left"
coverImage = "/img/postgresql-512.jpg"
coverSize = "partial"
coverMeta = "out"
aliases = ["/blog/2010/10/21-introducing-extensions",
           "/blog/2010/10/21-introducing-extensions.html"]
+++

After reading 
[Simon's blog post](http://database-explorer.blogspot.com/2010/10/extensions-in-91.html), I can't help but try to give some details
about what it is exactly that I'm working on. As he said, there are several
aspects to 
*extensions* in 
[PostgreSQL](http://www.postgresql.org/), it all begins here:
[Chapter 35. Extending SQL](http://www.postgresql.org/docs/9.0/interactive/extend.html).

It's possible, and mostly simple enough, to add your own code or behavior to
PostgreSQL, so that it will use your code and your semantics while solving
user queries. That's highly useful and it's easy to understand how so when
you look at some projects like 
[PostGIS](http://postgis.refractions.net/), 
[ip4r](http://pgfoundry.org/projects/ip4r/) (index searches of 
`ip` in a
`range`, not limited to 
`CIDR` notation), or our own 
*Key Value Store*, 
[hstore](http://www.postgresql.org/docs/9.0/interactive/hstore.html).


## So, what's in an *Extension*?

An 
*extension* in its simple form is a 
`SQL` 
*script* that you load on your
database, but manage separately. Meaning you don't want the script to be
part of your backups. Often, that kind of script will create new datatypes
and operators, support functions, user functions and index support, and then
it would include some 
`C` code that ships in a 
*shared library object*.

As far as PostgreSQL is concerned, at least in the current version of my
patch, the extension is first a 
*meta* information file that allows to
register it. We currently call that the 
`control` file. Then, it's an 
`SQL`
script that is 
*executed* by the server when you 
`create` the 
*extension*.

If it so happens that the 
`SQL` script depends on some 
*shared library objects*
file, this has to be present at the right place (
`MODULE_PATHNAME`) for the
*extension* to be successfully created, but that's always been the case.

The problem with current releases of PostgreSQL, that the 
*extension* patch is
solving, is the 
`pg_dump` and 
`pg_restore` support. We said it, you don't want
the 
`SQL` script to be part of your dump, because it's not maintained in your
database, but in some code repository out there. What you want is to be able
to install the 
*extension* again at the file system level then 
`pg_restore` your
database — that depends on it being there.

And that's exactly what the 
*extension* patch provides. By now having a 
`SQL`
object called an 
`extension`, and maintained in the new 
`pg_extension` catalog,
we have an 
`Oid` to refer to. Which we do by recording a dependency between
any object created by the script and the 
*extension* 
`Oid`, so that 
`pg_dump` can
be instructed to skip those.


## Examples?

So, let's have a look at what you can do if you play with a patched
development server version, or if you play directly from the 
`git` repository
at
[http://git.postgresql.org/gitweb?p=postgresql-extension.git;a=shortlog;h=refs/heads/extension](http://git.postgresql.org/gitweb?p=postgresql-extension.git;a=shortlog;h=refs/heads/extension)

~~~
dim ~ createdb exts
dim ~ psql exts
psql (9.1devel)
Type "help" for help.

dim=# \dx+
                                                        List of extensions
        Name        | | |                               Description                               
--------------------+-+-+-------------------------------------------------------------------------
 adminpack          | | | Administrative functions for PostgreSQL
 auto_username      | | | functions for tracking who changed a table
 autoinc            | | | functions for autoincrementing fields
 btree_gin          | | | GIN support for common types BTree operators
 btree_gist         | | | GiST support for common types BTree operators
 chkpass            | | | Store crypt()ed passwords
 citext             | | | case-insensitive character string type
 cube               | | | data type for representing multidimensional cubes
 dblink             | | | connect to other PostgreSQL databases from within a database
 dict_int           | | | example of an add-on dictionary template for full-text search
 dict_xsyn          | | | example of an add-on dictionary template for full-text search
 earthdistance      | | | calculating great circle distances on the surface of the Earth
 fuzzystrmatch      | | | determine similarities and distance between strings
 hstore             | | | storing sets of key/value pairs
 int_aggregate      | | | integer aggregator and an enumerator (obsolete)
 intarray           | | | one-dimensional arrays of integers: functions, operators, index support
 isn                | | | data types for the international product numbering standards
 lo                 | | | managing Large Objects
 ltree              | | | data type for hierarchical tree-like structure
 moddatetime        | | | functions for tracking last modification time
 pageinspect        | | | inspect the contents of database pages at a low level
 pg_buffercache     | | | examine the shared buffer cache in real time
 pg_freespacemap    | | | examine the free space map (FSM)
 pg_stat_statements | | | tracking execution statistics of all SQL statements executed
 pg_trgm            | | | determine the similarity of text, with indexing support
 pgcrypto           | | | cryptographic functions
 pgrowlocks         | | | show row locking information for a specified table
 pgstattuple        | | | obtain tuple-level statistics
 prefix             | | | Prefix Match Indexing
 refint             | | | functions for implementing referential integrity
 seg                | | | data type for representing line segments, or floating point intervals
 tablefunc          | | | various functions that return tables, including crosstab(text sql)
 test_parser        | | | example of a custom parser for full-text search
 timetravel         | | | functions for implementing time travel
 tsearch2           | | | backwards-compatible text search functionality (pre-8.3)
 unaccent           | | | text search dictionary that removes accents
(36 rows)
~~~


Ok I've edited the output in a visible way, to leave the 
*Version* and 
*Custom
Variable Classes* column out. It's taking lots of screen place and it's not
that useful here. Maybe the 
*classes* one will even get dropped out of the
patch before reaching 
`9.1`, we'll see.

Let's pick an extension there and install it in our new database:

~~~
exts=# create extension pg_trgm;
NOTICE:  Installing extension 'pg_trgm' from '/Users/dim/pgsql/exts/share/contrib/pg_trgm.sql', with user data
CREATE EXTENSION
exts=# \dx
                                           List of extensions
  Name   |  |  |                       Description                       
---------+--+--+---------------------------------------------------------
 pg_trgm |  |  | determine the similarity of text, with indexing support
(1 row)
~~~


See, that was easy enough. Same thing, the extra columns have been
removed. So, what's in this extension, will you ask me, what are those
objects that you would normally (that is, before the patch) find in your
`pg_dump` backup script?

~~~
exts=# select * from pg_extension_objects('pg_trgm');
    class     | classid | objid |                                                                objdesc                                                                 
--------------+---------+-------+----------------------------------------------------------------------------------------------------------------------------------------
 pg_extension |    3996 | 18498 | extension pg_trgm
 pg_proc      |    1255 | 18499 | function set_limit(real)
 pg_proc      |    1255 | 18500 | function show_limit()
 pg_proc      |    1255 | 18501 | function show_trgm(text)
 pg_proc      |    1255 | 18502 | function similarity(text,text)
 pg_proc      |    1255 | 18503 | function similarity_op(text,text)
 pg_operator  |    2617 | 18504 | operator %(text,text)
 pg_type      |    1247 | 18505 | type gtrgm
 pg_proc      |    1255 | 18506 | function gtrgm_in(cstring)
 pg_proc      |    1255 | 18507 | function gtrgm_out(gtrgm)
 pg_type      |    1247 | 18508 | type gtrgm[]
 pg_proc      |    1255 | 18509 | function gtrgm_consistent(internal,text,integer,oid,internal)
 pg_proc      |    1255 | 18510 | function gtrgm_compress(internal)
 pg_proc      |    1255 | 18511 | function gtrgm_decompress(internal)
 pg_proc      |    1255 | 18512 | function gtrgm_penalty(internal,internal,internal)
 pg_proc      |    1255 | 18513 | function gtrgm_picksplit(internal,internal)
 pg_proc      |    1255 | 18514 | function gtrgm_union(bytea,internal)
 pg_proc      |    1255 | 18515 | function gtrgm_same(gtrgm,gtrgm,internal)
 pg_opfamily  |    2753 | 18516 | operator family gist_trgm_ops for access method gist
 pg_opclass   |    2616 | 18517 | operator class gist_trgm_ops for access method gist
 pg_amop      |    2602 | 18518 | operator 1 %(text,text) of operator family gist_trgm_ops for access method gist
 pg_amproc    |    2603 | 18519 | function 1 gtrgm_consistent(internal,text,integer,oid,internal) of operator family gist_trgm_ops for access method gist
 pg_amproc    |    2603 | 18520 | function 2 gtrgm_union(bytea,internal) of operator family gist_trgm_ops for access method gist
 pg_amproc    |    2603 | 18521 | function 3 gtrgm_compress(internal) of operator family gist_trgm_ops for access method gist
 pg_amproc    |    2603 | 18522 | function 4 gtrgm_decompress(internal) of operator family gist_trgm_ops for access method gist
 pg_amproc    |    2603 | 18523 | function 5 gtrgm_penalty(internal,internal,internal) of operator family gist_trgm_ops for access method gist
 pg_amproc    |    2603 | 18524 | function 6 gtrgm_picksplit(internal,internal) of operator family gist_trgm_ops for access method gist
 pg_amproc    |    2603 | 18525 | function 7 gtrgm_same(gtrgm,gtrgm,internal) of operator family gist_trgm_ops for access method gist
 pg_proc      |    1255 | 18526 | function gin_extract_trgm(text,internal)
 pg_proc      |    1255 | 18527 | function gin_extract_trgm(text,internal,smallint,internal,internal)
 pg_proc      |    1255 | 18528 | function gin_trgm_consistent(internal,smallint,text,integer,internal,internal)
 pg_opfamily  |    2753 | 18529 | operator family gin_trgm_ops for access method gin
 pg_opclass   |    2616 | 18530 | operator class gin_trgm_ops for access method gin
 pg_amop      |    2602 | 18531 | operator 1 %(text,text) of operator family gin_trgm_ops for access method gin
 pg_amproc    |    2603 | 18532 | function 1 btint4cmp(integer,integer) of operator family gin_trgm_ops for access method gin
 pg_amproc    |    2603 | 18533 | function 2 gin_extract_trgm(text,internal) of operator family gin_trgm_ops for access method gin
 pg_amproc    |    2603 | 18534 | function 3 gin_extract_trgm(text,internal,smallint,internal,internal) of operator family gin_trgm_ops for access method gin
 pg_amproc    |    2603 | 18535 | function 4 gin_trgm_consistent(internal,smallint,text,integer,internal,internal) of operator family gin_trgm_ops for access method gin
(38 rows)
~~~


This function main intended users are the 
*extension* authors themselves, so
that it's easy for them to figure out which system identifier (the 
`objid`
column) has been attributed to some 
`SQL` objects from their install
script. With this knowledge, you can prepare some 
*upgrade* scripts. But
that's for another patch altogether, so we'll get back to the matter in
another blog entry.

So we chose 
[trgm](http://www.postgresql.org/docs/9.0/interactive/pgtrgm.html) as an example, let's follow the documentation and create a
test table and a custom index in there, just so that the extension is put to
good use. Then let's try to 
`DROP` our extension, because we're testing the
infrastructure, right?

~~~
exts=# create table test(id bigint, name text);
CREATE TABLE
exts=# CREATE INDEX idx_test_name ON test USING gist (name gist_trgm_ops);
CREATE INDEX
exts=# drop extension pg_trgm;
ERROR:  cannot drop extension pg_trgm because other objects depend on it
DETAIL:  index idx_test_name depends on operator class gist_trgm_ops for access method gist
HINT:  Use DROP ... CASCADE to drop the dependent objects too.
~~~


Of course PostgreSQL is smart enough here — the 
*extension* patch had nothing
special to do to achieve that, apart from recording the dependencies. Next,
as we didn't 
`drop extension pg_trgm cascade;`, it's still in the database. So
let's see what a 
`pg_dump` will look like. As it's quite a lot of text to
paste, let's see the 
`pg_restore` catalog instead. And that's a feature that
needs to be known some more, too.

~~~
dim ~ pg_dump -Fc exts | pg_restore -l | grep -v '^;'
1812; 1262 18497 DATABASE - exts dim
1; 3996 18498 EXTENSION - pg_trgm 
1813; 0 0 COMMENT - EXTENSION pg_trgm 
6; 2615 2200 SCHEMA - public dim
1814; 0 0 COMMENT - SCHEMA public dim
1815; 0 0 ACL - public dim
320; 2612 11602 PROCEDURAL LANGUAGE - plpgsql dim
1521; 1259 18543 TABLE public test dim
1809; 0 18543 TABLE DATA public test dim
1808; 1259 18549 INDEX public idx_test_name dim
~~~


As you see, the only SQL object that got into the backup are an 
`EXTENSION`
and its 
`COMMENT`. Nothing like the types or the functions that the 
`pg_trgm`
script creates.


## What does it means to extension authors?

In order to be an 
*extension*, you have to prepare a 
*control* file where to
give the necessary information to register your script. This file must be
named 
`extension.control` if the script is named 
`extension.sql`, at least at
the moment. This file can benefit from some variable expansion too, like
does the current 
`extension.sql.in`, in that if you provide an
`extension.control.in` file the term 
`VERSION` will be expanded to whatever
`$(VERSION)` is set to in your 
`Makefile`.

If you never wrote a 
`C` coded 
*extension* for PostgreSQL, this might look
complex and irrelevant. Baseline is that you need a 
`Makefile` so that you can
benefit easily from the PostgreSQL infrastructure work and have the 
`make
install` operation place your files at the right place, including the new
`control` file.


## That's it for today, folks

A next blog entry will detail what happens with extensions providing 
*user
data*, and the 
`CREATE EXTENSION name WITH NO DATA;` variant. Stay tuned!

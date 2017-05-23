+++
date = "2013-09-17T10:39:00.000000+02:00"
title = "PostgreSQL data recovery"
tags = ["PostgreSQL", "Recovery", "Backups"]
categories = ["PostgreSQL","Backups and Recovery"]
thumbnailImage = "/img/tazzine-barman-fondogrigio.png"
thumbnailImagePosition = "left"
coverImage = "/img/tazzine-barman-fondogrigio.png"
coverSize = "partial"
coverMeta = "out"
aliases = ["/blog/2013/09/16-PostgreSQL-data-recovery",
           "/blog/2013/09/16-PostgreSQL-data-recovery.html"]
+++

The following story is only interesting to read if you like it when bad
things happen, or if you don't have a trustworthy backup policy in place. By
trustworthy I mean that 
*each backup* you take 
**must** be tested with a test
recovery job. Only tested backups will prove useful when you need them. So
go read our 
[Backup and Restore](http://www.postgresql.org/docs/current/interactive/backup.html) documentation chapter then learn how to setup
[Barman](http://www.pgbarman.org/) for handling 
*physical backups* and 
[Point In Time Recovery](http://www.postgresql.org/docs/current/interactive/continuous-archiving.html). Get back
when you have proper backups, including recovery testing in place. We are
waiting for you. Back? Ok, let's see how bad you can end up without backups,
and how to still recover. With luck.


<div class="figure center dim-margin">
  <a href="http://www.pgbarman.org/">
    <img src="/img/old/pgbarman.png">
  </a>
</div>

*Set a trustworthy backup solution, and review your policy*

Did I mention that a 
*trustworthy* backup solution includes automated testing
of your ability to recover from any and all backup you've been taking? That
might be more important than you think.

> This article is going to be quite long. Prepare yourself a cup of your
> favorite beaverage. 



## The Setup

Most of the customers I visit have already laid out a 
*backup strategy* and
implemented it. Most of them did implement it with custom in-house scripts.
They hire high-skilled engineers who have been doing system administration
for more than a decade, and who are more than able to throw a 
*shell script*
at the problem.

*Shell scripting* must in hindsight be one of the most difficult things to do
right, given how many times it turns around doing something else entirely
than what the program author though it would. If you want another one of my
quite bulk advices, stop doing any 
*shell scripting* today: a 
*shell* is a nice
*interactive* tool, if you are doing non-interactive scripting, that's
actually called system programming and you deserve a better tool than that.


{{< image classes="fig50 center fancybox dim-margin" src="/img/old/a-shell-scripting-story.png" >}}


*My take: shell script makes it harder to write production quality code.*

In our very case, the customer did realize that a 
*production* setup had been
approved and was running live 
*before* any backup solution was in place. Think
about it for a minute. If you don't have 
***tested*** backups in place, it's not
production ready.

Well, the incident was taken seriously, and the usual backup 
*scripts*
deployed as soon as possible. Of course, the shell scripts depended in
non-obvious ways on some parameters (environment variables, database
connections, database setup with special configuration tables and rows). And
being a 
*shell script*, not much verification that the setup was complete had
been implemented, you see.


## The Horror Story

And guess what the first thing that 
*backup* script is doing? Of course,
making sure enough place is available on the file system to handle the next
backup. That's usually done by applying a 
*retention policy* and first
removing backups that are 
*too old* given said policy. And this script too did
exactly that.

The problem is that, as some of you already guessed (yes, I see those smiles
trying to hide brains thinking as fast as possible to decide if the same
thing could happen to you too), well, the script configuration had not been
done before entering production. So the script ran without setup, and
without much checking, began making bytes available. By removing any file
more than 5 days old. Right. In. 
`$PGDATA`.


## But recently modified are still there, right?

Exactly, not all the files of the database system had been removed. Surely
something can be done to recover data from a very small number of important
tables? Let's now switch to the present tense and see about it.


{{< image classes="fig50 center fancybox dim-margin" src="/img/old/data_loss.gif" >}}


*Can you spell ***data loss***?*

Remember, there's no 
*backups*. The 
`archive_command` is set though, so that's a
first track to consider. After that, what we can do is try to start
PostgreSQL on a copy of the remaining 
`$PGDATA` and massage it until it allows
us to 
`COPY` the data out.


## The desperate PITR

The 
*WAL Archive* is starting at the file 
`000000010000000000000009`, which
makes it unusable without a corresponding 
*base backup*, which we don't have.
Well, unless maybe if we tweak the system. We need to first edit the system
identifier, then reset the system to only begin replaying at the file we do
have. With some luck...


{{< image classes="fig50 center fancybox dim-margin" src="/img/old/a-broken-clock-is-right-twice-a-day-michael-flood.640.jpg" >}}


*A broken clock is still right twice a day, a broken backup never is...*

Time to try our luck here:

~~~
$ export PGDATA=/some/place
$ initdb
$ hexedit $PGDATA/global/pg_control
$ pg_controldata
$ xlogdump /archives/000000010000000000000009
$ pg_resetxlog -f -l 1,9,19 -x 2126 -o 16667 $PGDATA
$ cat > $PGDATA/recovery.conf <<EOF
restore_command = 'gunzip -c /archives/%f.gz > "%p"'
EOF
$ pg_ctl start
~~~


Using the transaction data we get from reading the first archive log file we
have with 
[xlogdump](https://github.com/snaga/xlogdump) then using 
[pg_resetxlog](http://www.postgresql.org/docs/current/static/app-pgresetxlog.html) and thus accepting to maybe lose
some more data, we still can't start the system in 
*archive recovery* mode,
because the 
*system identifier* is not the same in the WAL files and in the
system's 
`pg_controldata` output.

So we tweak our fresh cluster to match, by changing the first 8 bytes of the
control file, paying attention to the byte order here. As I already had a
Common Lisp 
*REPL* open on my laptop, the easier for me to switch from decimal
representation of the 
*database system identifier* was so:

~~~
(format nil "~,,' ,2:x" 5923145491842547187)
"52 33 3D 71 52 3B 3D F3"
~~~


Paying attention to the byte order means that you need to edit the control
file's first 8 bytes in reverse: 
`F3 3D 3B 52 71 3D 33 52`. But in our case,
no massaging seems to allow PostgreSQL to read from the archives we have.

On to massaging what is remaining in the old cluster then.


## The Promotion

I'm usually not doing promotion in such a prominent way, but I clearly
solved the situation thanks to my colleagues from the 24/7 support at
2ndQuadrant, with a special mention to 
***Andres Freund*** for inspiration and
tricks:


<div class="figure center dim-margin">
  <a href="http://2ndquadrant.com/">
    <img src="/img/old/2ndQuadrant-logo.640.jpg">
  </a>
</div>

*We also know how to recover your data, but we first insist in proper backups*

Oh, did I mention about proper backups and how you need to have been
successfully testing them before you can call a service 
*in production* or
have any hope about your recovery abilities? I wasn't sure I did...


## Playing fast and loose with PostgreSQL

The damaged cluster is not starting, for lack of important meta-data kind-of
files. First thing missing is 
`pg_filenode.map` in the 
`global` directory. Using
`xlogdump` it should be possible to recover just this file if it's been
changed in the WAL archives we have, but that's not the case.


{{< image classes="fig50 center fancybox dim-margin" src="/img/old/damage-case-bloat.640.jpg" >}}


*Trying to salvage a damage case*

### pg_filenode.map

As this file is only used for shared relations and some bootstraping
situation (you can't read current 
`pg_class` file node from 
`pg_class`, as the
file mapping is the information you need to know which file to read), and
knowing that the version on disk was older than 5 days on a cluster recently
put into production, we can allow ourselves trying something: copy the
`pg_filenode.map` from another fresh cluster.

My understanding is that this file only changes when doing heavy maintenance
on system tables, like 
`CLUSTER` or 
`VACUUM FULL`, which apparently didn't get
done here.

By the way, here's one of those tricks I learnt in this exercise. You can
read the second and fourth columns as filenames in the same directory:

~~~
od -j 8 -N $((512-8-8)) -td4 < $PGDATA/global/pg_filenode.map
~~~


So copying default 
`pg_filenode.map` allowed us to pass that error and get to
the next.


### pg_clog

Next is the lack of some 
`pg_clog` files. That's a little tricky because those
binary files contain the 
`commit log` information and are used to quickly
decide if recent transactions are still in flight, or committed already, or
have been rolled back. We can easily trick the system and declare that all
transaction older than 5 days (remember the bug in the 
*cleanup* script was
about that, right?) have in fact been 
*committed*. A commit in the 
`CLOG` is a
`01` value, and in a single byte we can stuff as many as 4 transactions'
status.

Here's how to create those file from scratch, once you've noticed that
`01010101` is in fact the ascii code for the letter 
`U`.

~~~
(code-char #b01010101)
#\U
~~~


So to create a series of clog file where all transactions have been
committed, so that we can see the data, we can use the following command
line:

~~~
for c in 0000 0001 0002 0003 0004 0005 0006 0007 0008 0009 000A 000B 000C
do
    dd if=/dev/zero bs=256k count=1 | tr '\0' 'U' > $c
done
~~~



{{< image classes="fig50 center fancybox dim-margin" src="/img/old/LRN-LNP-database.png" >}}



### pg_database

The next step we are confronted to is that PostgreSQL has lost its baking
files for the 
`pg_database` relation and has no idea what are those
directories in 
`$PGDATA/base` supposed to be all about. We only have the
numbers!

That said, the customer still had an history of the commands used to install
the database server, so knew in which order the databases where created. So
we had an OID to name mapping. How to apply it?

Well 
`pg_database` is a shared catalog and the underlying file apparently
isn't that easy to hack around, so the easiest solution is to actually hack
the 
`CREATE DATABASE` command and have it accepts a 
`WITH OIDS` option (
`OIDS` is
already a PostgreSQL keyword, 
`OID` is not, and we're not going to introduce
new keywords just for that particular patch).

Equiped with that 
***hacked version of PostgreSQL*** it's then possible to use
the new command and create the databases we need with the 
`OIDS` we know.

Those 
`OIDS` are then to be found on-disk in the file where 
`pg_database` is
internally stored, and we can ask the system where that file is:

~~~
select oid, relname, pg_relation_filenode(oid)
  from pg_class
 where relname = 'pg_database';
 oid  |   relname   | pg_relation_filenode 
------+-------------+----------------------
 1262 | pg_database |                12319
(1 row)
~~~


Then without surprise we can see:

~~~
$ strings $PGDATA/global/12319
postgres
template0
template1
~~~


Once that file is copied over to the (running, as it happened) damaged
cluster, it's then possible to actually open a connection to a database. And
that's pretty impressive. But suddenly it didn't work anymore...


### Sytem Indexes

This problem was fun to diagnose. The first 
`psql` call would be fine, but the
second one would always complain with an error you might have never seen in
the field. I sure didn't before.

~~~
FATAL:  database "dbname" does not exists
DETAIL:  Database OID 17838 now seems to belong to "otherdbname"
~~~


Part of PostgreSQL startup is building up some caches, and for that it's
using indexes. And we might have made a mistake, or the index is corrupted,
but apparently there's a mismatch somewhere.

But your now all-time favourite development team knew that would happen to
you and is very careful that any feature included in the software is able to
bootstrap itself without using any indexes. Or that in bad situations the
system knows how to resist the lack of those indexes by turning the feature
off, which is the case for 
[Event Triggers](http://www.postgresql.org/docs/9.3/interactive/event-triggers.html) for example, as you can see in the
commit 
[cd3413ec3683918c9cb9cfb39ae5b2c32f231e8b](http://git.postgresql.org/gitweb/?p=postgresql.git;a=commitdiff;h=cd3413ec3683918c9cb9cfb39ae5b2c32f231e8b).


{{< image classes="fig50 center fancybox dim-margin" src="/img/old/indexing-system.jpg" >}}


*Another kind of indexing system*

So yes, it is indeed possible to start 
[PostgreSQL](http://www.postgresql.org/) and have that marvellous
***production ready*** system avoid any system indexes, for dealing with cases
where you have reasons to think those are corrupted... or plain missing.

~~~
$ pg_ctl start -o "-P"
$ cat > $PGDATA/postgresql.conf <<EOF
	enable_indexscan = off
	enable_bitmapscan = off
	enable_indexonlyscan = off
EOF
$ pg_ctl reload
~~~


While at it, we edit the 
`postgresq.conf` and adjust some index usage related
settings, as you can see, because this problem will certain happen outside
of the system indexes.

> If you're not using (only) PostgreSQL as your database system of choice, now
> is the time to check that you can actually start those other systems when
> their internal indexes are corrupted or missing, by the way. I think that
> tells a lot about the readiness of the system for production usage, and the
> attitude of the developpers towards what happens in 


So we now have a running PostgreSQL service, servicing the data that still
is available. Well, not quite, We have a PostgreSQL service that accepts to
start and allows connections to a specific database.


### pg_proc, pg_operator, pg_cast, pg_aggregate, pg_amop and others

The first query I did try on the new database was against 
`pg_class` to get
details about the available tables. The 
`psql` command line tool is doing a
large number of queries in order to serve the 
`\d` output, the 
`\dt` one is
usable in our case.

To know what queries are sent to the server by 
[psql](http://www.postgresql.org/docs/current/interactive/app-psql.html) commands use the 
`\set
ECHO_HIDDEN` toggle.

About any query is now complaining that the target database is missing
files. To understand which file it is, I used the following query in a fresh
cluster. The following example is about an error message where
`base/16384/12062` is missing:

~~~
select oid, relname, pg_relation_filenode(oid)
  from pg_class
 where pg_relation_filenode(oid) = 12062;
 oid  | relname | pg_relation_filenode 
------+---------+----------------------
 1255 | pg_proc |                12062
(1 row)
~~~


In our specific case, no extensions were used. Check that before taking
action here, or at least make sure that the tables you want to try and
recover data from are not using extensions, that would make things so much
more complex.

Here we can just use default settings for most of the system catalogs: we
are using the same set of 
*functions*, 
*operators*, 
*casts*, 
*aggregates* etc as any
other 9.2 system, so we can directly use files created by 
[initdb](http://www.postgresql.org/docs/current/interactive/app-initdb.html) and copy
them over where the error message leads.


{{< image classes="fig50 center fancybox dim-margin" src="/img/old/namespace1.png" >}}



### pg_namespace

Some error messages are about things we should definitely not ignore. The
content of the 
`pg_namespace` relation was lost on about all our databases,
and the application here were using non default schema.

To recover from that situation, we need to better understand how this
relation is actually stored:

~~~
# select oid, * from pg_namespace;
  oid  |      nspname       | nspowner |        nspacl        
-------+--------------------+----------+----------------------
    99 | pg_toast           |       10 | 
 11222 | pg_temp_1          |       10 | 
 11223 | pg_toast_temp_1    |       10 | 
    11 | pg_catalog         |       10 | {dim=UC/dim,=U/dim}
  2200 | public             |       10 | {dim=UC/dim,=UC/dim}
 11755 | information_schema |       10 | {dim=UC/dim,=U/dim}
(6 rows)

# copy pg_namespace to stdout with oids;
99	pg_toast	10	\N
11222	pg_temp_1	10	\N
11223	pg_toast_temp_1	10	\N
11	pg_catalog	10	{dim=UC/dim,=U/dim}
2200	public	10	{dim=UC/dim,=UC/dim}
11755	information_schema	10	{dim=UC/dim,=U/dim}
~~~



{{< image classes="fig50 center fancybox dim-margin" src="/img/old/data_factory.jpg" >}}


So it's pretty easy here, actually, when you make the right connections:
let's import a default 
`pg_namespace` file then append to it thanks to 
`COPY
IN`, being quite careful about using 
`tabs` (well, unless you use the 
`delimiter`
option of course):

~~~
# copy pg_namespace from stdin with oids;
Enter data to be copied followed by a newline.
End with a backslash and a period on a line by itself.
>> 16443	my_namespace	10	\N
>> \.
~~~


And now there's a new 
*schema* in there with the 
`OID` we want. Wait, how do we
figure out the OID we need?

~~~
# select c.oid, relname, relnamespace, nspname
    from pg_class c left join pg_namespace n on n.oid = c.relnamespace
   where relname = 'bar';
  oid  | relname | relnamespace | nspname 
-------+---------+--------------+---------
 16446 | bar     |        16443 | 
(1 row)
~~~


So in the result of that query we have no 
`nspname`, but we happen to know
that the table 
`bar` is supposed to be in the schema 
`my_namespace`. 

And believe it or not, that method actually allows you to create a schema in
a database in a running cluster. We directly are editing the catalog files
and editing even the 
`OID` of the rows we are injecting.

The reason we couldn't do that with 
`pg_database`, if you're wondering about
that, is that 
`pg_database` is a shared catalog and part of the bootstrapping,
so that it was impossible to start PostgreSQL until we fix it, and the only
implementation of 
[COPY](http://www.postgresql.org/docs/current/interactive/sql-copy.html) we have requires a running PostgreSQL instance.


### pg_attributes and pg_attrdef

So now we are able to actually refer to the right relation in a SQL command,
we should be able to dump its content right? Well, it so happens that in
some case it's ok and in some cases it's not.

We are very lucky in that exercise in that 
`pg_attribute` is not missing. We
might have been able to rebuild it thanks to some 
`pg_upgrade` implementation
detail by forcing the 
`OID` of the next table to be created and then issuing
the right command, as given by 
[pg_dump](http://www.postgresql.org/docs/current/interactive/app-pgdump.html). By the way, did I mention about
backups? and automated recovery tests?


{{< image classes="fig50 center fancybox dim-margin" src="/img/old/relation-attributes.jpg" >}}


*We need the data attributes*

In some cases though, we are missing the 
`pg_attrdef` relation, wholesale.
That relation is used for default expressions attached to columns, as we can
see in the following example, taken on a working database server:

~~~
# \d a
                         Table "public.a"
 Column |  Type   |                   Modifiers                    
--------+---------+------------------------------------------------
 id     | integer | not null default nextval('a_id_seq'::regclass)
 f1     | text    | 
Indexes:
    "a_pkey" PRIMARY KEY, btree (id)

#  select adrelid, adnum, adsrc
     from pg_attrdef
    where adrelid = 'public.a'::regclass;
 adrelid | adnum |             adsrc             
---------+-------+-------------------------------
   16411 |     1 | nextval('a_id_seq'::regclass)
(1 row)

# select attnum, atthasdef
    from pg_attribute
   where     attrelid = 'public.a'::regclass
         and atthasdef;
 attnum | atthasdef 
--------+-----------
      1 | t
(1 row)
~~~


We need to remember that the goal here is to salvage some data out of an
installation where lots is missing, it's not at all about being able to ever
use that system again. Given that, what we can do here is just ignore the
default expression of the columns, by directly updating the catalogs:

~~~
# update pg_attribute
     set atthasdef = false
   where attrelid = 'my_namespace.bar';
~~~



### COPY the data out! now!

At this point we are now able to actually run the 
`COPY` command to store the
interesting data into a plain file, that is going to be usable on another
system for analysis.

Not every relation from the get go, mind you, sometime some 
*default* catalogs
are still missing, but in that instance of the data recovery we were able to
replace all the missing pieces of the puzzle by just copying the underlying
files as we did in the previous section.


## Conclusion

Really, 
[PostgreSQL](http://www.postgresql.org/) once again surprises me by its flexibility and
resilience. After having tried quite hard to kill it dead, it was still
possible to actually rebuild the 
*cluster* into shape piecemeal and get the
interesting data back.


{{< image classes="fig50 center fancybox dim-margin" src="/img/old/resilience_logo.jpg" >}}


I should mention, maybe, that with a proper 
*production setup* including a
[Continuous Archiving and Point-in-Time Recovery](http://www.postgresql.org/docs/current/interactive/continuous-archiving.html) solution such as 
[pgbarman](http://www.pgbarman.org/),
[walmgr](http://skytools.projects.pgfoundry.org/doc/walmgr.html), 
[OmniPITR](https://github.com/omniti-labs/omnipitr) or 
[PITRtools](https://public.commandprompt.com/projects/pitrtools/wiki); the recovery would have been really simple.

> Using an already made solution is often better because they don't just
> include 


It's even one of those rare cases where using 
[PostgreSQL replication](http://www.postgresql.org/docs/current/interactive/high-availability.html) would
have been a solution: the removing of the files did happen without
PostgreSQL involved, it didn't know that was happening and wouldn't have
replicated that to the standby.

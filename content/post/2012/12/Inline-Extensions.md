+++
date = "2012-12-13T11:34:00.000000+01:00"
title = "Inline Extensions"
tags = ["PostgreSQL", "Extensions", "9.3"]
categories = ["PostgreSQL","Extensions"]
thumbnailImage = "/img/old/dylibbundler.png"
thumbnailImagePosition = "left"
coverImage = "/img/old/dylibbundler.png"
coverSize = "partial"
coverMeta = "out"
aliases = ["/blog/2012/12/13-Inline-Extensions",
           "/blog/2012/12/13-Inline-Extensions.html"]
+++

We've been having the 
`CREATE EXTENSION` feature in 
[PostgreSQL](http://www.postgresql.org/) for a couple of
releases now, so let's talk about how to go from here. The first goal of the
extension facility has been to allow for a clean 
*dump* and 
*restore* process of
[contrib](http://www.postgresql.org/docs/9.2/static/contrib.html) modules. As such it's been tailored to the needs of deploying files
on the 
*file system* because there's no escaping from that when you have to
ship 
*binary* and 
*executable* files, those infamous 
`.so`, 
`.dll` or 
`.dylib` things.

Now that we have the 
*Extension* facility though, what we see is a growing
number of users taking advantage of it for the purpose of managing in house
procedural code and related objects. This code can be a bunch of 
[PLpgSQL](http://www.postgresql.org/docs/9.2/static/plpgsql.html) or
[plpython](http://www.postgresql.org/docs/9.2/static/plpython.html) functions and as such you normaly create them directly from any
application connection to PostgreSQL.

So the idea would be to allow creating 
*Extensions* fully from a SQL command,
including the whole set of objects it contains. More than one approach are
possible to reach that goal, each with downsides and advantages. We will see
them later in that document.

Before that though, let's first review what the extension mechanism has to
offer to its users when there's no 
*contrib like* module to manage.


## A use case for next generation extensions

The only design goal of the 
`9.1` PostgreSQL Extension feature has been to
support a proper 
*dump & restore* user experience when using 
*contrib modules*
such as 
`hstore` or 
`ltree`. Building up on that, what do 
*Extensions* have to
offer to non 
`C` developpers out there? In other words, what 
`CREATE EXTENSION`
brings on the table that a bunch of 
*loose* objects does not? What problems
can we now solve?

<center>
{{< image classes="fig50 fancybox dim-margin" src="/img/old/multi_function_equipment.jpg" >}}
</center>

<center>*A Multi Functions Equipment, All Bundled Together*</center>

A way to phrase it is to say that 
*Extensions* are user defined 
`CASCADE`
support. 
*Extensions* brings extensibility to the 
`pg_depend` PostgreSQL
internal dependency tracking system that 
`CASCADE` is built on. From that
angle, 
*Extensions* are a way to manage dependencies of 
*SQL objects* in a way
that allow you to manage them as a single entity.

One of the existing problems this helps solving is the infamous lack of
dependency tracking between function calls. Using 
*Extensions* when you deal
with a set of functions acting as an API, you can at least protect that as a
unit:

~~~
STATEMENT: drop function public.populate_record(anyelement,hstore);
    ERROR: cannot drop function populate_record(anyelement,hstore) because
           extension hstore requires it
     HINT: You can drop extension hstore instead.
~~~


And you also have a version number and tools integration to manage
extensions, with psql 
`\dx` command and the equivalent feature in 
[pgAdmin](http://www.pgadmin.org/).
Coming with your own version number management is not impossible, some do
that already. Here it's integrated and the upgrade sequences are offered too
(applying 
`1.1--1.2` then 
`1.2--1.3` automatically).

Let's just say that it's very easy to understand the 
*traction* our users feel
towards leveraging 
*Extensions* features in order to properly manage their set
of stored procedures and SQL objects.


## The *dump & restore* experience

The common problem of all those proposals is very central to the whole idea
of 
*Extensions* as we know them. The goal of building them as been to fix the
*restoring* experience when using extensions in a database, and we managed to
do that properly for contrib likes extensions.

<center>
{{< image classes="fig50 fancybox dim-margin" src="/img/old/fly.tn.png" >}}
</center>

<center>*A fly in the ointment*</center>

When talking about 
*Inline Extensions*, the fly in the ointment is how to
properly manage their 
`pg_dump` behavior. The principle we built for
*Extensions* and that is almost unique to them is to 
***omit*** them in the dump
files. The only other objects that we filter out of the dump are the one
installed at server initialisation times, when using 
[initdb](http://www.postgresql.org/docs/9.2/static/app-initdb.html), to be found in
the 
`pg_catalog` and 
`information_schema` systems' 
*schema*.

At restore time, the dump file contains the 
`CREATE EXTENSION` command so the
PostgreSQL server will go fetch the 
*control* and 
*script* files on disk and
process them, loading the database with the right set of SQL objects.

Now we're talking about 
*Extensions* which we would maybe want to dump the
objects of, so that at 
*restore* time we don't need to find them from unknown
external resources: the fact that the extension is 
*Inline* means that the
PostgreSQL server has no way to know where its content is coming from.

The next proposals are trying to address that problem, with more or less
success. So far none of them is entirely sastisfying to me, even if a clear
temporary winner as emerged on the 
*hackers* mailing list, summarized in the
[in-catalog Extension Scripts and Control parameters (templates?)](http://archives.postgresql.org/message-id/m2fw3judug.fsf@2ndQuadrant.fr) thread.


## Inline Extension Proposals

Now, on to some proposals to make the best out of our all time favorite
PostgreSQL feature, the only one that makes no sense at all by itself...


### Starting from an empty extension

We already have the facility to add existing 
*loose* objects to an extension,
and that's exactly what we use when we create an extension for the first
time when it used not to be an extension before, with the 
`CREATE EXTENSION
... FROM 'unpackaged';` command.

The 
`hstore--unpackaged--1.0.sql` file contains statements such as:

~~~
ALTER EXTENSION hstore ADD type hstore;
ALTER EXTENSION hstore ADD function hstore_in(cstring);
ALTER EXTENSION hstore ADD function hstore_out(hstore);
ALTER EXTENSION hstore ADD function hstore_recv(internal);
ALTER EXTENSION hstore ADD function hstore_send(hstore);
~~~


Opening 
`CREATE EXTENSION` so that it allows you to create a really 
*empty*
extension would then allow you to fill-in as you need, with as many commands
as you want to add objects to it. The 
*control* file properties would need to
find their way in that design, that sure can be taken care of.

<center>
{{< image classes="fig50 fancybox dim-margin" src="/img/old/empty-extension.jpg" >}}
</center>

<center>*Look me, an Empty Extension!*</center>

The main drawback here is that there's no separation anymore in between the
extension author, the distribution means, the DBA and the database user.
When you want to install a third party 
*Extension* using only SQL commands,
you could do it with that scheme by using a big script full of one-liners
commands.

So that if you screw up your 
*copy/pasting* session (well you should maybe
reconsider your choice of tooling at this point, but that's another topic),
you will end up with a perfectly valid 
*Extension* that does not contain what
you wanted. As the end user, you have no clue about that until the first
time using the extension fails.


### CREATE EXTENSION AS

The next idea is to embed the 
*Extension* script itself in the command, so as
to to get a cleaner command API (in my opinion at least) and a better error
message when the paste is wrong. Of course it your 
*paste* problem happens to
just be loosing a line in the middle of the script there is not so much I
can do for you...

~~~
CREATE EXTENSION hstore
  WITH parameter = value, ...
AS $$
CREATE TYPE hstore;

CREATE FUNCTION hstore_in(cstring) RETURNS hstore
 AS 'MODULE_PATHNAME' LANGUAGE C STRICT IMMUTABLE;

CREATE FUNCTION hstore_out(hstore) RETURNS cstring
AS 'MODULE_PATHNAME' LANGUAGE C STRICT IMMUTABLE;

CREATE FUNCTION hstore_recv(internal) RETURNS hstore
AS 'MODULE_PATHNAME' LANGUAGE C STRICT IMMUTABLE;

CREATE FUNCTION hstore_send(hstore) RETURNS bytea
AS 'MODULE_PATHNAME' LANGUAGE C STRICT IMMUTABLE;

CREATE TYPE hstore (
        INTERNALLENGTH = -1, STORAGE = extended
        INPUT = hstore_in, OUTPUT = hstore_out,
        RECEIVE = hstore_recv, SEND = hstore_send);
$$;
~~~


<center>*An edited version of `hstore--1.1.sql` for vertical space concerns*</center>

I've actually proposed a patch to implement that, as you can see in the
[pg_dump --extension-script](https://commitfest.postgresql.org/action/patch_view?id=981) commit fest entry. As spoiled by the commit fest
entry title, the main problem we have with 
*Inline Extensions* is their
management in the seamless experience of 
*dump & restore* that we are so happy
to have now. More about that later, though.


### Extension Templates

Another idea is to continue working from control parameters and scripts to
install and update extensions, but to have two different places where to
find those. Either on the server's 
*File System* (when dealing with 
*contribs*
and 
*shared libraries*, there's but a choice), or on the system catalogs.

<center>
{{< image classes="fig50 fancybox dim-margin" src="/img/old/templates.png" >}}
</center>

<center>*We Already Have `TEXT SEARCH TEMPLATE` After All*</center>

The idea would then be to have some new specific 
`TEMPLATE` SQL Object that
would be used to 
*import* or 
*upload* your control file and create and update
scripts in the database, using nothing else than a SQL connection. Then at
`CREATE EXTENSION` time the system would be able to work either from the file
system or the 
*template* catalogs.

One obvious problem is how to deal with a unique namespace when we split the
sources into the file system and the database, and when the file system is
typically maintained by using 
`apt-get` or 
`yum` commands.

Then again I would actually prefer that mechanism better than the other
proposals if the idea was to load the file system control and scripts files
as 
`TEMPLATEs` themselves and then only operate 
*Extensions* from 
*Templates*. But
doing that would mean getting back to the situation where we still are not
able to devise a good, simple and robust 
`pg_dump` policy for extensions and
templates.


## Conclusion

I hope to be finding the right solution to my long term plan in this release
development cycle, but it looks like the right challenge to address now is
to find the right compromise instead. Using the 
*Templates* idea already
brings a lot on the table, if not the whole set of features I would like to
see.

<center>
{{< image classes="fig50 fancybox dim-margin" src="/img/old/building-blocks.jpg" >}}
</center>

<center>*PostgreSQL: Building on Solid Foundations*</center>

What would be missing mainly would be the ability for an 
*Extension* to switch
from being file based to being a template, either because the author decided
to change the way he's shipping it, or because the user is switching from
using the 
[pgxn client](http://pgxnclient.projects.pgfoundry.org/) to using 
*proper* system packages. I guess that's
something we can see about later, though.

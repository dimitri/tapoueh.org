+++
date = "2012-03-08T14:25:00.000000+01:00"
title = "Extension White Listing"
tags = ["PostgreSQL", "Extensions", "pgextwlist"]
categories = ["PostgreSQL","Extensions"]
thumbnailImage = "/img/postgresql-512.jpg"
thumbnailImagePosition = "left"
coverImage = "/img/postgresql-512.jpg"
coverSize = "partial"
coverMeta = "out"
aliases = ["/blog/2012/03/08-extension-white-listing",
           "/blog/2012/03/08-extension-white-listing.html"]
+++

PostgreSQL 9.1 includes proper extension support, as you might well know if
you ever read this very blog here. Some hosting facilities are playing with
PostgreSQL at big scale (hello 
[Heroku](https://postgres.heroku.com/blog)!) and still meet with small caveats
making their life uneasy.

To be specific, only 
*superusers* are allowed to install C coded stored
procedures, and that impacts a lot of very useful PostgreSQL extension: all
those shiped in the 
*contrib* package are coded in C. Now, 
[Heroku](https://postgres.heroku.com/blog) is not
giving away 
*superuser* access to their hosted customers in order to limit the
number of ways they can shoot themselves in the foot. And given PostgreSQL
security model, being granted 
*database owner* is mostly good enough for day
to day operation.

>  See Andrew's article 


Mostly, but as we see, not completely good enough. How to arrange for a non
*superuser* to be able to still install a C-coded extension in his own
database? That's quite dangerous as any bug causing a crash would mean a
PostgreSQL whole restart. So you not only want to empower 
`CREATE EXTENSION`
to database owners, you also want to be able to review and explicitely 
*white
list* the allowed extensions.

Here we go: 
[pgextwlist](https://github.com/dimitri/pgextwlist) is a PostgreSQL extensions implementing just that
idea. You have to tweak 
`local_preload_libraries` so that it gets loaded
automatically and early enough, and you have to provide for the list of
authorized extensions in the 
`extwlist.extensions` setting.

Let's see a usage example, straight from the documentation:

~~~
dim=> select rolsuper from pg_roles where rolname = current_user;
select rolsuper from pg_roles where rolname = current_user;
 rolsuper
----------
 f
(1 row)

dim=> create extension hstore;
create extension hstore;
WARNING:  => is deprecated as an operator name
DETAIL:  This name may be disallowed altogether in future versions of PostgreSQL.
CREATE EXTENSION

dim=> create extension earthdistance;
create extension earthdistance;
ERROR:  extension "earthdistance" is not whitelisted
DETAIL: Installing the extension "earthdistance" failed, because it is not
        on the whitelist of user-installable extensions.
HINT: Your system administrator has allowed users to install certain
      extensions. SHOW extwlist.extensions;

dim=> \dx
\dx
                           List of installed extensions
  Name   | Version |   Schema   |                   Description
---------+---------+------------+--------------------------------------------------
 hstore  | 1.0     | public     | data type for storing sets of (key, value) pairs
 plpgsql | 1.0     | pg_catalog | PL/pgSQL procedural language
(2 rows)

dim=> drop extension hstore;
drop extension hstore;
DROP EXTENSION
~~~


As you can see, it allows non 
*superusers* to install an extension written in C.

+++
date = "2009-11-25T11:49:00.000000+01:00"
title = "Yet Another PostgreSQL tool hits debian"
tags = ["debian", "release", "backup", "restore", "pg_staging", "9.1"]
categories = ["Projects","pg_staging"]
thumbnailImage = "/img/old/debian-logo.png"
thumbnailImagePosition = "left"
coverImage = "/img/old/debian-logo.png"
coverSize = "partial"
coverMeta = "out"
aliases = ["/blog/2009/11/25-yet-another-postgresql-tool-hits-debian",
           "/blog/2009/11/25-yet-another-postgresql-tool-hits-debian.html"]
+++

So there it is, this newer contribution of mine that I presented at 
[PGDay](http://2009.pgday.eu) is
now in 
`debian NEW` queue. 
[pg_staging](pgstaging.html) will empower you with respect to what
you do about those nightly backups (
`pg_dump -Fc` or something).

The tool provides a lot of commands to either 
`dump` or 
`restore` a database. It
comes with documentation covering about it all, except for the 
*londiste*
support part, which will be there in time for 
`1.0.0` release. The 
[Todo list](http://github.com/dimitri/pg_staging/blob/master/TODO)
is getting smaller and smaller, the version you'll soon find in 
`debian sid`
is already called 
`0.9`.

So, how do you go about using this software, and what service it implements?


## it's all about deriving a staging environment from your backups

To validate backups, you want to restore them and check the database you get
from them. And your developers will want to sometime refresh the database
they're working with. And you could have both an integration environment and
a pre-live one: On the former, you develop new code atop a stable set of
data; while on the latter you test stable enough code (ready to go live) on
a set of data as near as live data as possible.

And you want to be flexible about it, so that there's not a fulltime job to
handle retoring databases each and every days, for project A integration or
project B pre-live testing, or project C accounting snapshot. Or you name
it.

And of course you want to have a single point of control of all your
databases. Let's call it the 
*controler*.


## setting up pg_staging

The 
[pg_staging](pgstaging.html) setup consists of one 
`pg_staging.ini` file wherein you
describe your different target databases (those 
`dev` and 
`prelive` ones), and
of course where to get the production backups from. Currently you have to
serve the backups file in a format suitable for 
`pg_restore` (that means you
use either 
`pg_dump -Ft` or 
`pg_dump -Fc`) on an 
`apache` folder. The produced
`HTML` will get parsed.

So you setup the 
`DEFAULT` section with common settings, then one section per
target: the databases you want to restore. Tell 
`pg_staging` where they are
(
`host`), etc, and it'll be able to drive them.

In order to being able to host more than a single restored dump on a staging
server, for the same database, we use 
`pgbouncer`:

~~~
pg_staging> pgbouncer some_db.dev
              some_db      some_db_20091029 :5432
     some_db_20090717      some_db_20090717 :5432
     some_db_20091029      some_db_20091029 :5432
~~~


So as explained into the 
`pg_staging(1)` man page, you have to open
non-interactive 
`SSH` connection from the 
*controler* to the 
*hosts* where the
databases will get restored. Then you have to do a minimal setup pgbouncer
on the 
*hosts* with a 
`trust` connection. It'll get used from 
`pg_staging` for
adding newly restored database and have them accessible. Then you can also
`switch` the new database to being the virtual 
*some_db* so that you avoid
editing any connection string on your softwares.

Also, install the 
`pgstaging-client` package on every host you target. The
client is a simple shell script that must run as root (
`sudo` is used) in
order to replace your 
`pgbouncer` setup or manage your 
`londiste` services.

See 
`man 5 pg_staging` for available options, including 
*schemas* to filter out
either completely or just skipping data restoring in those.


## pg_staging usage

Now you're all setup, you can begin to enjoy using 
`pgstaging`. Enter the
console and see what you have in there.

~~~
$ pg_staging 
Welcome to pg_staging 0.9.
pg_staging> databases
...
pg_staging> restore some_db.dev
...
pg_staging> pgbouncer some_db.dev
...
pg_staging> dbsizes --all some_db.dev
...
pg_staging> psql some_db.dev
some_db_20091125=# 
~~~


And as you can see in 
`man pg_staging` there are a lot of commands
already. You can for example obtain a new 
*pg_restore catalog* from a dump
file, with some 
*schemas* commented out. It will even comment out 
`triggers`
that are using a 
`function` which is defined in a filtered out 
`schema`, for
example a 
`PGQ` trigger. And much much more.

[pg_staging](pgstaging.html) will even allow you to 
`dump` your production databases, but
consider installing a separate instance of it on the machine serving the
backups to your local network thanks to an 
`apache` directory listing!


## Roadmap to `1.0.0`

What's remain to be done is testing and having 
`PITR` based restoring to work,
and adding some documentation (tutorial, which this blog post about is; and
*londiste* support). At this point, unless some reader here asks for a new
feature (set), I'll consider 
`pg_staging` ready for 
`1.0.0`. After all, we're
using it about daily here :)

Consider commenting, you should be able to easily spot my private mail
address...

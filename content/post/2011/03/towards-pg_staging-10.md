+++
date = "2011-03-29T15:30:00.000000+02:00"
title = "towards pg_staging 1.0"
tags = ["PostgreSQL", "skytools", "backup", "restore", "pg_staging"]
categories = ["Projects","pg_staging"]
thumbnailImage = "/img/old/pg_staging.png"
thumbnailImagePosition = "left"
coverImage = "/img/old/pg_staging.png"
coverSize = "partial"
coverMeta = "out"
aliases = ["/blog/2011/03/29-towards-pg_staging-10",
           "/blog/2011/03/29-towards-pg_staging-10.html"]
+++

If you don't remember about what 
[pg_staging](pgstaging.html) is all about, it's a central
console from where to control all your 
[PostgreSQL](http://www.postgresql.org/) databases.  Typically you
use it to manage your development and pre-production setup, where developers
ask you pretty often to install them some newer dump from the production,
and you want that operation streamlined and easy.



## Usage 

The typical session would be something like this:

~~~
pg_staging> databases foodb.dev
                    foodb      foodb_20100824 :5432
           foodb_20100209      foodb_20100209 :5432
           foodb_20100824      foodb_20100824 :5432
                pgbouncer           pgbouncer :6432
                 postgres            postgres :5432

pg_staging> dbsizes foodb.dev
foodb.dev
           foodb_20100209: -1
           foodb_20100824: 104 GB
Total                    = 104 GB

pg_staging> restore foodb.dev
...
pg_staging> switch foodb.dev today
~~~


The list of supported commands is quite long now, and documented too (it
comes with two man pages).  The 
`restore` one is the most important and will
create the database, add it to the 
`pgbouncer` setup, fetch the backup named
`dbname.`date -I`.dump`, prepare a filtered object list (more on that), load
*pre* 
`SQL` scripts, launch 
`pg_restore`, 
`VACUUM ANALYZE` the database when
configured to do so, load the 
*post* 
`SQL` scripts then optionaly 
*switch* the
`pgbouncer` setup to default to this new database.


## Filtering

The newer option is called 
`tablename_nodata_regexp`, and here's its documentation in full:

>      List of table names regexp (comma separated) to restore without
>      content. The 


This comes to supplement the 
`schemas` and 
`schemas_nodata` options, that allows
to only restore objects from a given set of 
*schemas* (filtering out triggers
that will calls function that are in the excluded schemas, like
e.g. 
[Londiste](http://wiki.postgresql.org/wiki/Skytools) ones) or to restore only the 
`TABLE` definitions while skipping
the 
`TABLE DATA` entries.


## Setup

To setup your environment for 
*pg_staging*, you need to take some steps.  It's
not complex but it's fairly involved.  The benefit is this amazingly useful
central unique console to control as many databases as you need.

You need a 
`pg_staging.ini` file where to describe your environment.  I
typically name the sessions in there by the name of the database to restore
followed by a 
`dev` or 
`preprod` extension.

You need to have all your backups available through 
`HTTP`, and as of now,
served by the famous 
*apache* 
`mod_dir` directory listing.  It's easy to add
support to other methods, but is has not been done yet.  You also need to
have a cluster wide 
`--globals-only` backup available somewhere so that you
can easily create the users etc you need from 
`pg_staging`.

You also need to run a 
`pgbouncer` daemon on each database server, allowing
you to bypass editing connection strings when you 
`switch` a new database
version live.

You also need to install the 
*client* script, have a local 
`pgstaging` system
user and allow it to run the client script as root, so that it's able to
control some services and edit 
`pgbouncer.ini` for you.


## Status

I'm still using it a lot (several times a week) to manage a whole
development and pre-production environment set, so the very low
[code activity](https://github.com/dimitri/pg_staging) of the project is telling that it's pretty stable (last series
of 
*commits* are all bug fixes and round corners).

Given that, I'm thinking in terms of 
`pg_staging 1.0` soon!  Now is a pretty
good time to try it and see how it can help you.

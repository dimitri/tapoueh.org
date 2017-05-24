+++
date = "2015-01-16T09:35:00.000000+01:00"
title = "New release: pgloader 3.2"
tags = ["PostgreSQL", "Common-Lisp"]
categories = ["Software Programming","Common Lisp"]
thumbnailImage = "/img/old/keep-calm-and-on-error-resume-next.png"
thumbnailImagePosition = "left"
coverImage = "/img/old/keep-calm-and-on-error-resume-next.png"
coverSize = "partial"
coverMeta = "out"
aliases = ["/blog/2015/01/16-pgloader-3-2",
           "/blog/2015/01/16-pgloader-3-2.html"]
+++

PostgreSQL comes with an awesome bulk copy protocol and tooling best known
as the 
[COPY](http://www.postgresql.org/docs/current/static/sql-copy.html) and 
`\copy` commands. Being a transactional system, PostgreSQL
COPY implementation will 
`ROLLBACK` any work done if a single error is found
in the data set you're importing. That's the reason why 
[pgloader](http://pgloader.io/) got
started: it provides with error handling for the 
[COPY protocol](http://www.postgresql.org/docs/9.3/static/protocol-flow.html#PROTOCOL-COPY).

<!--more-->

As soon as we have the capability to load data from unreliable sources,
another use case appears on the horizon, and soon enough 
[pgloader](http://pgloader.io/) grew the
capacity to load data from other databases, some having a more liberal
notion of what is sane data type input.

To be able to adapt to advanced use cases in database data migration
support, pgloader has grown an advanced command language wherein you can
define your own load-time data projection and transformations, and your own
type casting rules too.

New in 
*version 3.2* is that in simple cases, you don't need that command file
any more. Check out the 
[pgloader quick start](http://pgloader.io/howto/quickstart.html) page to see some examples where
you can use pgloader all from your command line!

Here's one such example, migrating a whole MySQL database data set over to
PostgreSQL, including automated schema discovery, automated type casting and
on-the-fly data cleanup (think about zero dates or booleans in 
`tinyint(1)`
disguise), support for indexes, primary keys, foreign keys and comments.
It's as simple as:

~~~
$ createdb sakila
$ pgloader mysql://root@localhost/sakila pgsql:///sakila
2015-01-16T09:49:36.068000+01:00 LOG Main logs in '/private/tmp/pgloader/pgloader.log'
2015-01-16T09:49:36.074000+01:00 LOG Data errors in '/private/tmp/pgloader/'
                    table name       read   imported     errors            time
------------------------------  ---------  ---------  ---------  --------------
               fetch meta data         43         43          0          0.222s
                  create, drop          0         36          0          0.130s
------------------------------  ---------  ---------  ---------  --------------
                         actor        200        200          0          0.133s
                       address        603        603          0          0.035s
                      category         16         16          0          0.027s
                          city        600        600          0          0.018s
                       country        109        109          0          0.017s
                      customer        599        599          0          0.035s
                          film       1000       1000          0          0.075s
                    film_actor       5462       5462          0          0.147s
                 film_category       1000       1000          0          0.035s
                     film_text       1000       1000          0          0.053s
                     inventory       4581       4581          0          0.086s
                      language          6          6          0          0.041s
                       payment      16049      16049          0          0.436s
                        rental      16044      16044          0          0.474s
                         staff          2          2          0          0.170s
                         store          2          2          0          0.010s
        Index Build Completion          0          0          0          0.000s
------------------------------  ---------  ---------  ---------  --------------
                Create Indexes         40         40          0          0.343s
               Reset Sequences          0         13          0          0.026s
                  Primary Keys         16         14          2          0.013s
                  Foreign Keys         22         22          0          0.078s
                      Comments          0          0          0          0.000s
------------------------------  ---------  ---------  ---------  --------------
             Total import time      47273      47273          0          2.261s
~~~


Other options are available to support a variety of input file formats,
including compressed csv files found on a remote location, as in:

~~~
curl http://pgsql.tapoueh.org/temp/2013_Gaz_113CDs_national.txt.gz \
    | gunzip -c                                                        \
    | pgloader --type csv                                              \
               --field "usps,geoid,aland,awater,aland_sqmi,awater_sqmi,intptlat,intptlong" \
               --with "skip header = 1"                                \
               --with "fields terminated by '\t'"                      \
               -                                                       \
               postgresql:///pgloader?districts_longlat

2015-01-16T10:09:06.027000+01:00 LOG Main logs in '/private/tmp/pgloader/pgloader.log'
2015-01-16T10:09:06.032000+01:00 LOG Data errors in '/private/tmp/pgloader/'
                    table name       read   imported     errors            time
------------------------------  ---------  ---------  ---------  --------------
                         fetch          0          0          0          0.010s
------------------------------  ---------  ---------  ---------  --------------
             districts_longlat        440        440          0          0.087s
------------------------------  ---------  ---------  ---------  --------------
             Total import time        440        440          0          0.097s
~~~


As usual in unix commands, the 
`-` input filename stands for 
*standard input*
and allows streaming data from a remote compressed file down to PostgreSQL.

So if you have any data loading job, including data migrations from SQLite,
MySQL or MS SQL server: have a look at 
[pgloader](http://pgloader.io/)!

+++
date = "2010-04-27T12:01:00.000000+02:00"
title = "Import fixed width data with pgloader"
tags = ["PostgreSQL", "release", "pgloader", "9.1"]
categories = ["Projects","pgloader"]
thumbnailImage = "/img/old/toy-loader.320.jpg"
thumbnailImagePosition = "left"
coverImage = "/img/old/toy-loader.320.jpg"
coverSize = "partial"
coverMeta = "out"
aliases = ["/blog/2010/04/27-import-fixed-width-data-with-pgloader",
           "/blog/2010/04/27-import-fixed-width-data-with-pgloader.html"]
+++

So, following previous blog entries about importing 
*fixed width* data, from
[Postgres Online Journal](http://www.postgresonline.com/journal/index.php?/archives/157-Import-fixed-width-data-into-PostgreSQL-with-just-PSQL.html) and 
[David (perl) Fetter](http://people.planetpostgresql.org/dfetter/index.php?/archives/58-psql,-Paste,-Perl-Pefficiency!.html), I couldn't resist following
the meme and showing how to achieve the same thing with 
[pgloader](http://pgloader.projects.postgresql.org/#toc9).

<!--more-->

{{< alert success >}}

A companion article using more recent software is available
at
[Import fixed width data with pgloader](/blog/2013/11/import-fixed-width-data-with-pgloader),
check it out!

{{< /alert >}}

I can't say how much I dislike such things as the following, and I can't
help thinking that non IT people are right looking at us like this when
encountering such prose.

~~~ perl
map {s/\D*(\d+)-(\d+).*/$a.="A".(1+$2-$1). " "/e} split(/\n/,<<'EOT');
~~~


So, the 
*pgloader* way. First you need to have setup a database, I called it
`pgloader` here. Then you need the same 
`CREATE TABLE` as on the original
article, here is it for completeness:

~~~ sql
CREATE TABLE places(usps char(2) NOT NULL,
    fips char(2) NOT NULL, 
    fips_code char(5),
    loc_name varchar(64));
~~~


Now the data file I've taken here:
[http://www.census.gov/tiger/tms/gazetteer/places2k.txt](http://www.census.gov/tiger/tms/gazetteer/places2k.txt).

{{< alert danger >}}

This article is about versions 2.x of pgloader, which are not supported
anymore. Consider using [pgloader](http://pgloader.io) version 3.x instead.
Alos the following example is still available in the 3.x series and you can
see the *command file* at the GitHub repository for
pgloader:

<https://github.com/dimitri/pgloader/blob/master/test/census-places.load>.

{{< /alert >}}


Then we translate the file description into 
*pgloader* setup:

~~~ ini
[pgsql]
host = localhost
port = 5432
base = pgloader
user = dim
pass = None

log_file            = /tmp/pgloader.log
log_min_messages    = DEBUG
client_min_messages = WARNING

client_encoding = 'latin1'
lc_messages         = C
pg_option_standard_conforming_strings = on

[fixed]
table           = places
format          = fixed
filename        = places2k.txt
columns         = *
fixed_specs     = usps:0:2, fips:2:2, fips_code:4:5, loc_name:9:64, p:73:9, h:82:9, land:91:14, water:105:14, ldm:119:14, wtm:131:14, lat:143:10, long:153:11
~~~


We're ready to import the data now:

~~~ bash
dim ~/PostgreSQL/examples pgloader -vsTc pgloader.conf 
pgloader     INFO     Logger initialized
pgloader     WARNING  path entry '/usr/share/python-support/pgloader/reformat' does not exists, ignored
pgloader     INFO     Reformat path is []
pgloader     INFO     Will consider following sections:
pgloader     INFO       fixed
pgloader     INFO     Will load 1 section at a time
fixed        INFO     columns = *, got [('usps', 1), ('fips', 2), ('fips_code', 3), ('loc_name', 4)]
fixed        INFO     Loading threads: 1
fixed        INFO     closing current database connection
fixed        INFO     fixed processing
fixed        INFO     TRUNCATE TABLE places;
pgloader     INFO     All threads are started, wait for them to terminate
fixed        INFO     COPY 1: 10000 rows copied in 5.769s
fixed        INFO     COPY 2: 10000 rows copied in 5.904s
fixed        INFO     COPY 3: 5375 rows copied in 3.187s
fixed        INFO     No data were rejected
fixed        INFO      25375 rows copied in 3 commits took 14.907 seconds
fixed        INFO     No database error occured
fixed        INFO     closing current database connection
fixed        INFO     releasing fixed semaphore
fixed        INFO     Announce it's over

Table name        |    duration |    size |  copy rows |     errors 
====================================================================
fixed             |     14.901s |       - |      25375 |          0
~~~


Note the 
`-T` option is for 
`TRUNCATE`, which you only need when you want to
redo the loading, I've come to always mention it in interactive usage. The
`-v` option is for some more 
*verbosity* and the 
`-s` for the 
*summary* at end of
operations.

With the 
`pgloader.conf` and 
`places2k.txt` in the current directory, and an
empty table, just typing in 
`pgloader` at the prompt would have done the job.

Oh, the 
`pg_option_standard_conforming_strings` bit is from the 
[git HEAD](http://github.com/dimitri/pgloader), the
current released version has no support for setting any PostgreSQL knob
yet. Still, it's not necessary here, so you can forget about it.

You will also notice that 
*pgloader* didn't trim the data for you, which ain't
funny for the 
*places* column. That's a drawback of the fixed width format
that you can work on two ways here, either by means of 

~~~ sql
UPDATE places SET loc_name = trim(loc_name);
~~~


 or a custom
reformat module for 
*pgloader*. I guess the latter solution is overkill, but
it allows for 
*pipe* style processing of the data and a single database write.

Send me a mail if you want me to show here how to setup such a reformatting
module in a next blog entry!

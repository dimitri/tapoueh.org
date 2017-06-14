+++
date = "2013-11-18T12:48:00.000000+01:00"
title = "Import fixed width data with pgloader"
tags = ["PostgreSQL", "pgloader", "Common-Lisp"]
categories = ["Projects","pgloader"]
thumbnailImage = "/img/old/fixed_text_read.png"
thumbnailImagePosition = "left"
coverImage = "/img/old/fixed_text_read.png"
coverSize = "partial"
coverMeta = "out"
aliases = ["/blog/2013/11/18-pgloader-fixed-width-data",
           "/blog/2013/11/18-pgloader-fixed-width-data.html"]
+++

A long time ago we talked about how to
[Import fixed width data with pgloader](/blog/2010/04/27-import-fixed-width-data-with-pgloader.html), following up on other stories still
online at 
[Postgres OnLine Journal](http://www.postgresonline.com/journal/index.php?/archives/157-Import-fixed-width-data-into-PostgreSQL-with-just-PSQL.html) and on 
[David Fetter's blog](http://people.planetpostgresql.org/dfetter/index.php?/archives/58-psql,-Paste,-Perl-Pefficiency!.html).

Back then, I showed that using pgloader made it easier to import the data,
but also showed quite poor performances characteristics due to using the
`debug` mode in the timings. Let's update that article with
current [pgloader](https://github.com/dimitri/pgloader) wonders!

<!--more-->
<!--toc-->


# Redoing the python based test

Let's be fair, hardware did evolve in those past 3 years, and the test that
ran in 14 seconds was done with debug information level, which is the wrong
way to do tests.

~~~
$ ~/dev/pgloader-v2/pgloader.py -R. -Tsc census.conf 

Table name        |    duration |    size |  copy rows |     errors 
====================================================================
fixed             |      1.834s |       - |      25375 |          0
~~~


I got timings anywhere betseen 
***1.5 seconds*** and 
***1.834 seconds*** here.


# The new pgloader

Now with the current version of pgloader, what do we get:

~~~
$ pgloader.exe census-place.load
2013-11-18T12:02:35.001000+01:00 LOG Starting pgloader, log system is ready.
2013-11-18T12:02:35.003000+01:00 LOG Parsing commands from file "/Users/dim/dev/pgloader/test/census-places.load"

                    table name       read   imported     errors            time
------------------------------  ---------  ---------  ---------  --------------
                      download          0          0          0          1.587s
                       extract          0          0          0          1.010s
                   before load          0          0          0          0.014s
------------------------------  ---------  ---------  ---------  --------------
                        places      25375      25375          0          0.366s
------------------------------  ---------  ---------  ---------  --------------
             Total import time      25375      25375          0          2.977s
~~~

{{< image classes="fig25 right dim-margin" src="/img/old/mph.jpg" >}}

So the loading itself took as much as 
***366 milliseconds*** to run. To be fair
that's kind of a best run, with run times varying between that and 
***700
milliseconds***.

So the new version is about 
***3 to 9 times faster*** depending on the story you
want to tell. Let's stick with 
*much faster* for the sake of this article.


# The command

The new loader takes a full command as its input, rather than a
configuration file. Here's what the command I've used this time looks like:

~~~
LOAD ARCHIVE
   FROM http://www.census.gov/geo/maps-data/data/docs/gazetteer/places2k.zip
   INTO postgresql:///pgloader

   BEFORE LOAD DO
     $$ drop table if exists places; $$,
     $$ create table places
       (
          usps      char(2)  not null,
          fips      char(2)  not null,
          fips_code char(5),
          loc_name  varchar(64)
       );
     $$

   LOAD FIXED
        FROM FILENAME MATCHING ~/places2k.txt/
             WITH ENCODING latin-1
             (
             -- name   start  length
                usps       0  2,
                fips       2  2,
                fips_code  4  5,
                loc_name   9 64,
                p         73  9,
                h         82  9,
                land      91 14,
                water    105 14,
                ldm      119 14,
                wtm      131 14,
                lat      143 10,
                long     153 11
             )
        INTO postgresql:///pgloader?places
             (
	        usps, fips, fips_code,
                loc_name text using (right-trim loc_name)
             );
~~~


First, the URL used in the blog post of april 2010 is no longer valid. You
can find a list of other interesting data at the 
[Census 2000 Gazetteer Files](http://www.census.gov/geo/maps-data/data/gazetteer2000.html)
page, and of course you have more recent version of the data available. In
another format entirely, this time tab-based csv-like, so better for general
usage, but not for this test where I wanted to reuse the same data source as
3 years ago.

What we can see in that command is that pgloader will actually download the
zip archive file from its http source URL, unzip it locally then work on the
filename from the archive matching the one we know about: we don't want to
hardcode in the command the name of the directory contained in the zip file.

Also, contrary to the previous version, it's quite easy to just trim the
`loc_name` column as we load the data. Here I've been adding a new function to
do that, because I wanted to play with optimizing it (adding type
declarations and inlining it), but the loading works about as well with just
the following (just timed 3 runs at 
`0.771s`, 
`0.654s` and 
`0.862s`) :

~~~
        INTO postgresql:///pgloader?places
             (
	        usps, fips, fips_code,
                loc_name text using (string-right-trim " " loc_name)
             );
~~~


The 
[string-right-trim](http://www.lispworks.com/documentation/HyperSpec/Body/f_stg_tr.htm) function is part of the Common Lisp Standard. The
optimisation here looks like:

~~~ lisp
(declaim (inline right-trim))

(defun right-trim (string)
  "Remove whitespaces at end of STRING."
  (declare (type simple-string string))
  (string-right-trim '(#\Space) string))
~~~


Note that you could be providing that definition in your own 
`trim.lisp` file
and provide it using the 
`--load trim.lisp` command line option, pgloader
would then compile that to machine code for you before processing your data
file.


# Conclusion

If you're already using pgloader, you will enjoy the new version of it! The
new version comes with a command line option to migrate the old
configuration file into a command string, making upgrades even easier.

Of course, if you're interested, consider giving the release candidate a
try, it's available on the 
[pgloader](https://github.com/dimitri/pgloader) github repository already.

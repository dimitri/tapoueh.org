+++
date = "2013-10-01T16:52:00.000000+02:00"
title = "Loading Geolocation Data"
tags = ["PostgreSQL", "pgloader", "ip4r", "Extensions", "Common-Lisp","Geolocation","Point"]
categories = ["Projects","pgloader"]
thumbnailImage = "/img/old/lisp-python.png"
thumbnailImagePosition = "left"
coverImage = "/img/old/lisp-python.png"
coverSize = "partial"
coverMeta = "out"
aliases = ["/blog/2013/10/01-loading-geolocation-data",
           "/blog/2013/10/01-loading-geolocation-data.html"]
+++

As I've been mentionning in the past already, I'm currently rewriting
pgloader from scratch in 
[Common Lisp](/tags/common-lisp). In terms of 
[technical debt](http://en.wikipedia.org/wiki/Technical_debt) that's akin
to declaring 
*bankrupcy*, which is both sad news and good news as there's
suddenly new hope of doing it right this time.

<!--more-->

<center>*Let's dive into the python to common lisp rewrite*</center>

<!--toc-->

## Why rewriting pgloader?

Several problems hinted me into doing something other than maintaining the
code I had for 
[pgloader](/projects/pgloader).

First, the configuration file format was already used well beyond its
capacities: it's quite hard to express advanced fine grained options when
using the infamous 
[INI file format](http://en.wikipedia.org/wiki/INI_file). Continuing with python I did consider
using the 
[pyparsing](http://pyparsing.wikispaces.com/) library and even went as far as writing a prototype
command language with it. While the lib was working great for that, at the
time at least it wasn't available for 
*Python 3* and not to be found in some
OS vendors. 
*pgloader* is available today for Debian and derivatives, RedHat
and derivatives, FreeBSD, OpenBSD, and some more. And you can easily enough
use it on Windows too, because it only depends on 
[Psycopg](http://initd.org/psycopg/), basically, so
that I wanted to keep things simple for the end user. Basically, integrating
that lib was a problem.

Then again, the performances were a bigger and bigger worry for me as the
size of files one want to load into its database grew bigger. The real
problem was not perceived as the current performance characteristics but
rather how to improve them, and I didn't see many ways forward. The most
obvious one was to rewrite critical parts in C, but if I need to switch away
from python I'd rather find a better compromse than writing python modules
in C.

That's in short what lead me to consider alternatives. And as I had some
really good preliminary performances results with a programming language I
can actually like, I decided to go with Common Lisp.


## A Test case

The blog post I actually wanted to write would have been about the awesome
[ip4r](https://github.com/RhodiumToad/ip4r) extension, which is about indexing IP range searches as mostly used
nowadays in 
*geoip location*. The first step here is to find some data to use
and load them, and wanting to do that I realised we were lacking an
all-integrated tool easy enough to use.

{{< image classes="fig25 right dim-margin" src="/img/old/toy-loader.320.jpg" >}}

And that's exactly the reason why I'm working on 
*pgloader* in the first
place, to make it really easy to load data, converting it on the fly and all
the jazz. And that 
*ip4r* test case actually has been a very good excuse at
improving what I already had.


## The new pgloader command language

Here's the current way of doing things with the new 
*pgloader* written in
Common-Lisp, then I'll tell you why using a 
*parser generator library* in that
language is less of a burden than doing the same thing in Python:

~~~
LOAD FROM ARCHIVE http://geolite.maxmind.com/download/geoip/database/GeoLiteCity_CSV/GeoLiteCity-latest.zip
   INTO postgresql://dim@localhost:54393/dim

BEFORE LOAD DO
   $$ create extension if not exists ip4r; $$,
   $$ create schema if not exists geolite; $$,
   $$ create table if not exists geolite.location
     (
        locid      integer primary key,
        country    text,
        region     text,
        city       text,
        postalcode text,
        location   point,
        metrocode  text,
        areacode   text
     );
   $$,
   $$ create table if not exists geolite.blocks
     (
        iprange    ip4r,
        locid      integer
     );
   $$,
   $$ drop index if exists geolite.blocks_ip4r_idx; $$,
   $$ truncate table geolite.blocks, geolite.location cascade; $$

    LOAD CSV FROM FILENAME MATCHING ~/GeoLiteCity-Location.csv/
             WITH ENCODING iso-8859-1
             (
                locId,
                country,
                region     null if blanks,
                city       null if blanks,
                postalCode null if blanks,
                latitude,
                longitude,
                metroCode  null if blanks,
                areaCode   null if blanks
             )
        INTO postgresql://dim@localhost:54393/ip4r?geolite.location
             (
                locid,country,region,city,postalCode,
                location point using (format nil "(~a,~a)" longitude latitude),
                metroCode,areaCode
             )
        WITH skip header = 2,
             fields optionally enclosed by '"',
             fields escaped by double-quote,
             fields terminated by ','

AND LOAD CSV FROM FILENAME MATCHING ~/GeoLiteCity-Blocks.csv/
             WITH ENCODING iso-8859-1
             (
                startIpNum, endIpNum, locId
             )
        INTO postgresql://dim@localhost:54393/ip4r?geolite.blocks
             (
                iprange ip4r using (ip-range startIpNum endIpNum),
                locId
             )
        WITH skip header = 2,
             fields optionally enclosed by '"',
             fields escaped by double-quote,
             fields terminated by ','

FINALLY DO
   $$
     create index blocks_ip4r_idx on geolite.blocks using gist(iprange);
   $$;
~~~


What you see here looks quite complex and heavy. That's because it's
addressing the full needs, let's see about that:

  - the source of the data loading is a *zip archive* that we have to download from a *http url*,

  - the *zip archive* contains a single directory entry wherein we find the data files we want to load, currently `GeoLiteCity_20130903`... as we want to be able to update the data without having to edit the command, you will note the support for matching the file name against a full path name: `FILENAME MATCHING ~/GeoLiteCity-Blocks.csv/` is applying a regular expression,

  - we want the loading process to take care about creating the database schema we're going to use, and even drop the current index if any before loading, because we know that loading the data in bulk and then creating the index is way faster than keeping the index around,

  - there's no out of band place where to register whether a field value is null in a plain text CSV file, even if technically that could be done, so we have to provide per field instructions on what a `NULL` will look like in the data itself,

  - the files we're using don't have the exact definition we want: rather than a pair of *double precision* fields for hosting the location we want a [PostgreSQL point](http://www.postgresql.org/docs/9.3/static/datatype-geometric.html#AEN6542) datatype column; and rather than a pair of *bigint* fields we want an *ip4r* column in PostgreSQL,

  - and of course as soon as the data loading is done we want to create the shiny *ip4r* **GiST** index.

And as we are using an attribution licence for the data here, let me tell
you that: This article includes 
**GeoLite** data created by 
**MaxMind**, available
from 
[http://www.maxmind.com](http://www.maxmind.com).


## Transforming and Projecting

{{< image classes="fig25 left dim-margin"
              src="/img/old/huge-full-outer-join.gif"
            title="Traditionnaly used for Full Outer Join, but I liked the symbol">}}

The former *pgloader* was capable of using dynamic python code to reformat
fields on the fly, which was already quite good, but not up to the task
described just above. What we really want to be able to do here is *project*
any number of fields into a *possibly different* number of columns that are
dependant on the fields.

In the simplest case possible, each field ends up into a column of the same
name, and that's still supported of course, it looks like this:

~~~
LOAD CSV FROM FILENAME MATCHING ~/GeoLiteCity-Blocks.csv/
         WITH ENCODING iso-8859-1
    INTO postgresql://dim@localhost:54393/ip4r?geonumip.blocks
    WITH skip header = 2,
         fields optionally enclosed by '\"',
         fields escaped by double-quote,
         fields terminated by ',';
~~~


That's a very simple processing actually, with no added processing when
reading the file.

In the previous example though, we're using a 
*transformation* function that I
included in 
*pgloader* to transform a couple of integers into an 
*iprange*.
Here's the Common Lisp sources for that function:

~~~ lisp
(defun ip-range (start-integer-string end-integer-string)
  "Transform a couple of integers to an IP4R ip range notation."
  (declare (inline)
	   (optimize speed)
	   (type string start-integer-string end-integer-string))
  (flet ((integer-to-ip-string (int)
	   "see http://dev.maxmind.com/geoip/legacy/csv/"
	   (declare (inline) (optimize speed) (type fixnum int))
	   (format nil "~a.~a.~a.~a"
		   (mod (truncate int #. (expt 2 24)) 256)
		   (mod (truncate int #. (expt 2 16)) 256)
		   (mod (truncate int #. (expt 2 8)) 256)
		   (mod int 256))))
    (let ((ip-start (integer-to-ip-string (parse-integer start-integer-string)))
	  (ip-end   (integer-to-ip-string (parse-integer end-integer-string))))
      (format nil "~a-~a" ip-start ip-end))))
~~~


And here's how it's used:

~~~
INTO postgresql://dim@localhost:54393/ip4r?geolite.blocks
     (
        iprange ip4r using (ip-range startIpNum endIpNum),
        locId
     )
~~~


Internally, pgloader generates a 
*lambda* expression to process each row, in
that very case the expression looks like this:

~~~ lisp
(lambda (row)
  (destructuring-bind (startIpNum endIpNum locId) row
    (list (pgloader.transforms::ip-range startIpNum endIpNum)
          locId)))
~~~

{{< image classes="fig25 right dim-margin" src="/img/old/lisplogo_fancy_256.png" >}}

That generated lambda expression is then 
***compiled*** on the fly into machine
code that is then executed for each row input. That's the kind of things
allowing Common Lisp programmers not to resort to C.

Note that as in the previous 
*location* example you can also directly write
the Lisp expressions within the command itself:

~~~
INTO postgresql://dim@localhost:54393/ip4r?geolite.location
     (
        locid,country,region,city,postalCode,
        location point using (format nil "(~a,~a)" longitude latitude),
        metroCode,areaCode
     )
~~~



## Simple benchmarking

Enough presenting the software already, let's run it!


## Python version

First, let's run the current python pgloader code against our dataset. Note
that you will have to download and unzip the data yourself here, then
prepare that kind of configuration file:

~~~ ini
[pgsql]
base = pgloader

log_file            = /tmp/pgloader.log
log_min_messages    = INFO
client_min_messages = WARNING

client_encoding = 'latin1'
lc_messages         = C
pg_option_standard_conforming_strings = on
; This setting has no effect other than allowing to check option precedence
pg_option_work_mem = 12MB

copy_every      = 25000
commit_every    = 25000
#copy_delimiter  = %

null         = ""
empty_string = "\ "

[tmpl]
template        = True
format          = csv
field_sep       = ,
skip_head_lines = 2

[location]
use_template    = tmpl
table           = location
filename        = /tmp/GeoLiteCity-latest/GeoLiteCity_20130903/GeoLiteCity-Location.csv
columns         = *

[blocks]
use_template    = tmpl
table           = blocks
filename        = /tmp/GeoLiteCity-latest/GeoLiteCity_20130903/GeoLiteCity-Blocks.csv
columns         = *
skip_head_lines = 2
~~~


And with that we can do:

~~~ bash
./pgloader.py -R reformat -sTv -c ~/dev/temp/pgloader.geolite.conf
   ... edited ...
Table name        |    duration |    size |  copy rows |     errors 
====================================================================
blocks            |  01m18.979s |       - |    1790461 |          0
location          |     35.710s |       - |     438386 |          0
====================================================================
Total             |  01m54.713s |       - |    2228847 |          0
~~~



## Common Lisp version, no projection

With a command doing the same amount of work as the previous one for the
*blocks* table (I still kept the longitude and latitude to point formating):

~~~
                    table name       read   imported     errors       time
------------------------------  ---------  ---------  ---------  ---------
                       extract          0          0          0      1.007
                   before load          0          0          0      0.024
------------------------------  ---------  ---------  ---------  ---------
             geonumip.location     438386     438386          0      8.868
               geonumip.blocks    1790461    1790461          0     17.425
------------------------------  ---------  ---------  ---------  ---------
             Total import time    2228847    2228847          0    27.324s
~~~


We can see new sections, as the command is now actually extracting the
archive (at the size and given how many tests I wanted to do, the fetching
step as been avoided in the tests here), then preparing the load with all
the 
`CREATE TABLE IF NOT EXISTS` steps, and then actually parsing the files
and loading the data.

{{< image classes="fig25 left dim-margin" src="/img/old/made-with-lisp.320.png" >}}

So for doing the same work, we went from nearly 
***2 minutes*** with the previous
solution down to less than 
***30 seconds***, including the zip archive extraction
and the create table steps. Another way to look at it is that 
*pgloader* here
spent 
`9.73213 µs` to process each row. Yeah, less than 10 
***microseconds*** per
row.

Declaring bankrupcy on the previous code base was maybe not the worst
decision I made this year, finally...

I want to note here that the python importing code is actually using the 
*csv*
module for reading the files, and that module is already written in C in
fact. So we're actually comparing python and C on the one hand to Common
Lisp alone on the other hand.

## Common Lisp version with fields to columns projection

Now, as we have some time budget left, let's have the loader actually do
more work for us and convert on the fly to the data representation we are
interested into:

~~~
                    table name       read   imported     errors       time
------------------------------  ---------  ---------  ---------  ---------
                       extract          0          0          0      1.008
                   before load          0          0          0      0.134
------------------------------  ---------  ---------  ---------  ---------
              geolite.location     438386     438386          0      9.521
                geolite.blocks    1790461    1790461          0     31.546
------------------------------  ---------  ---------  ---------  ---------
                       finally          0          0          0     31.134
------------------------------  ---------  ---------  ---------  ---------
             Total import time    2228847    2228847          0  1m13.343s
~~~


So this time with the extra work we're going from about 18 seconds up to
about 32 seconds. The projection of integers to ip ranges is costly, but we
are still so much ahead from the old solution than we have enough time to
run the command 
`create index blocks_ip4r_idx on geolite.blocks using
gist(iprange);` and still finish about 40 seconds earlier.

This time 31,456,000 microseconds where spent loading 1,790,461 rows, and
that gives us 
`17.568659 µs` per row in average, including the 
*two bigint as
text to iprange as text* transformation calls. Not too bad.


## Using Common Lisp

Earlier in the article I said I would tell you why it's ok to use a 
*parser
generator library* such as 
[esrap](http://nikodemus.github.io/esrap/) here, whereas in the python case it was a
burden. The first part is that we don't have the major 
*platform* problem that
python has with version 2 against version 3: 
[the Common Lisp Specs](http://www.lispworks.com/documentation/HyperSpec/Front/) are a
really 
***stable*** base to develop libs, and so even decade old untouched code
has a really good chance to work as-is.

It's also possible and should be easy enough with 
[cl-buildapp](http://www.xach.com/lisp/buildapp/) to produce a
static all-included binary image of the application so that users don't have
to care about which programming language and libraires are used when
developping 
*pgloader*.

And of course the proficiency and interactivity of using Common Lisp is so
much better than when using python than you almost wonder why, given the
performances characteristics we're seeing here and in almost any other test,
o why are so much people still using python?

## Conclusion

Have a look at [pgloader.io](https://pgloader.io), the main website for
pgloader, and read about my white paper [Migrating to PostgreSQL, Tools and
Methodology](https://pgloader.io/white-paper/) which introduces the concept
of *Continuous Migration* using pgloader!

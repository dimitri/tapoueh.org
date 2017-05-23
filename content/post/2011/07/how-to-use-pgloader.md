+++
date = "2011-07-22T13:38:00.000000+02:00"
title = "How To Use PgLoader"
tags = ["PostgreSQL", "pgloader"]
categories = ["Projects","pgloader"]
thumbnailImage = "/img/old/toy-loader.320.jpg"
thumbnailImagePosition = "left"
coverImage = "/img/old/toy-loader.320.jpg"
coverSize = "partial"
coverMeta = "out"
aliases = ["/blog/2011/07/22-how-to-use-pgloader",
           "/blog/2011/07/22-how-to-use-pgloader.html"]
+++

This question about 
[pgloader](../../../pgsql/pgloader.html) usage coms in quite frequently, and I think the
examples 
[README](https://github.com/dimitri/pgloader/tree/master/examples) goes a long way in answering it.  It's not exactly a
*tutorial* but is almost there. Let me paste it here for reference:


# installing pgloader

Either use the 
[debian package](http://packages.debian.org/source/pgloader) or the one for your distribution of choice if
you use another one.  RedHat, CentOS, FreeBSD, OpenBSD and some more already
include a binary package that you can use directly.

Or you could 
`git clone https://github.com/dimitri/pgloader.git` and go from
there.  As it's all 
`python` code, it runs fine interpreted from the source
directory, you don't 
*need* to install it in a special place in your system.


# setting up the test environment

To use them, please first create a 
`pgloader` database, then for each example
the tables it needs, then issue the pgloader command:

~~~
$ createdb --encoding=utf-8 pgloader
$ cd examples
$ psql pgloader < simple/simple.sql
$ ../pgloader.py -Tvc pgloader.conf simple
~~~


If you want to load data from all examples, create tables for all of them
first, then run pgloader without argument.


# example description

The provided examples are:

  - simple

  This dataset shows basic case, with trailing separator and data
  reordering.

  - xzero

  Same as simple but using 
`\0` as the null marker (
`^@`)

  - errors

  Same test, but with impossible dates. Should report some errors. If it
  does not report errors, check you're not using psycopg 1.1.21.

  Should report 3 errors out of 7 lines (4 updates).

  - clob

  This dataset shows some text large object importing to PostgreSQL text
  datatype.

  - cluttured

  A dataset with newline escaped and multi-line input (without quoting)
  Beware of data reordering, too.

  - csv

  A dataset with csv delimiter ',' and quoting '"'.

  - partial

  A dataset from which we only load some columns of the provided one.

  - serial

  In this dataset the id field is ommited, it's a serial which will be
  automatically set by PostgreSQL while COPYing.

  - reformat

  A timestamp column is formated the way MySQL dump its timestamp,
  which is not the same as the way PostgreSQL reads them. The
  reformat.mysql module is used to reformat the data on-the-fly.

  - udc

  A used defined column test, where all file columns are not used but
  a new constant one, not found in the input datafile, is added while
  loading data.


# running the import

You can launch all those pgloader tests in one run, provided you created the
necessary tables:

~~~
$ for sql in */*sql; do psql pgloader < $sql; done
 $ ../pgloader.py -Tsc pgloader.conf

  errors       WARNING  COPY error, trying to find on which line
  errors       WARNING  COPY data buffer saved in /tmp/errors.AhWvAv.pgloader
  errors       WARNING  COPY error recovery done (2/3) in 0.064s
  errors       WARNING  COPY error, trying to find on which line
  errors       WARNING  COPY data buffer saved in /tmp/errors.BclHtj.pgloader
  errors       WARNING  COPY error recovery done (1/1) in 0.054s
  errors       ERROR    3 errors found into [errors] data
  errors       ERROR    please read /tmp/errors.rej.log for errors log
  errors       ERROR    and /tmp/errors.rej for data still to process
  errors       ERROR    3 database errors occured
  reformat     WARNING  COPY error, trying to find on which line
  reformat     WARNING  COPY data buffer saved in /tmp/reformat.6P4WCD.pgloader
  reformat     WARNING  COPY error recovery done (1/4) in 0.034s
  reformat     ERROR    1 errors found into [reformat] data
  reformat     ERROR    please read /tmp/reformat.rej.log for errors log
  reformat     ERROR    and /tmp/reformat.rej for data still to process
  reformat     ERROR    1 database errors occured
  
  Table name        |    duration |    size |  copy rows |     errors
  ====================================================================
  allcols           |      0.025s |       - |          8 |          0
  clob              |      0.034s |       - |          7 |          0
  cluttered         |      0.061s |       - |          6 |          0
  csv               |      0.035s |       - |          6 |          0
  errors            |      0.113s |       - |          4 |          3
  fixed             |      0.045s |       - |          3 |          0
  partial           |      0.030s |       - |          7 |          0
  reformat          |      0.036s |       - |          4 |          1
  serial            |      0.029s |       - |          7 |          0
  simple            |      0.050s |       - |          7 |          0
  udc               |      0.020s |       - |          5 |          0
  ====================================================================
  Total             |      0.367s |       - |         64 |          4
~~~


Please note errors test should return 3 errors and reformat 1 error.

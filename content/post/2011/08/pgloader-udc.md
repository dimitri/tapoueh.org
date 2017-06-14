+++
date = "2011-08-12T11:00:00.000000+02:00"
title = "pgloader constant cols"
tags = ["PostgreSQL", "pgloader"]
categories = ["Projects","pgloader"]
thumbnailImage = "/img/old/toy-loader.320.jpg"
thumbnailImagePosition = "left"
coverImage = "/img/old/toy-loader.320.jpg"
coverSize = "partial"
coverMeta = "out"
aliases = ["/blog/2011/08/12-pgloader-udc",
           "/blog/2011/08/12-pgloader-udc.html"]
+++

The previous articles in the [pgloader](http://pgloader.io) series
detailed
[How To Use PgLoader](http://tapoueh.org/blog/2011/07/22-how-to-use-pgloader.html) then
[How to Setup pgloader](http://tapoueh.org/blog/2011/07/29-how-to-setup-pgloader.html),
then what to expect from
a
[parallel pgloader](http://tapoueh.org/blog/2011/08/01-parallel-pgloader.html) setup,
and
then
[pgloader reformating](http://tapoueh.org/blog/2011/08/05-reformating-modules-for-pgloader.html).
Another need you might encounter when you get to
use [pgloader](../../../pgsql/pgloader.html) is adding *constant* values
into a table's column.

<!--more-->

{{< alert danger >}}

This article is about version series 2.x of pgloader, which are not
supported anymore. Consider using [pgloader](http://pgloader.io) version 3.x
instead. Also the following example is still available in the 3.x series and
you can see the *command file* at the GitHub repository for pgloader:

<https://github.com/dimitri/pgloader/blob/master/test/udc.load>.

{{< /alert >}}


The basic situation where you need to do so is adding an *origin* field to
your table. The value of that is not to be found in the data file itself,
typically, but known in the pgloader setup. That could even be the
`filename` you are importing data from.

In [pgloader](http://pgloader.io) that's called a *user defined column*.
Here's what the
relevant
[examples/pgloader.conf](https://github.com/dimitri/pgloader/blob/master/examples/pgloader.conf) setup
looks like:

~~~ ini
[udc]
table           = udc
format          = text
filename        = udc/udc.data
input_encoding  = 'latin1'
field_sep       = %
columns         = b:2, d:1, x:3, y:4
udc_c           = constant value
copy_columns    = b, c, d
~~~


And the data file is:

~~~
1%5%foo%bar
2%10%bar%toto
3%4%toto%titi
4%18%titi%baz
5%2%baz%foo
~~~


And here's what the loaded table looks like:

~~~ psql
pgloader/examples$ pgloader -Tsc pgloader.conf udc
Table name        |    duration |    size |  copy rows |     errors 
====================================================================
udc               |      0.201s |       - |          5 |          0

pgloader/examples$ psql --cluster 8.4/main pgloader -c "table udc"
 b  |       c        | d 
----+----------------+---
  5 | constant value | 1
 10 | constant value | 2
  4 | constant value | 3
 18 | constant value | 4
  2 | constant value | 5
(5 rows)
~~~


Of course the configuration is not so straightforward as to process fields
in the data file in the order that they appear, after all the
[examples/pgloader.conf](https://github.com/dimitri/pgloader/blob/master/examples/pgloader.conf) are also a test suite.

Long story short: if you need to add some 
*constant* values into the target
table you're loading data to, 
[pgloader](../../../pgsql/pgloader.html) will help you there!

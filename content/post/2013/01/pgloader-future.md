+++
date = "2013-01-28T10:48:00.000000+01:00"
title = "pgloader: what's next?"
tags = ["PostgreSQL", "Common-Lisp", "Python", "pgloader", "lparallel"]
categories = ["Projects","pgloader"]
thumbnailImage = "/img/old/PDL_Adapter-250.png"
thumbnailImagePosition = "left"
coverImage = "/img/old/PDL_Adapter-250.png"
coverSize = "partial"
coverMeta = "out"
aliases = ["/blog/2013/01/28-pgloader-future",
           "/blog/2013/01/28-pgloader-future.html"]
+++

[pgloader](../../../pgsql/pgloader.html) is a tool to help loading data into 
[PostgreSQL](http://www.postgresql.org/), adding some error
management to the 
[COPY](http://www.postgresql.org/docs/9.2/interactive/sql-copy.html) command. 
`COPY` is the fast way of loading data into
PostgreSQL and is transaction safe. That means that if a single error
appears within your bulk of data, you will have loaded none of it. 
`pgloader`
will submit the data again in smaller chunks until it's able to isolate the
bad from the good, and then the good is loaded in.

<center>*Not quite this kind of data loader*</center>

In a recent migration project where we freed data from MySQL into
PostgreSQL, we used 
`pgloader` again. But the loading time was not fast enough
for the service downtime window that we had here. Indeed 
[Python](http://www.python.org/) is not known
for being the fastest solution around. It's easy to use and to ship to
production, but sometimes you not only want to be able to be efficient when
writing code, you also need the code to actually run fast too.


## Faster data loading

So I began writing a little dedicated tool for that migration in 
[Common Lisp](http://cliki.net/)
which is growing on me as my personal answer to the burning question: 
*python
2 or python 3*? I find 
*Common Lisp* to offer an even more dynamic programming
environment, an easier language to use, and the result often has
performances characteristics way beyond what I can get with python. Between
[5 times faster](http://tapoueh.org/blog/2012/07/10-solving-sudoku.html) and 
[121 times faster](http://tapoueh.org/blog/2012/08/20-performance-the-easiest-way.html) in some quite stupid benchmark.

Here, with real data, my one shot attempt has been running more than 
*twice
as fast* as the python version, after about a day of programming.

<center>
{{< image classes="fig50 fancybox dim-margin" src="/img/old/lisp-python.png" >}}
</center>

<center>*See what's happening now?*</center>

The other thing here is that I've tempted to get 
`pgloader` work in parallel,
but at the time I didn't know about the 
[Global Interpreter Lock](http://docs.python.org/3/c-api/init.html#threads) that they
didn't find how to remove in Python 3 still, by the way. So my threading
attempts at making 
`pgloader` work in parallel are pretty useless.

Whereas in 
*Common Lisp* I can just use the 
[lparallel](http://lparallel.org/) lib, which exposes
threading facilities and some 
*queueing* facilities as a mean to communicate
data in between workers, and have my code easily work in parallel for real.


## Compatibility

The only drawback that I can see here is that if you've been writing your
own 
*reformating modules* in python for 
`pgloader` (yes you can
[implement your own reformating module for pgloader](http://tapoueh.org/pgsql/pgloader.html#sec21)), then you would have to
port it to 
*Common Lisp*. Shout me an email if that's your case.


## Next version

So, I think we're going to have a 
*pgloader 3* someday, that will be way
faster than the current one, and bundle some more features: real parallel
behavior, ability to fetch non local data (connecting to MySQL directly, or
HTTP, S3, etc); and I'm thinking about offering a 
`COPY` like syntax to drive
the loading too, while at it. Also, the ability to discover the set of data
to load all by itself when you want to load a whole database: think of it as
a special 
*Migration* mode of operations.

Some feature requests can't be solved easily when keeping the old 
`.INI`
syntax cruft, so it's high time to implement some kind of a real command
language. I have several ideas about those, in between the 
`COPY` syntax and
the 
`SQL*Loader` configuration format, which is both clunky and quite
powerful, too.

After a beginning in 
`TCL` and a complete rewrite in python in 
`2005`, it looks
like 
`2013` is going to be the year of 
*pgloader 3*, in 
*Common Lisp*!

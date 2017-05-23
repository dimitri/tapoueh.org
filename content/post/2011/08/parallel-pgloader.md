+++
date = "2011-08-01T12:15:00.000000+02:00"
title = "Parallel pgloader"
tags = ["PostgreSQL", "pgloader"]
categories = ["Projects","pgloader"]
thumbnailImage = "/img/old/toy-loader.320.jpg"
thumbnailImagePosition = "left"
coverImage = "/img/old/toy-loader.320.jpg"
coverSize = "partial"
coverMeta = "out"
aliases = ["/blog/2011/08/01-parallel-pgloader",
           "/blog/2011/08/01-parallel-pgloader.html"]
+++

This article continues the series that began with 
[How To Use PgLoader](http://tapoueh.org/blog/2011/07/22-how-to-use-pgloader.html) then
detailed 
[How to Setup pgloader](http://tapoueh.org/blog/2011/07/29-how-to-setup-pgloader.html).  We have some more fine points to talk about
here, today's article is about loading your data in parallel with 
[pgloader](../../../pgsql/pgloader.html).


# several files at a time

Parallelism is implemented in 3 different ways in pgloader.  First, you can
load more than one file at a time thanks to the 
`max_parallel_sections`
parameter, that has to be setup in the 
*global section* of the file.

This setting is quite simple and already allows the most common use case.


# several workers per file

The other use case is when you have huge files to load into the database.
Then you want to be able to have more than one process reading the file at
the same time.  Using 
[pgloader](../../../pgsql/pgloader.html), you already did the compromise to load the
whole content in more than one transaction, so there's no further drawback
here about having those multiple transactions per file spread to more than
one load 
*worker*.

There are basically two ways to split the work between several workers here,
and both are implemented in pgloader.


## N workers, N splits of the file
~~~
section_threads    = 4
split_file_reading = True
~~~


Setup this way, 
[pgloader](../../../pgsql/pgloader.html) will launch 4 different 
*threads* (see the 
**caveat**
section of this article).  Each thread is then given a part of the input
data file and will run the whole usual pgloader processing on its own.  For
this to work you need to be able to 
`seek` in the input stream, which might
not always be convenient.


## one reader, N workers
~~~
section_threads    = 4
split_file_reading = False
rrqueue_size       = 5000
~~~


With such a setup, 
[pgloader](../../../pgsql/pgloader.html) will start 4 different worker 
*threads* that will
receive the data input in an internal 
[python queue](http://docs.python.org/library/collections.html#deque-objects).  Another active 
*thread*
will be responsible of reading the input file and filling the queues in a
*round robin* fashion, but will hand all the processing of the data to each
worker, of course.


## how many threads?

If you're using a mix and match of 
`max_parallel_sections` and 
`section_threads`
with 
`split_file_reading` set to 
`True` of 
`False`, it's uneasy to know exactly
how many 
*threads* will run at any time in the loading.  How to ascertain
which section will run in parallel when it depends on the timing of the
loading?

The advice here is the usual one, don't overestimate the capabilities of
your system unless you are in a position to check before by doing trial
runs.


# caveat

Current implementation of all the parallelism in 
[pgloader](../../../pgsql/pgloader.html) has been done with
the 
[python threading](http://docs.python.org/library/threading.html) API.  While this is easy enough to use when you want to
exchange data between threads, it's suffering from the
[Global Interpreter Lock](http://docs.python.org/c-api/init.html#thread-state-and-the-global-interpreter-lock) issue.  This means that while the code is doing its
processing in parallel, the 
*runtime* not so much.  You might still benefit
from the current implementation if you have hard to parse files, or custom
reformat modules that are part of the loading bottleneck.


# future

The solution would be to switch to using the newer 
[python multiprocessing](http://docs.python.org/library/multiprocessing.html)
API, and some preliminary work has been done in pgloader to allow for that.
If you're interested in real parallel bulk loading, 
[contact-me](dim (at) tapoueh (dot) org)!

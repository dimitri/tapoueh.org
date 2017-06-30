+++
date = "2014-05-14T14:59:00+02:00"
title = "Why is pgloader so much faster?"
tags = ["PostgreSQL", "pgloader", "Common-Lisp"]
categories = ["Projects","pgloader"]
thumbnailImage = "/img/faster-icon.png"
thumbnailImagePosition = "left"
coverImage = "/img/Chevvy.jpg"
coverSize = "partial"
coverMeta = "in"
aliases = ["/blog/2014/05/14-pgloader-got-faster",
           "/blog/2014/05/14-pgloader-got-faster.html"]
+++

[pgloader](http://pgloader.io/) loads data into PostgreSQL. The new version is stable enough
nowadays that it's soon to be released, the last piece of the 
`3.1.0` puzzle
being full 
[debian](https://www.debian.org/) packaging of the tool.

<!--more-->

As you might have noticed if you've read my blog before, I decided
that [pgloader](http://pgloader.io/) needed a full rewrite in order for it
to be able to enter the current decade as a relevant tool. pgloader used to
be written in the [python programming language](https://www.python.org/),
which is used by lots of people and generally quite appreciated by its
users.

<!--toc-->

# Why changing

Still, python is not without problems, the main ones I had to deal with
being 
*poor performances* and lack of threading capabilities. Also, the
pgloader setup design was pretty hard to maintain, and adding compatiblity
to other 
*loader* products from competitors was harder than it should.

As I said in my 
[pgloader lightning talk](http://tapoueh.org/confs/2014/05/05-ELS-2014) at the 
[7th European Lisp Symposium](http://www.european-lisp-symposium.org/)
last week, in searching for a 
***modern programming language*** the best candidate
I found was actually 
[Common Lisp](http://en.wikipedia.org/wiki/Common_Lisp).

<center>
<div class="figure dim-margin">
  <a href="/images/confs/ELS_2014_pgloader.pdf">
    <img src="/img/old/ELS_2014_pgloader.png">
  </a>
</div>
</center>

After some basic performances checking as seen in my
[Common Lisp Sudoku Solver](https://github.com/dimitri/sudoku) project where I did get up to 
***ten times faster***
code when compared to python, it felt like the amazing set of features of
the language could be put to good use here.


# So, what about performances after rewrite?

The main reason why I'm now writing this blog post is receiving emails from
pgloader users with strange feelings about the speedup. Let's see at the
numbers one user gave me, for some data point:

~~~ sql
 select rows, v2, v3,
        round((  extract(epoch from v2)
               / extract(epoch from v3))::numeric, 2) as speedup
   from timing;
~~~
~~~ psql
  rows   |        v2         |       v3        | speedup 
---------+-------------------+-----------------+---------
 4768765 | @ 37 mins 10.878  | @ 1 min 26.917  |   25.67
 3115880 | @ 36 mins 5.881   | @ 1 min 10.994  |   30.51
 3865750 | @ 33 mins 40.233  | @ 1 min 15.33   |   26.82
 3994483 | @ 29 mins 30.028  | @ 1 min 18.484  |   22.55
(4 rows)
~~~


<center>*The raw numbers have been loaded into a PostgreSQL table*</center>

So what we see in this quite typical 
[CSV Loading](http://pgloader.io/howto/csv.html) test case is a best case of
***30 times faster*** import. Which brings some questions on the table, of course.


# Wait, you're still using *copy*, right?

The 
[PostgreSQL](http://www.postgresql.org/docs/9.3/interactive/index.html) database system provides a really neat 
[COPY](http://www.postgresql.org/docs/9.3/interactive/sql-copy.html) command, which in
turn is only exposing the 
[COPY Streaming Protocol](http://www.postgresql.org/docs/9.3/static/protocol-flow.html#PROTOCOL-COPY), that pgloader is using.

So yes, 
[pgloader](http://pgloader.io/) is still using 
`COPY`. This time the protocol implementation
is to be found in the Common Lisp 
[Postmodern](http://marijnhaverbeke.nl/postmodern/) driver, which is really great.
Before that, back when pgloader was python code, it was using the very good
[psycopg](http://initd.org/psycopg/) driver, which also exposes the COPY protocol.


# So, what did happen here?

Well it happens that pgloader is now built using Common Lisp technologies,
and those are really great, powerful and fast!

{{< image classes="fig25 right dim-margin" src="/img/old/speedup.jpg" >}}

Not only is Common Lisp code compiled to 
*machine code* when using most
[Common Lisp Implementations](http://cliki.net/Common%20Lisp%20implementation) such as 
[SBCL](http://sbcl.org/) or 
[Clozure Common Lisp](http://ccl.clozure.com/); it's also
possible to actually benefit from 
*parallel computing* and 
*threads* in Common
Lisp.

In the 
[pgloader](http://pgloader.io/) case I've been using the 
[lparallel](http://lparallel.org/) utilities, in particular
its 
[queuing facility](http://lparallel.org/api/queues/) to be able to implement 
*asynchronous IOs* where a thread
reads the source data and preprocess it, fills up a batch at a time in a
buffer that is then pushed down to the writer thread, that handles the 
`COPY`
protocol and operations.

So my current analysis is that the new thread based architecture used with a
very powerful compiler for the Common Lisp high-level language are allowing
pgloader to enter a whole new field of 
*data loading performances*.


# Conclusion

Not only is pgloader so much faster now, it's also full of new capabilities
and supports several sources of data such as 
[dBase files](http://pgloader.io/howto/dBase.html),
[SQLite database files](http://pgloader.io/howto/sqlite.html) or even 
[MySQL live connections](http://pgloader.io/howto/mysql.html).

Rather than a configuration file, the way to use the new pgloader is using a
*command language* that has been designed to look as much like SQL as possible
in the pgloader context, to make it easy for its users. Implementation wise,
it should now be trivial enough to implement compatibility with other 
*data
load* software that some 
[PostgreSQL](http://www.postgresql.org/) competitor products do have.

Also, the new code base and feature set seems to attract way more users than
the previous implementation ever did, despite using a less popular
programming language.

You can already 
[download pgloader binary packages](http://pgloader.io/download.html) for 
*debian* based
distributions and 
*centos* based ones too, and you will even find a 
*Mac OS X*
package file (
`.pkg`) that will make 
`/usr/local/bin/pgloader` available for you
on the command line. If you need a windows binary, drop me an email.

The first stable release of the new 
[pgloader](http://pgloader.io/) utility is scheduled to be
named 
`3.1.0` and to happen quite soon. We are hard at work on packaging the
dependencies for 
*debian*, and you can have a look at the 
[Quicklisp to debian](https://github.com/dimitri/ql-to-deb)
project if you want to help us get there!

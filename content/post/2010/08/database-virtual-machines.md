+++
date = "2010-08-03T13:30:00.000000+02:00"
title = "Database Virtual Machines"
tags = ["PostgreSQL"]
categories = ["PostgreSQL"]
thumbnailImage = "/img/postgresql-512.jpg"
thumbnailImagePosition = "left"
coverImage = "/img/postgresql-512.jpg"
coverSize = "partial"
coverMeta = "out"
aliases = ["/blog/2010/08/03-database-virtual-machines",
           "/blog/2010/08/03-database-virtual-machines.html"]
+++

Today I'm being told once again about 
[SQLite](http://www.sqlite.org/) as an embedded database
software. That one ain't a 
*database server* but a 
*software library* that you
can use straight into your main program. I'm yet to use it, but it looks
like 
[its SQL support](http://www.sqlite.org/lang.html) is good enough for simple things — and that covers
*loads* of things. I guess read-only cache and configuration storage would be
the obvious ones, because it seems that 
[SQLite use cases](http://www.sqlite.org/whentouse.html) aren't including
[mixed concurrency](http://www.sqlite.org/lockingv3.html), that is workloads with concurrent readers and writers.

The part that got my full attention is
[The Virtual Database Engine of SQLite](http://www.sqlite.org/vdbe.html), as this blog title would imply. It
seems to be the same idea as what 
[MonetDB](http://monetdb.cwi.nl/) calls their
[MonetDB Assembly Language](http://monetdb.cwi.nl/MonetDB/Documentation/MAL-Synopsis.html), and I've been trying to summarize some idea about
it in my 
[Next Generation PostgreSQL](http://tapoueh.org/char10.html#sec11) article.

The main thing is how to further optimize 
[PostgreSQL](http://www.postgresql.org/) given what we have. It
seems that among the major road blocks in the performance work is how we get
the data from disk and to the client. We're still spending so many time in
the 
`CPU` that the disk bandwidth are not always saturated, and that's a
problem. Further thoughts on the 
[full length article](http://tapoueh.org/char10.html#sec11), but that's just about
a one page section now!

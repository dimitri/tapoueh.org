+++
date = "2009-05-27T14:30:00.000000+02:00"
title = "PgCon 2009"
tags = ["PostgreSQL", "pgcon", "skytools"]
categories = ["PostgreSQL","Skytools"]
thumbnailImage = "/img/old/conferences.jpg"
thumbnailImagePosition = "left"
coverImage = "/img/old/conferences.jpg"
coverSize = "partial"
coverMeta = "out"
aliases = ["/blog/2009/05/27-pgcon-2009",
           "/blog/2009/05/27-pgcon-2009.html"]
+++

I can't really compare 
[PgCon 2009](http://www.pgcon.org/2009/) with previous years versions, last time I
enjoyed the event it was in 2006, in Toronto. But still I found the
experience to be a great one, and I hope I'll be there next year too!

I've met a lot of known people in the community, some of them I already had
the chance to run into at Toronto or 
[Prato](http://2008.pgday.org/en/), but this was the first time I
got to talk to many of them about interresting projects and ideas. That only
was awesome already, and we also had a lot of talks to listen to: as others
have said, it was really hard to get to choose to go to only one place out
of three.

I'm now back home and seems to be recovering quite fine from jet lag, and I
even begun to move on the todo list from the conference. It includes mainly
`Skytools 3` testing and contributions (code and documentation),
[Extension Packaging](http://wiki.postgresql.org/wiki/ExtensionPackaging) work (Stephen Frost seems to be willing to help, which I
highly appreciate) begining with 
[search_path issues](http://archives.postgresql.org/pgsql-hackers/2009-05/msg00912.php), and posting some
backtrace to help fix some 
[SPI_connect()](http://archives.postgresql.org/pgsql-hackers/2009-05/msg00923.php) bug at 
`_PG_init()` time in an
extension.

The excellent 
[lightning talk](http://wiki.postgresql.org/wiki/PgCon_2009_Lightning_talks) about _How not to Review a Patch_ by Joshua
Tolley took me out of the 
*dim*, I'll try to be 
*bright* enough and participate
as a reviewer in later commit fests (well maybe not the first next ones as
some personal events on the agenda will take all my 
*"free"* time)...

Oh and the 
[Golconde](http://code.google.com/p/golconde/) presentation gave some insights too: this queueing based
solution is to compare to the 
`listen/notify` mechanisms we already have in
[PostgreSQL](http://www.postgresql.org/docs/current/static/sql-listen.html), in the sense that's it's not transactional, and the events are
kept in memory only to achieve very high distribution rates. So it's a very
fine solution to manage a distributed caching system, for example, but not
so much for asynchronous replication (you need not to replicate events tied
to rollbacked transactions).

So all in all, spending last week in Ottawa was a splendid way to get more
involved in the PostgreSQL community, which is a very fine place to be
spending ones free time, should you ask me. See you soon!

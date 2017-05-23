+++
date = "2011-05-30T11:00:00.000000+02:00"
title = "Back from Ottawa, preparing for Cambridge"
tags = ["PostgreSQL", "debian", "pgcon", "Conferences", "skytools", "9.1"]
categories = ["Conferences","PostgreSQL Confs"]
thumbnailImage = "/img/old/conferences.jpg"
thumbnailImagePosition = "left"
coverImage = "/img/old/conferences.jpg"
coverSize = "partial"
coverMeta = "out"
aliases = ["/blog/2011/05/30-back-from-ottawa-preparing-for-cambridge",
           "/blog/2011/05/30-back-from-ottawa-preparing-for-cambridge.html"]
+++

While 
[Magnus](http://blog.hagander.net/) is all about 
[PG Conf EU](http://2011.pgconf.eu/) already, you have to realize we're just
landed back from 
[PG Con](http://www.pgcon.org/2011/) in Ottawa.  My next stop in the annual conferences
is 
[CHAR 11](http://char11.org/), the 
*Clustering, High Availability and Replication* conference in
Cambridge, 11-12 July.  Yes, on the old continent this time.

This year's 
*pgcon* hot topics, for me, have been centered around a better
grasp at 
[SSI](http://www.postgresql.org/docs/9.1/static/transaction-iso.html#XACT-SERIALIZABLE) and 
*DDL Triggers*.  Having those beasts in 
[PostgreSQL](http://www.postgresql.org/) would
allow for auditing, finer privileges management and some more automated
replication facilities.  Imagine that 
`ALTER TABLE` is able to fire a 
*trigger*,
provided by 
*Londiste* or 
*Slony*, that will do what's needed on the cluster by
itself.  That would be awesome, wouldn't it?

At 
*CHAR 11* I'll be talking about 
[Skytools 3](http://wiki.postgresql.org/wiki/SkyTools).  You know I've been working on
its 
*debian* packaging, now is the time to review the documentation and make
there something as good looking as the monitoring system are...

Well, expect some news and a nice big picture diagram overview soon, if work
schedule leaves me anytime that's what I want to be working on now.

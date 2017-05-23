+++
date = "2009-04-14T00:00:00.000000+02:00"
title = "Skytools 3.0 reaches alpha1"
tags = ["PostgreSQL", "release", "skytools"]
categories = ["PostgreSQL","Release"]
thumbnailImage = "/img/old/londiste_logo.gif"
thumbnailImagePosition = "left"
coverImage = "/img/old/londiste_logo.gif"
coverSize = "partial"
coverMeta = "out"
aliases = ["/blog/2009/04/14-skytools-30-reaches-alpha1",
           "/blog/2009/04/14-skytools-30-reaches-alpha1.html"]
+++

It's time for 
[Skytools](http://wiki.postgresql.org/wiki/Skytools) news again! First, we did improve documentation of
current stable branch with hosting high level presentations and 
[tutorials](http://wiki.postgresql.org/wiki/Londiste_Tutorial) on
the 
[PostgreSQL wiki](http://wiki.postgresql.org/). Do check out the 
[Londiste Tutorial](http://wiki.postgresql.org/wiki/Londiste_Tutorial), it seems that's
what people hesitating to try out londiste were missing the most.

The other things people miss out a lot in current stable Skytools (version
`2.1.9` currently) are cascading replication (which allows for 
*switchover* and
*failover*) and 
`DDL` support. The new incarnation of skytools, version 
`3.0`
[reaches alpha1](http://lists.pgfoundry.org/pipermail/skytools-users/2009-April/001029.html) today. It comes with full support for 
*cascading* and 
*DDL*, so
you might want to give it a try.

It's a rough release, documentation is still to get written for a large part
of it, and bugs are still to get fixed. But it's all in the Skytools spirit:
simple and efficient concepts, easy to use and maintain. Think about this
release as a 
*developer preview* and join us :)

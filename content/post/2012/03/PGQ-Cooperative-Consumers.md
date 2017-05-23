+++
date = "2012-03-12T14:43:00.000000+01:00"
title = "PGQ Coop Consumers"
tags = ["PostgreSQL", "PGQ"]
categories = ["PostgreSQL","Skytools"]
thumbnailImage = "/img/old/workers.jpg"
thumbnailImagePosition = "left"
coverImage = "/img/old/workers.jpg"
coverSize = "partial"
coverMeta = "out"
aliases = ["/blog/2012/03/12-PGQ-Cooperative-Consumers",
           "/blog/2012/03/12-PGQ-Cooperative-Consumers.html"]
+++

While working a new 
[PostgreSQL](http://www.postgresql.org/) architecture for an high scale project that
used to be in the top 10 of internet popular web sites (in terms of
visitors), I needed to be able to off load some processing from the main
path: that's called a 
*batch job*. This needs to be 
*transactional*: don't run
the job if we did 
`rollback;` the transaction, process all 
*events* that were
part of the same transaction in the same transaction, etc.


That calls for using 
[PGQ](http://wiki.postgresql.org/wiki/PGQ_Tutorial), the 
*jobs queue* solution from 
[Skytools](http://wiki.postgresql.org/wiki/Skytools), the power
horse for 
[Londiste](http://wiki.postgresql.org/wiki/Londiste_Tutorial). If 
`PGQ` is good enough to build a full trigger-based
replication solution on top of it, certainly it's good enough for our custom
processing, right? Well, you still need to check that your expectations are
met, and that was happily the case in my implementation. It's a very common
problem, and 
`PGQ` very often is a great solution to it.

As this implementation is 
`PHP` centric, we've been using 
[libphp-pgq](https://github.com/dimitri/libphp-pgq) to drive
our background workers. Using 
`PGQ` in 
`PHP` has been very easy to setup, the
only trap being not to forget about running the 
*ticker* process.

It got interesting because of two elements. First, we're nor running a
single database instance here but a bunch of them... make it 
*256 databases*.
Each of them having 
`5` queues to consume, that would be about 
`1280` consumer
processes, distributed on 
`16` servers that's still 
`80` per server, so way too
many. What we did instead is reuse the 
[queue mover](https://github.com/markokr/skytools/blob/master/scripts/queue_mover.py) script found in the
Skytools distribution and adapt it to 
*forward* the event of the 1280 source
queues to only 5 destination queues. We then process the events from this
single location.

Now it's easier to deal with, but we're not still exactly there. Of course,
with so many sources, concentrating them all into the same place means that
a single consumer is not able to process the events as fast as they are
produced. That's where the 
*cooperative consuming* shines, it's very easy to
turn your 
*consumer* into a 
*cooperative* one even on an existing and running
queue, and that's what we did. So now we can choose how many 
*workers* we want
per queue: one of them has 4 workers, another one see not so much activity
and 1 worker still fits.


{{< image classes="fig50 center fancybox dim-margin" src="/img/old/coop-workers.jpeg" >}}


The queue mover script that knows how to subscribe to many queues from the
same process is going to be contributed to Skytools proper, of course.

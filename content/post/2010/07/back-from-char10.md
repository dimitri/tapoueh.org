+++
date = "2010-07-05T09:30:00.000000+02:00"
title = "Back from CHAR(10)"
tags = ["PostgreSQL", "Conferences", "pgcon"]
categories = ["Conferences","PostgreSQL Confs"]
thumbnailImage = "/img/old/conferences.jpg"
thumbnailImagePosition = "left"
coverImage = "/img/old/conferences.jpg"
coverSize = "partial"
coverMeta = "out"
aliases = ["/blog/2010/07/05-back-from-char10",
           "/blog/2010/07/05-back-from-char10.html"]
+++

It surely does not feel like a full month and some more went by since we
were enjoying 
[PGCon 2010](http://www.pgcon.org/2010/), but in fact it was already the time for
[CHAR(10)](http://char10.org/talk-schedule). The venue was most excellent, as Oxford is a very beautiful
city. Also, the college was like a city in the city, and having the
accomodation all in there really smoothed it all.

On a more technical viewpoint, the 
[range of topics](http://char10.org/talk-schedule) we talked about and the
even broader one in the 
*"Hall Track"* make my mind full of ideas, again. So
I'm preparing a quite lengthy article to summarise or present all those
ideas, and I think a post series should cover the points in there. When
trying to label things, it appears that my current obsessions are mainly
about 
*PostgreSQL in the Cloud* and 
*Further Optimising PostgreSQL*, so that's
what I'll be talking about those next days.

Meanwhile I'm going to search for existing solutions on how to use the
[Paxos algorithm](http://en.wikipedia.org/wiki/Paxos_algorithm) to generate a reliable distributed sequence, using 
[libpaxos](http://libpaxos.sourceforge.net/)
for example. The goal would be to see if it's feasible to have a way to
offer some global 
`XID` from a network of servers in a distributed fashion,
ideally in such a way that new members can join in at any point, and of
course that losing a member does not cause downtime for the online ones. It
sounds like this problem has been extensively researched and is solved,
either by the 
*Global Communication Systems* or the underlying
algorithms. Given the current buy-in lack of our community for 
`GCS` my guess
is that bypassing them would be a pretty good move, even if that mean
implementing a limited form of 
`GCS` ourselves.

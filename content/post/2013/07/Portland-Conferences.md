+++
date = "2013-07-29T17:09:00.000000+02:00"
title = "OSCON, Portland, and PDXPUG"
tags = ["PostgreSQL", "Conferences", "OSCON"]
categories = ["Conferences","PostgreSQL Confs"]
thumbnailImage = "/img/old/mark_crochet_oscon_2013.640.jpg"
thumbnailImagePosition = "left"
coverImage = "/img/old/mark_crochet_oscon_2013.640.jpg"
coverSize = "partial"
coverMeta = "out"
aliases = ["/blog/2013/07/29-Portland-Conferences",
           "/blog/2013/07/29-Portland-Conferences.html"]
+++

After spending an awesome week in 
*San Francisco, CA* I'm lucky enough to be
spending another week in the USA, in 
*Portand, OR*. The main excuse for
showing up here has been 
[OSCON](http://www.oscon.com/oscon2013) where I presented a talk about the 
[fotolog](http://fotolog.com/)
migration from MySQL to 
[PostgreSQL](http://www.postgresql.org/).


*[Mark Wong](http://markwkm.blogspot.com/) is doing some serious database crochet work!*

Fotolog is a photo sharing website having more than 
*32 millions* of users
sharing more than 
***a billion*** of photos, which made for a very interesting
migration use case. In particular switching from a hand-made MySQL sharding
cluster of 37 databases to a fully integrated 
[PLProxy](http://wiki.postgresql.org/wiki/PL/Proxy) setup on 16 servers
hosting each 16 shards, in less than 6 months including complete code
rewrite, made things... interesting.


<div class="figure center dim-margin">
  <a href="/images/confs/fotolog.pdf">
    <img src="/img/old/archi_v7.png">
  </a>
</div>

*This image is the missing piece in the slide deck*

While in Portland I also had the pleasure to present a talk at the 
[PGXPUG](http://pdxpug.wordpress.com/)
User Group Meeting, hosted by 
[MyEmma](http://myemma.com/) who provided a very nice place, beers
and pizzas. Meeting with local users is always a great time, and I've
presenting the 
*PostgreSQL as a developer* talk that I also did for the 
*Dublin*
User Group, which you can see online at my 
[Dublin User Group](/confs/2013/07/02-dubpug) conference
report page.


<div class="figure center dim-margin">
  <a href="/images/confs/postgresql-as-a-developer.pdf">
    <img src="/img/old/postgresql-as-a-developer.png">
  </a>
</div>

*You are already using SQL, make the best out of it!*

Apparently at least one of the attendees really did like the presentation.
The angle is to convince developpers to consider 
**SQL** really as one of their
programming languages, more like they consider 
*Javascript* than how they
generally consider 
*HTML*. And this attendee's questions where all about how
much of the 
*middleware* (or 
*model implementation*) can we move down to
PostgreSQL.

Of course the technical answer is all of it, as demonstrated by
[apache mod_libpq](http://asmith.id.au/mod_libpq.html), wherein URLs are simply transformed into 
*stored procedure*
calls. Which in PostgreSQL you can implement in basically any programming
language you want, like 
[PL/Python](http://www.postgresql.org/docs/current/static/plpython.html) or 
[PL/Perl](http://www.postgresql.org/docs/current/static/plperl.html) to give but only two examples.

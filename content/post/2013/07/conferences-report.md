+++
date = "2013-07-03T16:53:00.000000+02:00"
title = "Conferences Report"
tags = ["PostgreSQL", "Conferences"]
categories = ["Conferences","PostgreSQL Confs"]
thumbnailImage = "/img/old/kattekrab_Crowd.png"
thumbnailImagePosition = "left"
coverImage = "/img/old/kattekrab_Crowd.png"
coverSize = "partial"
coverMeta = "out"
aliases = ["/blog/2013/07/03-conferences-report",
           "/blog/2013/07/03-conferences-report.html"]
+++

Recently I've been to some more conferences and didn't take the time to blog
about them, even though I really did have great fun over there. So I felt I
should take some time and report about my experience at those conferences.
And of course, some more is on the way, as the 
[PostgreSQL](http://www.postgresql.org/) Conference Tour
gets busier each year it seems.


*And PostgreSQL Conferences get more attendees each year!*

## PGCON 2013, Ottawa

In may was the famous 
[PGCON](http://www.pgcon.org/2013/) conference where PostgreSQL contributors are
meeting all together, offering the occasion to run the 
*Hackers Meeting*. This
year's edition has been a really great one, with lots of people attending
and lots of really interesting talks to attend to. In fact, so much
interesting that I almost skipped the 
*Hallway Track* entirely, which is
really impressive.

The main topics of interests I've been hearing in the more general talks
I've attended, and in the numerous informal chats I had (we call that the
*beer track* or the 
*social events*) where about 
*Bi Directional Replication* as
the next step forward with our core-included replication technology and how
to get more from 
*Extensions*. I'm very happy to be affiliated with the
company working on those topics, as you can imagine.

I've been presenting my 
[Implementing High Availability](http://www.pgcon.org/2013/schedule/events/533.en.html) talk and the video of
it is now available on Youtube, so that you can enjoy my unique accent:


<iframe style="margin-left: 15%;" width="420" height="315" src="//www.youtube.com/embed/j642n39oBgQ" frameborder="0" allowfullscreen></iframe>


My own take away for that conference is another batch of work to complete
for 
*Extensions* and 
*Event Triggers*, so expect to see some articles about
those topics in the following months, and with some luck I will even be able
to talk about what I want to achieve when those tools land in core.


## PGDayFR 2013


<div class="figure center dim-margin">
  <a href="/images/confs/petabyte.pdf">
    <img src="/img/old/petabyte.png">
  </a>
</div>

Then in June was the 
[PG Day France](https://www.pgday.fr/programme) where I presented a talk about
[Petabyte PostgreSQL](https://www.pgday.fr/_media/petabyte.pdf), in french. This talk is about listing the current
limitations preventing us from enjoying PostgreSQL at full capacity on a
single 
*Petabyte* node, then talking about the work in progress to get there.

The conference itself was great with about one hundred attendees and a good
talk selection. It was a single track, all in french, and I still hope that
we would be able to organize a conference with two tracks, allowing us to
invite speakers from all over Europe. I'm pretty sure attendees would be
happy to listen to English talks if they had a choice about it and could go
to the main french session instead.


## CHAR(13)


{{< image classes="fig50 center fancybox dim-margin" src="/img/old/horwoodhouse.jpg" >}}


The next conference on the schedule is now 
[CHAR(13)](http://char13.info/), a 
[2ndQuadrant](http://2ndquadrant.com/) company
conference about 
***Clustering, High Availability and Replication***. I will be
talking about 
*Advanced Distributed Architectures*, or in other words those
use cases for replication where the main goal is not to implement High
Availability.

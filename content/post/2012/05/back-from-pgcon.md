+++
date = "2012-05-24T09:40:00.000000+02:00"
title = "Back From PgCon"
tags = ["PostgreSQL", "Conferences", "Fotolog"]
categories = ["Conferences","PostgreSQL Confs"]
thumbnailImage = "/img/old/in-core-replication.jpg"
thumbnailImagePosition = "left"
coverImage = "/img/old/in-core-replication.jpg"
coverSize = "partial"
coverMeta = "out"
aliases = ["/blog/2012/05/24-back-from-pgcon",
           "/blog/2012/05/24-back-from-pgcon.html"]
+++

Last week was the annual 
*PostgreSQL Hackers* gathering in Canada, thanks to
the awesome 
[pgcon](http://www.pgcon.org/) conference. This year's issue has been packed with good
things, beginning with the 
[Cluster Summit](http://wiki.postgresql.org/wiki/PgCon2012CanadaClusterSummit) then followed the next day by the
[Developer Meeting](http://wiki.postgresql.org/wiki/PgCon_2012_Developer_Meeting) just followed (yes, in the same day) with the
[In Core Replication Meeting](http://wiki.postgresql.org/wiki/PgCon2012CanadaInCoreReplicationMeeting). That was a packed shedule!


The 
*in core replication* project has been presented with slides titled
[Future In-Core Replication for PostgreSQL](http://wiki.postgresql.org/images/7/75/BDR_Presentation_PGCon2012.pdf) and got a very good reception. For
instance, people implementing 
[Slony](http://slony.info/) (
*Jan Wieck*, 
*Christopher Browne* and 
*Steve
Singer* where here) appreciated the concepts here and where rather supportive
of both the requirements and the design, and appreciated the very early demo
and results that we had to show already, as a kind of a proof of concepts.

After those first two days, we could start the actual show. I had the honnor
to present a migration use case entitled 
[Large Scale MySQL Migration](http://www.pgcon.org/2012/schedule/events/431.en.html) where
we're speaking about going from MySQL to PostgreSQL, from 37 to 256 shards,
moving more than 6TB of data including binary 
*blobs* that we had to process
with 
`pl/java`. A quite involved migration project whose slides you now can
read here:


<div class="figure center dim-margin">
  <a href="/images/confs/fotolog.pdf">
    <img src="/img/old/fotolog.jpg">
  </a>
</div>

I've heard that we should soon be able to enjoy audio and video recordings
of the sessions, so if you couldn't make it this year for any reason, don't
miss that, you will have loads of very interesting talks to virtually
attend. I definitely will do that to catch-up with some talks I couldn't
attend, having to pick one out of three is not an easy task, all the more
when you add the providential 
*hallway track*.

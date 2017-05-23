+++
date = "2011-10-26T10:08:00.000000+02:00"
title = "Back From Amsterdam"
tags = ["PostgreSQL", "Conferences"]
categories = ["Conferences","PostgreSQL Confs"]
thumbnailImage = "/img/old/ams-conf-room.jpg"
thumbnailImagePosition = "left"
coverImage = "/img/old/ams-conf-room.jpg"
coverSize = "partial"
coverMeta = "out"
aliases = ["/blog/2011/10/26-back-from-amsterdam",
           "/blog/2011/10/26-back-from-amsterdam.html"]
+++

Another great conference took place last week,
[PostgreSQL Conference Europe 2011](http://2011.pgconf.eu/) was in Amsterdam and plenty of us
PostgreSQL geeks were too. I attended to lot of talks and did learn some
more about our project, its community and its features, but more than that
it was a perfect occasion to meet with the community.


[Dave Page](http://www.postgresql.eu/events/schedule/pgconfeu2011/speaker/2-dave-page/) talked about 
`SQL/MED` under the title
[PostgreSQL at the center of your dataverse](http://www.postgresql.eu/events/schedule/pgconfeu2011/session/146-postgresql-at-the-center-of-your-dataverse/) and detailed what to expert from
a 
*Foreign Data Wrapper* in PostgreSQL 9.1, then how to write your own.
Wherever you are currently managing your data, you can easily enough make it
so that PostgreSQL integrates them by means of fetching them to answer your
queries. Which means real time data federating: you don't copy data around,
you remote access them when executing the query.

I might need to come up with new 
*Foreign Data Wrappers* in a not too distant
future, now that I better grasp how much work it really is to do that, it
appears to be a good migration strategy too:

~~~
INSERT INTO real.table
       SELECT * FROM foreign.table;
~~~


Another discovery is that apparently 
[PLv8](http://code.google.com/p/plv8js/wiki/PLV8) is ready for public consumption.
Using it can lead to 
[Heralding the Death of NoSQL](http://www.postgresql.eu/events/schedule/pgconfeu2011/session/174-heralding-the-death-of-nosql/), so use it with care.

In the presentation of 
[Synchronous Replication and Durability Tuning](http://www.postgresql.eu/events/schedule/pgconfeu2011/session/156-synchronous-replication-and-durability-tuning/) we
mainly saw that mixing 
*synchronous* and 
*asynchronous* transactions in your
application is the key to real performances across the ocean, as the speed
of the light is not infinite. From Baltimore to Amsterdam the latency can
not be better than 
`100ms` and that's not the same as 
*instant* nowadays.

Then again, depending on the number of concurrent queries to sync over the
ocean link, the experimental setup was able to achieve several thousands of
queries per second, which is validating the model we picked for 
*sync rep* and
its implementation.

If you want to read the slides again at home, or if you could not be there
for some reason, then most of the talks are now available online at the
[PostgreSQL Conference Europe Talks 2011](http://wiki.postgresql.org/wiki/PostgreSQL_Conference_Europe_Talks_2011) wiki page.

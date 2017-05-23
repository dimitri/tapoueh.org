+++
date = "2013-03-18T14:54:00.000000+01:00"
title = "Bulk Replication"
tags = ["PostgreSQL", "Skytools", "Londiste"]
categories = ["PostgreSQL","Skytools"]
thumbnailImage = "/img/old/clock-key.jpg"
thumbnailImagePosition = "left"
coverImage = "/img/old/clock-key.jpg"
coverSize = "partial"
coverMeta = "out"
aliases = ["/blog/2013/03/18-bulk-replication",
           "/blog/2013/03/18-bulk-replication.html"]
+++

In the previous article here we talked about how to 
*properly* update more
than one row at a time, under the title 
[Batch Update](http://tapoueh.org/blog/2013/03/15-batch-update.html). We did consider
performances, including network round trips, and did look at the behavior of
our results when used concurrently.


A case where we want to apply the previous article approach is when
replicating data with a 
*trigger based solution*, such as 
[SkyTools](http://wiki.postgresql.org/wiki/SkyTools) and
[londiste](https://github.com/markokr/skytools). Well, maybe not in all cases, we need to have a amount of 
`UPDATE`
trafic worthy of setting up the solution. As soon as we know we're getting
to 
*replay* important enough batches of events, though, certainly using the
*batch update* tricks makes sense.

It so happens that 
`londiste 3` includes the capability to use 
*handlers*. Those
are plugins written in 
*python* (like all the client side code from 
*SkyTools*)
whose job is to handle the 
*processing* of the event batches. Several of them
are included in the 
[londiste sources](https://github.com/markokr/skytools/tree/master/python/londiste), and one of them is named 
`bulk.py`.


## Bulk loading data with londiste

To use set in 
`londiste.ini`:

~~~
handler_modules = londiste.handlers.bulk
~~~


then add table with one of those commands:

~~~
londiste3 add-table xx --handler="bulk"
londiste3 add-table xx --handler="bulk(method=X)"
~~~


The default method is 
`0`, and the available methods are the following:

*correct* (
`0`)
  - inserts as 
`COPY` into table
  - update as 
`COPY` into temp table and single 
`UPDATE` from there
  - delete as 
`COPY` into temp table and single 
`DELETE` from there

*delete* (
`1`)
  - as 
*correct*, but 
*update* are done as 
`DELETE` then 
`COPY`

*merged* 
`(2`)
 - as 
*delete*, but merge 
*insert* rows with 
*update* rows


## Conclusion


{{< image classes="fig50 center fancybox dim-margin" src="/img/old/londiste.jpg" >}}


Yes, by using that 
*handler* which is provided by default in 
*londiste*, you
will apply the previous article tricks in your replication solution. And you
can even choose to use that for only some of the tables you are replicating.

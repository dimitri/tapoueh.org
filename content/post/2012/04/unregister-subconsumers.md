+++
date = "2012-04-26T15:05:00.000000+02:00"
title = "Clean PGQ Subconsumers"
tags = ["PostgreSQL", "PGQ", "Skytools"]
categories = ["PostgreSQL","Skytools"]
thumbnailImage = "/img/old/drop-queue.png"
thumbnailImagePosition = "left"
coverImage = "/img/old/drop-queue.png"
coverSize = "partial"
coverMeta = "out"
aliases = ["/blog/2012/04/26-unregister-subconsumers",
           "/blog/2012/04/26-unregister-subconsumers.html"]
+++

Now that you're all using the wonders of 
[Cooperative Consumers](../03/12-PGQ-Cooperative-Consumers.html) to help you
efficiently and reliably implement your business constraints and offload
them from the main user transactions, you're reaching a point where you have
to clean up your development environment (because that's what happens to
development environments, right?), and you want a way to start again from a
clean empty place.

Here we go. It used to be much more simple than that, so if you're still
using 
**PGQ** from 
**Skytools2**, just jump to the next step.


## Unregister Subconsumers

That query will figure out subconsumers in the system function
`pgq.get_consumer_info()` and ask PGQ to please 
*unregister* them, losing events
in the way, even events from batches that are currently active.

~~~
with subconsumers as (
   select q1.queue_name,
          q2.consumer_name,
          substring(q1.consumer_name from '%.#"%#"' for '#') as subconsumer_name
    from (select *
            from pgq.get_consumer_info()
           where lag is null)
         as q1
    join (select *
           from pgq.get_consumer_info()
           where lag is not null)
         as q2
         on q1.queue_name = q2.queue_name
)
select *,
       pgq_coop.unregister_subconsumer(queue_name, consumer_name,
                                       subconsumer_name, 1)
 from subconsumers;
~~~



## Unregister Consumers

Now that the first step is done, we have to 
*unregister* the main consumers,
which is easy and what you already did before:

~~~
select queue_name, consumer_name,
       pgq.unregister_consumer(queue_name, consumer_name)
  from pgq.get_consumer_info();
~~~



## Drop queues

And as we want to really clean up the mess, let's also drop the queues.

~~~
select queue_name, pgq.drop_queue(queue_name)
  from pgq.queue;
~~~


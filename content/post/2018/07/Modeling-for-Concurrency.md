+++
title = "Modeling for Concurrency"
date = "2018-07-10T10:26:47+02:00"
tags = ["PostgreSQL","YeSQL","Concurrency","Modelisation","UPDATE","INSERT"]
categories = ["PostgreSQL","YeSQL"]
coverImage = "/img/concurrency-dogs.jpg"
coverMeta = "in"
coverSize = "partial"
thumbnailImage = "/img/concurrency-logo.png"
thumbnailImagePosition = "left"

+++

Let's continue to dive in PostgreSQL Concurrency. Last week's article
[PostgreSQL Concurrency: Isolation and
Locking](/blog/2018/07/postgresql-concurrency-isolation-and-locking/) was a
primer on PostgreSQL isolation and locking properties and behaviors.

Today's article takes us a step further and builds on what we did last week,
in particular the database modeling for a *tweet* like application. After
having had all the characters from Shakespeare's *A Midsummer Night's Dream*
tweet their own lines in our database in [PostgreSQL Concurrency: Data
Modification Language](/blog/2018/06/PostgreSQL-DML.md), it's time for them
to do some actions on the tweets: likes and retweet.

Of course, we're going to put concurrency to the test, so we're going to
have to handle very very popular tweets from the play!

<!--more-->
<!--toc-->

## Modeling for Concurrency

We should have another modeling pass on the *tweet.message* table now. With
what we learned about concurrency in PostgreSQL, it's easy to see that we
won't get anywhere with the current model. Remember when [Donald
Knuth](https://en.wikipedia.org/wiki/Donald_Knuth) said

> _We should forget about small efficiencies, say about 97% of the time:
> premature optimization is the root of all evil. Yet we should not pass up
> our opportunities in that critical 3%._

Database systems have been designed to handle concurrency so that your
application's code doesn't have to. One part for the critical 3% is then
related to concurrent operations, and the one that is impossible to
implement in a both fast and correct way is a concurrent *update* on the
same target row.

In our model here, given how the application works, we know that messages
will get concurrent *update* activity for the *favs* and *rts* counters. So
while the previous model looks correct with respect to *normal forms* — the
counters are dependent on the message's key — we know that concurrent
activity is going to be hard to handle in production.

So here's a smarter version of the *activity* parts of the database model:

~~~ sql
begin;

create type tweet.action_t
    as enum('rt', 'fav', 'de-rt', 'de-fav');

create table tweet.activity
 (
  id          bigserial primary key,
  messageid   bigint not null references tweet.message(messageid),
  datetime    timestamptz not null default now(),
  action      tweet.action_t not null,

  unique(messageid, datetime, action)
 );

commit;
~~~

In this version, the counters have disappeared, replaced by a full record of
the base information needed to compute them. We now have an *activity* list
with a denormalized *ENUM* for possible actions.

To get the *rts* and *favs* counters back from this schema, we count lines
in the *activity* records associated with a given *messageid*:

~~~ sql
  select   count(*) filter(where action = 'rt')
         - count(*) filter(where action = 'de-rt')
         as rts,
           count(*) filter(where action = 'fav')
         - count(*) filter(where action = 'de-fav')
         as favs
    from tweet.activity
         join tweet.message using(messageid)
   where messageid = :id;
~~~

Reading the current counter value has become quite complex when compared to
just adding a column to your query output list. On the other hand, when
adding a *rt* or a *fav* action to a message, we transform the SQL:

~~~ sql
update tweet.message set rts = rts +1 where messageid = :id;
~~~

This is what we use instead:

~~~ sql
insert into tweet.activity(messageid, action) values(:id, 'rt');
~~~

The reason why replacing an *update* with an *insert* is interesting is
concurrency behavior and locking. In the first version, retweeting has to
wait until all concurrent retweets are done, and the business model wants to
sustain as many concurrent activities on the same small set of messages as
possible (read about *influencer* accounts).

The *insert* has no concurrency because it targets a row that doesn't exist
yet. We register each action into its own tuple and require no locking to do
that, allowing our production setup of PostgreSQL to sustain a much larger
load.

Now, computing the counters each time we want to display them is costly. And
the counters are displayed on every tweet message. We need a way to *cache*
that information, and we'll see about that in a follow-up article about
*Computing and Caching in SQL*.

## Putting Concurrency to the Test

When we *benchmark* the concurrency properties of the two statements above,
we quickly realize that the *activity* table is badly designed. The unique
constraint includes a *timestamptz* field, which in PostgreSQL is only
precise down to the microsecond.

This kind of made-up *unique* constraint means we now have these errors to
deal with:

~~~
Error: Database error 23505: duplicate key value violates unique     ⏎
  constraint "activity_messageid_datetime_action_key"
DETAIL: Key (messageid, datetime, action)                            ⏎
           =(2, 2017-09-19 18:00:03.831818+02, rt) already exists.
~~~

The best course of action here is to do this:

~~~ sql
    alter table tweet.activity
drop constraint activity_messageid_datetime_action_key;
~~~

Now we can properly compare the concurrency scaling of the *insert* and the
*update* based version. In case you might be curious about it, here's the
testing code that's been used:

~~~ lisp
(defpackage #:concurrency
  (:use #:cl #:appdev)
  (:import-from #:lparallel
                #:*kernel*
                #:make-kernel #:make-channel
                #:submit-task #:receive-result
                #:kernel-worker-index)
  (:import-from #:cl-postgres-error
                #:database-error)
  (:export      #:*connspec*
                #:concurrency-test))

(in-package #:concurrency)

(defparameter *connspec* '("appdev" "dim" nil "localhost"))

(defparameter *insert-rt*
  "insert into tweet.activity(messageid, action) values($1, 'rt')")

(defparameter *update-rt*
  "update tweet.message set rts = coalesce(rts, 0) + 1 where messageid = $1")

(defun concurrency-test (workers retweets messageid
                         &optional (connspec *connspec*))
  (format t "Starting benchmark for updates~%")
  (with-timing (rts seconds)
      (run-workers workers retweets messageid *update-rt* connspec)
    (format t "Updating took ~f seconds, did ~d rts~%" seconds rts))

  (format t "~%")

  (format t "Starting benchmark for inserts~%")
  (with-timing (rts seconds)
      (run-workers workers retweets messageid *insert-rt* connspec)
    (format t "Inserting took ~f seconds, did ~d rts~%" seconds rts)))

(defun run-workers (workers retweets messageid sql
                    &optional (connspec *connspec*))
  (let* ((*kernel* (lparallel:make-kernel workers))
         (channel  (lparallel:make-channel)))
    (loop repeat workers
       do (lparallel:submit-task channel #'retweet-many-times
                                 retweets messageid sql connspec))

    (loop repeat workers sum (lparallel:receive-result channel))))

(defun retweet-many-times (times messageid sql
                           &optional (connspec *connspec*))
  (pomo:with-connection connspec
    (pomo:query
     (format nil "set application_name to 'worker ~a'"
             (lparallel:kernel-worker-index)))
    (loop repeat times sum (retweet messageid sql))))

(defun retweet (messageid sql)
  (handler-case
      (progn
        (pomo:query sql messageid)
        1)
    (database-error (c)
      (format t "Error: ~a~%" c)
      0)))
~~~

Here's a typical result with a concurrency of 100 workers all wanting to do
10 retweet in a loop using a *messageid*, here message 3. While it's not
representative to have them loop 10 times to retweet the same message, it
should help create the concurrency effect we want to produce, which is
having several concurrent transactions waiting in turn in order to have a
lock access to the same row.

The theory says that those concurrent users will have to wait in line, and
thus spend time waiting for a lock on the PostgreSQL server. We should see
that in the timing reports as a time difference:

~~~ lisp
CL-USER> (concurrency::concurrency-test 100 10 3)
Starting benchmark for updates
Updating took 3.099873 seconds, did 1000 rts

Starting benchmark for inserts
Inserting took 2.132164 seconds, did 1000 rts
~~~

The *update* variant of the test took almost 50% as much time to complete
than the *insert* variant, with this level of concurrency. Given that we
have really simple SQL statements, we can attribute the timing difference to
having had to wait in line. Basically, the *update* version spent almost 1
second out of 3 seconds waiting for a free slot.

In another test with even more concurrency pressure at 50 retweets per
worker, we can show that the results are repeatable:

~~~ lisp
CL-USER> (concurrency::concurrency-test 100 50 6)
Starting benchmark for updates
Updating took 5.070135 seconds, did 5000 rts

Starting benchmark for inserts
Inserting took 3.739505 seconds, did 5000 rts
~~~

<hr />

{{< figure class="right"
             src="/img/TAOP_Book_Cover_200x260.png"
            link="https://theartofpostgresql.com" >}}

This article is extracted from my book [The Art of
PostgreSQL](https://theartofpostgresql.com), which teaches SQL to developers
so that they may replace thousands of lines of code with very simple
queries. The book has a full chapter about *Data Manipulation and
Concurrency Control* in PostgreSQL, including caching with materialized
views, check it out!

<hr />

## Conclusion

If you know that your application has to scale, think about how to avoid
concurrent activity that competes against a single shared resource. Here,
this shared resource is the *rts* field of the *tweet.message* row that you
target, and the concurrency behavior is going to be fine if the retweet
activity is well distributed. As soon as many users want to retweet the same
message, then the *update* solution has a non-trivial scalability impact.

Now, we're going to implement the *tweet.activity* based model. In this
model, the number of *retweets* needs to be computed each time we display
it, and it's part of the visible data. 

Also, in the general case, it's impossible for our users to know for sure
how many retweets have been made so that we can implement a cache with
*eventual consistency* properties in the next article of our series about
concurrency in PostgreSQL.


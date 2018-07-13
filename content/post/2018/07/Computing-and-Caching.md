+++
title = "Computing and Caching"
date = "2018-07-13T13:10:21+02:00"
tags = ["PostgreSQL","YeSQL","Concurrency","Materialized Views"]
categories = ["PostgreSQL","YeSQL"]
coverImage = "/img/cache-memory.jpg"
coverMeta = "in"
coverSize = "partial"
thumbnailImage = "/img/brain-2062051_960_720.png"
thumbnailImagePosition = "left"

+++

Let's continue to dive in PostgreSQL Concurrency. In the previous article of
the seies, [Modeling for
Concurrency](/blog/2018/07/modeling-for-concurrency/), we saw how to model
your application for highly concurrent activity. It was a follow-up to the
article entitled [PostgreSQL Concurrency: Isolation and
Locking](/blog/2018/07/postgresql-concurrency-isolation-and-locking/), which
was a primer on PostgreSQL isolation and locking properties and behaviors.

Today's article takes us a step further and builds on what we did in the
previous articles in our series. After having had all the characters from
Shakespeare's *A Midsummer Night's Dream* tweet their own lines in our
database in [PostgreSQL Concurrency: Data Modification
Language](/blog/2018/06/PostgreSQL-DML.md), and having had them like a
retweet a lot in [PostgreSQL Concurrency: Isolation and
Locking](/blog/2018/07/postgresql-concurrency-isolation-and-locking/), it's
time to think about how to display our counters in an efficient way.

In this article, we're going to think about when we should compute results
and when we should cache them for instant retrieval, all within the SQL
tooling. The SQL tooling for handling cache is a [MATERIALIZED
VIEW](https://www.postgresql.org/docs/current/static/sql-creatematerializedview.html),
and it comes with **cache invalidation** routines, of course.

<!--more-->
<!--toc-->

# Computing and Caching in SQL

There's a pretty common saying:

> _There are only two hard things in computer science: cache invalidation
> and naming things._
>
> — Phil Karlton

More about that saying can be read at the [Two Hard
Things](https://martinfowler.com/bliki/TwoHardThings.html) page from *Martin
Fowler*, who tries to track it back to its origins.

It is time that we see about how to address the cache problems in SQL.
Creating a set of values for caching is of course really easy as it usually
boils down to writing a SQL query. Any SQL query executed by PostgreSQL uses
a snapshot of the whole database system. To create a cache from that
snapshot, the simplest way is to use the *create table as* command.

~~~ sql
create table tweet.counters as
  select   count(*) filter(where action = 'rt')
         - count(*) filter(where action = 'de-rt')
         as rts,
           count(*) filter(where action = 'fav')
         - count(*) filter(where action = 'de-fav')
         as favs
    from tweet.activity
         join tweet.message using(messageid);
~~~

Now we have a *tweet.counters* table that we can use whenever we need the
numbers of *rts* or *favs* from a tweet message. How do we update the
counters? That's the cache invalidation problem quoted above, and we'll come
to the answer by the end of this article!

# Views

Views allow integrating server-side computations in the definition of a
relation. The computing still happens dynamically at query time and is made
transparent to the client. When using a view, there's no problem with *cache
invalidation*, because nothing gets cached away.

~~~ sql
create view tweet.message_with_counters
      as
  select messageid,
         message.userid,
         message.datetime,
         message.message,
           count(*) filter(where action = 'rt')
         - count(*) filter(where action = 'de-rt')
         as rts,
           count(*) filter(where action = 'fav')
         - count(*) filter(where action = 'de-fav')
         as favs,
         message.location,
         message.lang,
         message.url
    from tweet.activity
         join tweet.message using(messageid)
group by message.messageid, activity.messageid;
~~~

Given this view, the application code can query
*tweet.message_with_counters* and process the same relation as in the first
normalized version of our schema. The view hides the *complexity* of how to
obtain the counters from the schema.

~~~ sql
  select messageid,
         rts,
         nickname
    from tweet.message_with_counters
         join tweet.users using(userid)
   where messageid between 1 and 6
order by messageid;
~~~

We can see that I played with generating some retweets in my local testing,
done mainly over the six first messages:

~~~ psql
 messageid │  rts   │   nickname   
═══════════╪════════╪══════════════
         1 │  20844 │ Duke Theseus
         2 │ 111345 │ Hippolyta
         3 │  11000 │ Duke Theseus
         5 │   3500 │ Duke Theseus
         6 │  15000 │ Egeus
(5 rows)
~~~

That view now embeds the computation details and abstracts them away from
the application code. It allows having several parts of the application deal
with the same way of counting *retweets* and *favs*, which might come to be
quite important if you have different backends for reporting, data analysis,
and user analytics products that you're selling, or using it to sell
advertising, maybe. It might even be that those parts are written in
different programming languages, yet they all want to deal with the same
numbers, a shared *truth*.

The view embeds the computation details, and still it computes the result
each time it's referenced in a query.

<hr />

{{< figure class="right"
             src="/img/MasteringPostgreSQLinAppDev-Cover-th.png"
            link="https://masteringpostgresql.com" >}}
            
This article is extracted from my book [Mastering PostgreSQL in Application
Development](https://masteringpostgresql.com), which teaches SQL to
developers so that they may replace thousands of lines of code with very
simple queries. The book has a full chapter about *Data Manipulation and
Concurrency Control* in PostgreSQL, including caching with materialized
views, check it out!

<hr />

# Materialized Views

It is easy enough to cache a snapshot of the database into a permanent
relation for later querying thanks to PostgreSQL implementation of
*materialized views*:

~~~ sql
create schema if not exists twcache;

create materialized view twcache.message
    as select messageid, userid, datetime, message,
              rts, favs,
              location, lang, url
         from tweet.message_with_counters;

create unique index on twcache.message(messageid);
~~~

As usual, read the PostgreSQL documentation about the command [CREATE
MATERIALIZED
VIEW](https://www.postgresql.org/docs/current/static/sql-creatematerializedview.html)
for complete details about the command and its options.

The application code can now query *twcache.message* instead of *tw.message*
and get the extra pre-computed columns for *rts* and *favs* counter. The
information in the materialized view is static: it is only updated with a
specific command. We have effectively implemented a cache in SQL, and now we
have to solve the *cache invalidation* problem: as soon as a new action
(retweet or favorite) happens on a message, our cache is wrong.

Now that we have created the cache, we run another benchmark with 100
workers doing each 100 retweets on *messageid* 3:

~~~ lisp
CL-USER> (concurrency::concurrency-test 100 100 3)
Starting benchmark for updates
Updating took 8.132917 seconds, did 10000 rts

Starting benchmark for inserts
Inserting took 6.684597 seconds, did 10000 rts
~~~

Then we query our cache again:

~~~ sql
  select messageid,
         rts,
         nickname,
         substring(message from E'[^\n]+') as first_line
    from twcache.message
         join tweet.users using(userid)
   where messageid = 3
order by messageid;
~~~

We can see that the *materialized view* is indeed a cache, as it knows
nothing about the last round of retweets that just happened:

~~~ psql
 messageid │ rts  │   nickname   │    first_line    
═══════════╪══════╪══════════════╪══════════════════
         3 │ 1000 │ Duke Theseus │ Go, Philostrate,
(1 row)
~~~

Of course, as every PostgreSQL query uses a database snapshot, the situation
when the counter is already missing actions already happens with a table and
a view already. If some *insert* are *committed* on the *tweet.activity*
table while the *rts* and *favs* count query is running, the result of the
query is not counting the new row, which didn't make it yet at the time when
the query snapshot had been taken. *Materialized view* only extends the
cache *time to live*, if you will, making the problem more obvious.

To invalidate the cache and compute the data again, PostgreSQL implements
the [refresh materialized
view](https://www.postgresql.org/docs/current/static/sql-refreshmaterializedview.html)
command:

~~~ sql
refresh materialized view concurrently twcache.message;
~~~

This command makes it possible to implement a *cache invalidation policy*.
In some cases, a business only analyses data up to the day before, in which
case you can *refresh* your materialized views every night: that's your
cache invalidation policy.

Once the *refresh materialized view* command has been processed, we can
query the cache again. This time, we get the expected answer:

~~~ psql
 messageid │  rts  │   nickname   │    first_line    
═══════════╪═══════╪══════════════╪══════════════════
         3 │ 11000 │ Duke Theseus │ Go, Philostrate,
(1 row)
~~~

In the case of instant messaging such as Twitter, maybe the policy would
require *rts* and *favs* counters to be as fresh as *five minutes ago*
rather than *yesterday*. When the *refresh materialized view* command runs
in less than five minutes then implementing the policy is a matter of
scheduling that command to be executed every five minutes, using for example
the *cron* Unix task scheduler.

# Conclusion

{{< figure class="right" src="/img/there-are-two-hard-problems-in-computer-science-0-naming-320.png" >}}

Once more, PostgreSQL makes it very easy to solve a complex problem.
Managing a cache becomes a matter of using two commands: CREATE MATERIALIZED
VIEW to initialize the cache structure, and then REFRESH MATERIALIZED VIEW
to implement your **cache invalidation policy**. 

Reminder: everywhere you are caching values, have an explicit cache
invalidation policy and a tool to invalidate your cache manually, in case of
emergency cache cleaning situations. Also, your software should be able to
bypass the cache easily. Caching is efficient, and it comes with
complexities that need to be handled.

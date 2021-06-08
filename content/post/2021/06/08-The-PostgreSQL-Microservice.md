+++
title = "PostgreSQL as a Microservice"
date = "2021-06-08T13:40:00+0200"
tags = ["PostgreSQL","YeSQL","Conferences"]
categories = ["PostgreSQL","YeSQL"]
coverImage = "/img/MicroservicesDeathStar.jpg"
coverMeta = "out"
coverSize = "partial"
thumbnailImage = "/img/Daco_4082843.png"
thumbnailImagePosition = "left"

+++

The [MACI](https://www.clever-cloud.com/fr/podcast/) French podcast honoured
me with an invitation to a guest appearance on their weekly schedule. As you
can imagine, we talked about many things related to PostgreSQL... and also
reacted to some newsworthy articles carefully curated by the MACI team. One
of the topics we discussed in the podcast started with looking at PostgreSQL
through the angle of it being one of the microservices that your application
would be composed of.

<hr />

The podcast episode is available as a video, and it's in French.

  <center style="margin-left: 20%; margin-right: 20%;">
    Votre attention s'il vous plaît, le message à caractère informatique suivant
    est delivré en français :
  </center>

{{< youtube NiHEwgCN7y4 >}}

<hr />

I must admit looking at Postgres as a microservice is an exercise that I
like. It allows focusing on what Postgres offers that your application is
happy to re-use rather than having to implement it for itself.

## Postgres is not (just) about storage

At a first glance, the Postgres microservice might look like a storage API
to many developers, and that's how we got started in the podcast. 

I believe that's a mistake though. A storage service would typically host
files encoded as XML, JSON, Parquet, or some other format, and rely on a
get/set API such as the ones found in all the Cloud services nowadays (think
AWS S3, Azure Storage, or GCP Cloud Storage... or if you want to host it
yourself, have a look at [MinIO](https://min.io)).

I believe that Postgres is not a storage service. When RDBMS were invented
in the 70s developers knew how to serialize memory to disk and read from
disk in memory again when needed, of course. Some early file systems already
existed, and even the Unix hierarchichal file system was available. So
that's not what our pioneers set themselves to solve as an architecture
component.

## Postgres is all about concurrency

The Postgres microservice would be the **concurrency** microservice, if that
would ever make sense. A service that maintains a data set with some
guarantees (think ACID, schema, constraints) when it's being accessed in a
read/write capacity by several concurrent actors; and a service that even
allows online schema changes.

As an thought exercise, imagine implementing a network service that allows
your application to insert/update/delete items in a single collection. Let's
target something small, so that it appears to be an easy enough problem to
solve, something like a single collection of about a dozen items. Now, the
idea is to have half--a-dozen users concurrently accesing the collection to
add, remove, or edit items in there. The job is the write a service that is
reliable and predictable in how the changes are implemented when the API is
used by several concurrent users.

If you've been a developper for a little while already, it might be obvious
to you why implementing such a service is already non-trivial. If you think
it's trivial, well, spend a couple days on it and write a concurrent test
suite that makes you feel safe about the concurrency aspects of what you've
done.

Now, expanding on our single collection of items, we may introduce
[relations](https://en.wikipedia.org/wiki/Relation_(database)) and
constraints such as data types and the NOT NULL check, etc. And make it
scale to hundreds of concurrent users and millions of items, or tuples.

## Isolation and Locking

The ability to maintain read/write access to a single data set with
concurrent users is what Postgres implements for you, in a way that you
don't have to think too hard about the concurrency aspects of the database
properties. 

Well, until you need to anyway, as shown in my article [PostgreSQL
Concurrency: Isolation and
Locking](https://tapoueh.org/blog/2018/07/postgresql-concurrency-isolation-and-locking/)
where we dive in the user settings that impacts transaction characteristics.

And sometimes you might want to relax your approach to concurrency in
Postgres and implement a read-only cache that you would only update on a
schedule, rather than at transaction boundaries. That's what my article
series titled [PostgreSQL
Concurrency](https://tapoueh.org/blog/2018/08/postgresql-concurrency-an-article-series/)
is all about, give it a read: the series dives into advanced tooling and
concepts such as Materialized Views, Listen/Notify, and Triggers.

## Say it again: RDBMS are all about concurrency!

These days the RDBMS innovation landscape is shifted towards distributed SQL
and how to handle very large databases. Some aspects require an
[OLAP](https://en.wikipedia.org/wiki/Online_analytical_processing) approach,
others want to still handle
[OLTP](https://en.wikipedia.org/wiki/Online_transaction_processing) systems
of records in the billions scale and more, and more and more we see
[HTAP](https://en.wikipedia.org/wiki/Hybrid_transactional/analytical_processing)
use cases.

HTAP stands for *Hybrid transactional/analytical processing* and is a mixed
approach that attempts at solving both analytical and transactional aspects
of your data production life cycle within a single system. When dealing with
a data set in the Petabyte scale, having to operate more than one copy of
the system becomes somewhat impractical, or at least quite costly.

One of the major difficulties when implementing a distributed SQL solution
is around transaction properties of the system. We like the ACID aspect of
RDBMS because it allows the application code and the developers to focus on
a single user workflow at a time. That's quite wonderful, let's keep it that
way!


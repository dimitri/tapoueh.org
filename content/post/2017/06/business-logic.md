+++
title = "SQL and Business Logic"
date = "2017-06-19T13:30:19+02:00"
tags = ["PostgreSQL","YeSQL","Python","AnoSQL","SQL"]
categories = ["PostgreSQL","YeSQL"]
coverImage = "/img/maze-inside-a-database.jpg"
coverMeta = "out"
coverSize = "partial"
thumbnailImage = "/img/maze-inside-a-database.jpg"
thumbnailImagePosition = "left"

+++

Business logic is *supposed to be* the part of the application where you
deal with customer or user facing decisions and computations. It is often
argued that this part should be well separated from the rest of the
technical infrastructure of your code. Of course, SQL and relational
database design is meant to support your business cases (or user stories),
so then we can ask ourselves if SQL should be part of your business logic
implementation. Or actually, how much of your business logic should be SQL?

<!--more-->

As the database model is meant to support your business activity as a whole,
it's quite easy to see that the SQL schema you are working with already
implements an important layer of your business logic. Its goal is to
implement as much as possible of it, so that when you use the accounting,
the back office and the user facing applications on top of it, they all
share the same understanding of the database and respect the same set of
validity constraints and consistency rules.

My argument then is that every SQL query you send to the server embeds some
business logic. Even those you didn't write because you're using that
awesome ORM tool… but what does your ORM knows exactly about your business
case, or your user stories?

<!--toc-->

# A very simple example

In the following example, we are going to first define a business case we
want to implement, and then have a look at the SQL statement that we would
be using to solve it. We are using
the [Chinook database](https://github.com/lerocha/chinook-database/) again,
it models a music collection of tracks, artists, albums, and genre.

{{< alert info >}}

With the latest [pgloader](http://pgloader.io/) code (yet to be released at
the time of this writing), you can install the Chinook database in a single
command:

~~~ bash
$ createdb chinook
$ pgloader https://github.com/lerocha/chinook-database/raw/master/ChinookDatabase/DataSources/Chinook_Sqlite_AutoIncrementPKs.sqlite pgsql:///chinook
~~~

Well ok a single command line once you've created the target database. I
guess that makes it two commands.

{{< /alert >}}

Let's pick quite a simple user story: display the list of albums from a
given artist, each with its total duration.

In the Chinook model we have a per-track duration field, named
*milliseconds*. Each track is associated with an album through its
*albumid*, and each album is the work of one artist that we reach through
*artistid*. Let's write a query for solving our business case:

~~~ sql
  select album.title as album,
         sum(milliseconds) * interval '1 ms' as duration
    from album
         join artist using(artistid)
         left join track using(albumid)
   where artist.name = 'Red Hot Chili Peppers'
group by album
order by album;
~~~

The output is:

~~~ psql
         album         |           duration           
-----------------------+------------------------------
 Blood Sugar Sex Magik | @ 1 hour 13 mins 57.073 secs
 By The Way            | @ 1 hour 8 mins 49.951 secs
 Californication       | @ 56 mins 25.461 secs
(3 rows)
~~~

What we see here is a direct translation from the business case (or user
story if you prefer that term) into a SQL query. The SQL implementation uses
joins and computations that are specific to both the data model and the use
case we are solving. 

# Business Logic in the Application Code

Now, we could decide that the application's code is where to implement our
business case, because that's easier to maintain in the long run. I wrote a
direct implementation of the Chinook model in Python and then wrote the same
query against the Python model.

The goal of this Python exercise is to obtain classic application's code and
to mimic to some degree the usual layers of abstractions such as found in
ORM librairies. Here goes:

~~~ python
#! /usr/bin/env python3
# -*- coding: utf-8 -*-

import psycopg2
import psycopg2.extras
import sys
from datetime import timedelta

DEBUGSQL = False
PGCONNSTRING = "dbname=chinook application_name=cdstore"


class Model(object):
    tablename = None
    columns = None

    @classmethod
    def buildsql(cls, pgconn, **kwargs): ...

    @classmethod
    def fetchone(cls, pgconn, **kwargs): ...

    @classmethod
    def fetchall(cls, pgconn, **kwargs): ...


class Artist(Model):
    tablename = "artist"
    columns = ["artistid", "name"]
    ...


class Album(Model):
    tablename = "album"
    columns = ["albumid", "title"]
    ...


class Track(Model):
    tablename = "track"
    columns = ["trackid", "name", "milliseconds", "bytes", "unitprice"]
    ...


if __name__ == '__main__':
    if len(sys.argv) > 1:
        pgconn = psycopg2.connect(PGCONNSTRING)
        artist = Artist.fetchone(pgconn, name=sys.argv[1])

        for album in Album.fetchall(pgconn, artistid=artist.id):
            ms = 0
            for track in Track.fetchall(pgconn, albumid=album.id):
                ms += track.duration

            duration = timedelta(milliseconds=ms)
            print("%25s: %s" % (album.title, duration))
    else:
        print('albums.py <artist name>')
~~~

When we run the code we have the same result as before with the query:

~~~ bash
$ ./albums.py "Red Hot Chili Peppers"
    Blood Sugar Sex Magik: 1:13:57.073000
               By The Way: 1:08:49.951000
          Californication: 0:56:25.461000
~~~

What can we learn about those two implementations of the same business case?

# Correctness

In the application's code implementation we are doing 5 queries instead of
one. What happens if an album had been badly assigned to our artist and is
reassigned while our program runs? Well we might fetch the album, but then
fail to find any track associated with it in the next query thus report a
zero duration.

In this business case it might not be very costly, well except if you charge
by album's duration in some kind of library access and you're stuck in the
90s for some reason.

What I mean is that it's easy to transpose this example to your own business
and see if you are subject to correctness issues in your implementation.
Doing several queries without setting a
proper
[Transaction Isolation Level](https://www.postgresql.org/docs/current/static/transaction-iso.html) will
undoubtly open the door of inconsistencies in the information your
application retrieves. Maybe for billing purposes.

# Efficiency

Another problem with the application's code implementation as written is
with its very bad efficiency. As this artists has 3 albums in our data set,
we are doing 5 queries here. In my test environement the query runs in about
2ms on the server… laptop really. The network ping in a real setup is
usually around 1ms or 2ms, so when going from a single 2ms query to 5
simpler queries you are actually adding 4 round-trips to your application.

So for the same result, 5 queries, say 1ms each, with a very good 1ms
round-trip, that's already 10ms instead of 3ms in the single-query case.

Also the Python code now needs to retrieve way more information than needed
and will store and scan that in the local memory. So we are consuming a
disproportionate amount of memory, network latency and network bandwidth
compared to the SQL only solution. Our PostgreSQL server still has to fetch
the same amount of data in its backend's memory, and scan the tracks
*milliseconds* to compute each album's duration. Good chances are that those
data (if considered *hot* in your application) are already cached in memory
tho.

And this is not the only problem. In terms of scalability of your
application, running 5 times as many queries might not turn cheap. Maybe you
don't need this size of a caching layer in front of your API servers after
all…

# Maintenance

In terms of code maitenance we have to compare the SQL with only those 9
lines of Python code:

~~~ python
artist = Artist.fetchone(pgconn, name=sys.argv[1])

for album in Album.fetchall(pgconn, artistid=artist.id):
    ms = 0
    for track in Track.fetchall(pgconn, albumid=album.id):
        ms += track.duration

    duration = timedelta(milliseconds=ms)
    print("%25s: %s" % (album.title, duration))
~~~

With the equivalent query written as:

~~~ sql
  select album.title as album,
         sum(milliseconds) * interval '1 ms' as duration
    from album
         join artist using(artistid)
         left join track using(albumid)
   where artist.name = 'Red Hot Chili Peppers'
group by album
order by album;
~~~

It seems to me that the expected maintenance burden of both the solution are
roughly equivalent. The main difference is that in the second case
PostgreSQL is smart enough to pick the proper join algorithm depending on
the size of the different data sets involved and the defined indexes, where
in the Python code we manually express a *nested loop* and would have to
manually code up a *hash join* or a *merge join* should the data set size
come to require it.

A somewhat interesting maintenance case to consider would be to change the
result ordering. Say now we want to display the album list sorted by album's
duration, shortest first.

{{< alert warning >}}

Exercice to the reader: implement the new ordering specification in the
application's code, where you deal with business logic.

Note that the Chinook database model does not have a per-album duration so
you can't rely on `.orderBy(duration)` or something like that at your ORM
layer.

{{< /alert >}}

In the SQL solution, this translates to changing the *order by* clause of
course, and the change is easy for your peers to review before your commit
makes it to the release branch:

~~~ diff
-order by album;
+order by duration;
~~~

Unfortunately we don't have the album's date of release in the Chinook
database model, that would be the proper ordering here I guess.

# Conclusion

{{< image classes="fig25 right dim-margin"
              src="/img/old/sql-logo.png"
           title="PostgreSQL is YeSQL" >}}
           
Of course SQL is meant to implement *business logic*, and that doesn't mean
you need to resort to Stored Procedure to do so. I find that in too many
cases, modern developers tend to forget about basic application's
architecture and like to pretend they can live with a single application's
programming language in the backend.

So it's time to properly **learn SQL** and use it to its full potential as
part of your backend source code, as we saw
in [How to Write SQL](/blog/2017/06/anonsql.md) previously here. And
remember, ***if you think education is expensive, try ignorance***.

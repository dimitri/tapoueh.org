+++
date = "2013-07-04T15:55:00.000000+02:00"
title = "Simple Case for Pivoting in SQL"
tags = ["PostgreSQL", "tricks", "YeSQL"]
categories = ["PostgreSQL","YeSQL"]
thumbnailImage = "/img/old/transpose-matrix.jpg"
thumbnailImagePosition = "left"
coverImage = "/img/old/transpose-matrix.jpg"
coverSize = "partial"
coverMeta = "out"
aliases = ["/blog/2013/07/04-Simple-case-for-pivoting",
           "/blog/2013/07/04-Simple-case-for-pivoting.html"]
+++

In a recent article 
[Craig Kerstiens](http://www.craigkerstiens.com/) from 
[Heroku](https://postgres.heroku.com/) did demo the really useful
[crosstab](http://www.postgresql.org/docs/current/static/tablefunc.html) extension. That function allows you to 
*pivot* a table so that you
can see the data from different categories in separate columns in the same
row rather than in separate rows. The article from 
***Craig*** is
[Pivoting in Postgres](http://www.craigkerstiens.com/2013/06/27/Pivoting-in-Postgres/).

<center>*Pivoting a matrix, also known as a matrix transposition*</center>

Let's do the same setup as he did, with a table containing some randomly
generated data about hypothetical visits to a web page, say, by date then by
operating system.

~~~
~# create table daily_visits_per_os as
      select date::date,
             b.desc AS TYPE,
             (random() * 10000 + 1)::int AS val
       from generate_series((now() - '100 days'::interval)::date,
                            now()::date,
                            '1 day'::interval) as t(date),
            (SELECT unnest(ARRAY['OSX', 'Windows', 'Linux']) AS DESC) b;
SELECT 303

~# table daily_visits_per_os limit 12;
    date    |  type   | val  
------------+---------+------
 2013-03-26 | OSX     | 1583
 2013-03-26 | Windows | 3075
 2013-03-26 | Linux   |  848
 2013-03-27 | OSX     | 4377
 2013-03-27 | Windows | 7685
 2013-03-27 | Linux   | 9550
 2013-03-28 | OSX     | 3063
 2013-03-28 | Windows | 7920
 2013-03-28 | Linux   | 2760
 2013-03-29 | OSX     | 1873
 2013-03-29 | Windows | 8123
 2013-03-29 | Linux   |  866
(12 rows)
~~~


<center>*Yes, `TABLE` is a real SQL statement from the standard!*</center>

Now that we have some data to play with, what we want is the number of
visits per os as different columns, having a result with 4 columns: the
date, the number of visits using OSX that day, then using Windows, then
using Linux. How to do that in plain SQL?

~~~
~# select date,
          sum(case when type = 'OSX' then val end) as osx,
          sum(case when type = 'Windows' then val end) as windows,
          sum(case when type = 'Linux' then val end) as linux
     from daily_visits_per_os
 group by date
 order by date
    limit 4;

    date    | osx  | windows | linux 
------------+------+---------+-------
 2013-03-26 | 1583 |    3075 |   848
 2013-03-27 | 4377 |    7685 |  9550
 2013-03-28 | 3063 |    7920 |  2760
 2013-03-29 | 1873 |    8123 |   866
(4 rows)
~~~


The other advantage of that solution — 
*apart from being standard SQL* — is
that if you happen to have more than a single row per group for a given
category (after all, the data definition is not normalized here, it's known
as 
[EAV](http://en.wikipedia.org/wiki/Entity%E2%80%93attribute%E2%80%93value_model) and you want to stay away from that as much as possible), then the
`sum(case)` query will work just fine.

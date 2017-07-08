+++
title = "PostgreSQL and the calendar"
date = "2017-06-30T14:35:59+02:00"
tags = ["PostgreSQL","YeSQL","SQL","calendar",
        "date","interval","generate_series"]
categories = ["PostgreSQL","YeSQL"]
coverImage = "/img/mayan-calendar.jpg"
coverMeta = "in"
coverSize = "partial"
thumbnailImage = "/img/Calendar-Time.png"
thumbnailImagePosition = "left"

+++

The modern calendar is a trap for the young engineer's mind. We deal with
the calendar on a daily basis and until exposed to its insanity it's rather
common to think that calendar based computations are easy. That's until
you've tried to do it once. A very good read about how the current calendar
came to be the way it is now is Erik's
Naggum [The Long, Painful History of Time](http://naggum.no/lugm-time.html).

<!--more-->

Fortunately for us developpers we don't have to deal with managing calendar
based computations ourselves: we can rely on PostgreSQL rich set
of
[Date/Time Functions and Operators](https://www.postgresql.org/docs/9.6/static/functions-datetime.html) and
[Data Type Formatting Functions](https://www.postgresql.org/docs/9.6/static/functions-formatting.html) to
do the job for us!

So here's a SQL query that show cases PostgreSQL abilities when it comes to
playing with calendar related data. This query started from a conversation
about week numbers with friends. Week numbers sound easy, you begin with the
first January each year, and every 7 days you increment the week number. You
might even be able to code that as a modulo operation and be done. 

Well, except that weeks always begin on a Monday, and months and years
don't, for crazy historical reasons. Really, we could simplify things down a
lot. One proposal around is to have 13 months of 28 days (4 weeks) each
year, that amounts to 364 days. We're short of one day, maybe two for leap
years: let's have that day off the calendar as an extra festivity day. Now
everything would be very simple, every 1st of the month would be a Monday,
always, for instance.

Anyway, in the real world years don't always begin a Monday, so when do you
count the first week of the year? PostgreSQL documentation tells us:

> By definition, ISO weeks start on Mondays and the first week of a year
> contains January 4 of that year. In other words, the first Thursday of a
> year is in week 1 of that year.

Easy right? Now, rather than trying to compute that yourself in your code,
let's see how to get a decade of week numbers for the first day of the year.
And while at that, add in how many days that year will have in February, as
well as other bits:

~~~ sql
select date::date,
       extract('isodow' from date) as dow,
       to_char(date, 'dy') as day,
       extract('isoyear' from date) as "iso year",
       extract('week' from date) as week,
       extract('day' from
               (date + interval '2 month - 1 day')
              )
        as feb,
       extract('year' from date) as year,
       extract('day' from
               (date + interval '2 month - 1 day')
              ) = 29
       as leap
  from generate_series(date '2000-01-01',
                       date '2010-01-01',
                       interval '1 year')
       as t(date);
~~~

The *generate_series* function returns a set of items, here all the dates of
the first day of the years from the 2000's decade. For each of them we then
compute several calendar based values:

~~~
    date    │ dow │ day │ iso year │ week │ feb │ year │ leap 
════════════╪═════╪═════╪══════════╪══════╪═════╪══════╪══════
 2000-01-01 │   6 │ sat │     1999 │   52 │  29 │ 2000 │ t
 2001-01-01 │   1 │ mon │     2001 │    1 │  28 │ 2001 │ f
 2002-01-01 │   2 │ tue │     2002 │    1 │  28 │ 2002 │ f
 2003-01-01 │   3 │ wed │     2003 │    1 │  28 │ 2003 │ f
 2004-01-01 │   4 │ thu │     2004 │    1 │  29 │ 2004 │ t
 2005-01-01 │   6 │ sat │     2004 │   53 │  28 │ 2005 │ f
 2006-01-01 │   7 │ sun │     2005 │   52 │  28 │ 2006 │ f
 2007-01-01 │   1 │ mon │     2007 │    1 │  28 │ 2007 │ f
 2008-01-01 │   2 │ tue │     2008 │    1 │  29 │ 2008 │ t
 2009-01-01 │   4 │ thu │     2009 │    1 │  28 │ 2009 │ f
 2010-01-01 │   5 │ fri │     2009 │   53 │  28 │ 2010 │ f
(11 rows)
~~~

So by the ISO standard the first of January 2000 is part of week 52, 1999.
That's right. Also that year is a leap year. Well, 2000 I mean, not 1999.

To conclude with our calendar fun, here's a very practical advice when you
use dates in PostgreSQL: make good use of the *interval* data type when
computing date ranges like for example monthly based reporting.

If we want to display all the Formula One races for last quarter (from April
the first, and a quarter lasts 3 month), I would use the following query:

~~~ sql
\set beginning '2017-04-01'
\set months 3

select date, name, drivers.surname as winner
  from races
       left join results
              on results.raceid = races.raceid
             and results.position = 1
       left join drivers using(driverid)
 where date >= date :'beginning'
   and date <   date :'beginning'
              + :months * interval '1 month';
~~~

{{< alert info >}}

Read my article about [How to Write SQL](/blog/2017/06/how-to-write-sql/)
for more details about the `\set` feature of *psql* and how best to
integrate such a query into your application's code.

{{< /alert >}}

As you can see the query is written with two input parameters: the date
beginning the quarter and how many months we are interested into, in case we
then want to do the same thing for a year or a semester. Thanks to using an
explicit range that includes the first date and *excludes* the closing date,
we don't have to compute the number of days of the closing month here.
PostgreSQL is pretty good at that. Just refrain from using *between* which
would include both dates in the range.

And if you're curious here's the result:

~~~
    date    │         name          │  winner  
════════════╪═══════════════════════╪══════════
 2017-04-09 │ Chinese Grand Prix    │ Hamilton
 2017-04-16 │ Bahrain Grand Prix    │ Vettel
 2017-04-30 │ Russian Grand Prix    │ Bottas
 2017-05-14 │ Spanish Grand Prix    │ Hamilton
 2017-05-28 │ Monaco Grand Prix     │ Vettel
 2017-06-11 │ Canadian Grand Prix   │ Hamilton
 2017-06-25 │ Azerbaijan Grand Prix │ ¤
(7 rows)
~~~

You can see that my local copy of
the [historical record of motor racing data](http://ergast.com/mrd/) is not
uptodate and we lack the information for the winner of the *Azerbaijan Grand
Prix*. If you're curious about it, you can download a MySQL dump of the
database and then import it into PostgreSQL thanks
to [pgloader](http://pgloader.io):

~~~ bash
$ createdb f1db
$ pgloader mysql://root@localhost/f1db pgsql:///f1db
$ psql -d f1db -c 'ALTER DATABASE f1db SET search_path TO f1db, public;'
~~~

Have fun with SQL, and stop doing calendar based computations yourself: use
an API you can trust such as PostgreSQL!

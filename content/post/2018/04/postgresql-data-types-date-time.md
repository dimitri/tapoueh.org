+++
title = "PostgreSQL Data Types: Date, Timestamp, and Time Zones"
date = "2018-04-13T00:15:44+02:00"
tags = ["PostgreSQL","YeSQL","Data Types","Date","Time","Timestamp","Time Zone"]
categories = ["PostgreSQL","YeSQL"]
coverImage = "/img/World_Time_Zones_Map.png"
coverMeta = "in"
coverSize = "partial"
thumbnailImage = "/img/world-clock-logo.png"
thumbnailImagePosition = "left"

+++

Continuing our series of [PostgreSQL Data Types](/tags/data-types/) today
we're going to introduce date, timestamp, and interval data types.

PostgreSQL implementation of the calendar is very good, and we're going to
show some mice example about how confusing this matter is. The time zone
notion in particular is mainly a political tool these days, and it makes no
sense on an engineering principle: there's no way to solve time zone
problems from first hand principles!

<!--more-->
<!--toc-->

# Date/Time and Time Zones

Handling dates and time and time zones is a very complex matter, and on this
topic, you can read Erik Naggum's piece [The Long, Painful History of
Time](http://naggum.no/lugm-time.html).

The PostgreSQL documentation chapters with the titles [Date/Time
Types](https://www.postgresql.org/docs/current/static/datatype-datetime.html),
[Data Type Formatting
Functions](https://www.postgresql.org/docs/current/static/functions-formatting.html),
and [Date/Time Functions and
Operators](https://www.postgresql.org/docs/current/static/functions-datetime.html)
cover all you need to know about date, time, timestamps, and time zones with
PostgreSQL.

The first question we need to answer here is about using timestamps with or
without *time zones* from our applications. The answer is simple: always use
*timestamps WITH time zones*. 

A common myth is that storing time zones will certainly add to your storage
and memory footprint. It's actually not the case:

~~~ sql
select pg_column_size(timestamp without time zone 'now'),
       pg_column_size(timestamp with time zone 'now');
~~~
~~~ psql
 pg_column_size │ pg_column_size 
════════════════╪════════════════
              8 │              8
(1 row)
~~~

PostgreSQL defaults to using *bigint* internally to store timestamps, and
the on-disk and in-memory format are the same with or without time zone
support. Here's their whole type definition in the PostgreSQL source code
(in `src/include/datatype/timestamp.h`):

~~~ c
typedef int64 Timestamp;
typedef int64 TimestampTz;
~~~

From the PostgreSQL documentation for timestamps, here's how it works:

> For timestamp with time zone, the internally stored value is always in UTC
> (Universal Coordinated Time, traditionally known as Greenwich Mean Time,
> GMT). An input value that has an explicit time zone specified is converted
> to UTC using the appropriate offset for that time zone. If no time zone is
> stated in the input string, then it is assumed to be in the time zone
> indicated by the system's TimeZone parameter, and is converted to UTC
> using the offset for the timezone zone.

PostgreSQL doesn't store the time zone they come from with your timestamp.
Instead it converts to and from the input and output timezone much like
we've seen for text with *client_encoding*.

~~~ sql
begin;

drop table if exists tstz;

create table tstz(ts timestamp, tstz timestamptz);

set timezone to 'Europe/Paris';
select now();
insert into tstz values(now(), now());

set timezone to 'Pacific/Tahiti';
select now();
insert into tstz values(now(), now());

set timezone to 'Europe/Paris';
table tstz;

set timezone to 'Pacific/Tahiti';
table tstz;

commit;
~~~

In this script, we play with the client's setting *timezone* and change from
a French value to another French value, as Tahiti is an island in the
Pacific that is part of France. Here's the full output as seen when running
this script, when launched with `psql -a -f tz.sql`:

~~~ psql
BEGIN
...
set timezone to 'Europe/Paris';
SET
select now();
              now              
═══════════════════════════════
 2017-08-19 14:22:11.802755+02
(1 row)

insert into tstz values(now(), now());
INSERT 0 1
set timezone to 'Pacific/Tahiti';
SET
select now();
              now              
═══════════════════════════════
 2017-08-19 02:22:11.802755-10
(1 row)

insert into tstz values(now(), now());
INSERT 0 1
set timezone to 'Europe/Paris';
SET
table tstz;
             ts             │             tstz              
════════════════════════════╪═══════════════════════════════
 2017-08-19 14:22:11.802755 │ 2017-08-19 14:22:11.802755+02
 2017-08-19 02:22:11.802755 │ 2017-08-19 14:22:11.802755+02
(2 rows)

set timezone to 'Pacific/Tahiti';
SET
table tstz;
             ts             │             tstz              
════════════════════════════╪═══════════════════════════════
 2017-08-19 14:22:11.802755 │ 2017-08-19 02:22:11.802755-10
 2017-08-19 02:22:11.802755 │ 2017-08-19 02:22:11.802755-10
(2 rows)

commit;
COMMIT
~~~

First, we see that the *now()* function always returns the same timestamp
within a single transaction. If you want to see the clock running while in a
transaction, use the *clock_timestamp()* function instead.

Then, we see that when we change the *timezone* client setting, PostgreSQL
outputs timestamps as expected, in the selected timezone. If you manage an
application with users in different time zones and you want to display time
in their own local preferred time zone, then you can *set timezone* in your
application code before doing any timestamp related processing, and have
PostgreSQL do all the hard work for you.

Finally, when selecting back from the *tstz* table, we see that the column
*tstz* realizes that both the inserted values actually are the same point in
time, but seen from different places in the world, whereas the *ts* column
makes it impossible to compare the entries and realize they actually
happened at exactly the same time.

As said before, even when using timestamps *with* time zone, PostgreSQL will
not store the time zone in use at input time, so there's no way from our
*tstz* table to know that the entries are at the same time but just from
different places.

The opening of this section links to [The Long, Painful History of
Time](http://naggum.no/lugm-time.html), and if you didn't read it yet, maybe
now is a good time. Allow me to quote a relevant part of the article here:

> _The basic problem with time is that we need to express both time and
> place whenever we want to place some event in time and space, yet we tend
> to assume spatial coordinates even more than we assume temporal
> coordinates, and in the case of time in ordinary communication, it is
> simply left out entirely. Despite the existence of time zones and strange
> daylight saving time regimes around the world, most people are blithely
> unaware of their own time zone and certainly of how it relates to standard
> references. Most people are equally unaware that by choosing a notation
> that is close to the spoken or written expression of dates, they make it
> meaningless to people who may not share the culture, but can still read
> the language. It is unlikely that people will change enough to put these
> issues to rest, so responsible computer people need to address the issues
> and resist the otherwise overpowering urge to abbreviate and drop
> context._

Several options are available to input timestamp values in PostgreSQL. The
easiest is to use the ISO format, so if your application's code allows that
you're all set. In the following example we leave the time zone out, as
usually, it's handled by the *timezone* session parameter, as seen above. If
you need to, of course, you can input the time zone in the timestamp values
directly:

~~~ sql
select timestamptz '2017-01-08 04:05:06',
       timestamptz '2017-01-08 04:05:06+02';
~~~

At insert or update time, use the same literal strings without the type
decoration: PostgreSQL already knows the type of the target column, and it
uses that to parse the values literal in the DML statement.

Some application use-cases only need the date. Then use the *date* data type
in PostgreSQL. It is of course then possible to compare a *date* and a
*timestamp with time zone* in your SQL queries, and even to append a time
offset on top of your date to construct a *timestamp*.

# Time Intervals

PostgreSQL implements an *interval* data type along with the *time*, *date*
and *timestamptz* data types. An *interval* describes a duration, like a
month or two weeks, or even a millisecond:

~~~ sql
set intervalstyle to postgres;

select interval '1 month',
       interval '2 weeks',
       2 * interval '1 week',
       78389 * interval '1 ms';
~~~

The default PostgreSQL output looks like this:

~~~ psql
 interval │ interval │ ?column? │   ?column?   
══════════╪══════════╪══════════╪══════════════
 1 mon    │ 14 days  │ 14 days  │ 00:01:18.389
(1 row)
~~~

Several *intervalstyle* values are possible, and the setting
*postgres_verbose* is quite nice for interactive *psql* sessions:

~~~ sql
set intervalstyle to postgres_verbose;

select interval '1 month',
       interval '2 weeks',
       2 * interval '1 week',
       78389 * interval '1 ms';
~~~

This time we get a user-friendly output:

~~~ psql
 interval │ interval  │ ?column?  │      ?column?       
══════════╪═══════════╪═══════════╪═════════════════════
 @ 1 mon  │ @ 14 days │ @ 14 days │ @ 1 min 18.389 secs
(1 row)
~~~

How long is a month? Well, it depends on which month, and PostgreSQL knows
that:

~~~ sql
select d::date as month,
       (d + interval '1 month' - interval '1 day')::date as month_end,
       (d + interval '1 month')::date as next_month,
       (d + interval '1 month')::date - d::date as days

  from generate_series(
                       date '2017-01-01',
                       date '2017-12-01',
                       interval '1 month'
                      )
       as t(d);
~~~

When you attach an *interval* to a date or timestamp in PostgreSQL then the
number of days in that interval adjusts to the specific calendar entry
you've picked. Otherwise, an interval of a month is considered to be 30
days. Here we see that computing the last day of February is very easy:

~~~ psql
   month    │ month_end  │ next_month │ days 
════════════╪════════════╪════════════╪══════
 2017-01-01 │ 2017-01-31 │ 2017-02-01 │   31
 2017-02-01 │ 2017-02-28 │ 2017-03-01 │   28
 2017-03-01 │ 2017-03-31 │ 2017-04-01 │   31
 2017-04-01 │ 2017-04-30 │ 2017-05-01 │   30
 2017-05-01 │ 2017-05-31 │ 2017-06-01 │   31
 2017-06-01 │ 2017-06-30 │ 2017-07-01 │   30
 2017-07-01 │ 2017-07-31 │ 2017-08-01 │   31
 2017-08-01 │ 2017-08-31 │ 2017-09-01 │   31
 2017-09-01 │ 2017-09-30 │ 2017-10-01 │   30
 2017-10-01 │ 2017-10-31 │ 2017-11-01 │   31
 2017-11-01 │ 2017-11-30 │ 2017-12-01 │   30
 2017-12-01 │ 2017-12-31 │ 2018-01-01 │   31
(12 rows)
~~~

# Conclusion

PostgreSQL's implementation of the calendar is very good, so use it!

{{< figure class="right"
             src="/img/MasteringPostgreSQLinAppDev-Cover-th.png"
            link="https://masteringpostgresql.com" >}}

This article is an extract from my book [Mastering PostgreSQL in Application
Development](https://masteringpostgresql.com), which teaches SQL to
developers so that they may replace thousands of lines of code with very
simple queries. The book has a full chapter about data types in PostgreSQL,
check it out!

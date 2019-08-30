+++
title = "PostgreSQL Data Types: Date and Time Processing"
date = "2018-04-13T13:35:47+02:00"
tags = ["PostgreSQL","YeSQL","Data Types","Date","Time","Timestamp","Time Zone"]
categories = ["PostgreSQL","YeSQL"]
coverImage = "/img/World_Time_Zones_Map.png"
coverMeta = "in"
coverSize = "partial"
thumbnailImage = "/img/cog-256.png"
thumbnailImagePosition = "left"

+++

Continuing our series of [PostgreSQL Data Types](/tags/data-types/) today
we're going to introduce date and time based processing functions.

Once the application's data, or rather the user data is properly stored as
timestamp with time zone, PostgreSQL allows implementing all the processing
you need to. In this article we dive into a set of examples to help you get
started with time based processing in your database. Can we boost your
reporting skills?

<!--more-->
<!--toc-->

## Loading a Data Set: Git History

As an example data set this time we're playing with *git* history. The
PostgreSQL and pgloader project history have been loaded into the
*commitlog* table thanks to the *git log* command, with a custom format, and
some post-processing — properly splitting up the commit's subjects and
escaping its content. Here's for example the most recent commit registered
in our local *commitlog* table:

~~~ sql
  select project, hash, author, ats, committer, cts, subject
    from commitlog
   where project = 'postgres'
order by ats desc
   limit 1;
~~~

The column names *ats* and *cts* respectively stand for *author commit
timestamp* and *committer commit timestamp*, and the *subject* is the first
line of the commit message, as per the *git log* format *%s*.

To get the most recent entry from a table we *order by* dates in
*descending* order then *limit* the result set to a single entry, and we get
a single line of output:

~~~ psql
─[ RECORD 1 ]───────────────────────────────────────
project   │ postgres
hash      │ 65a69dfa08e212556d11e44a5a8a1861fd826ccd
author    │ Tom Lane
ats       │ 2018-04-13 00:39:51+02
committer │ Tom Lane
cts       │ 2018-04-13 00:39:51+02
subject   │ Fix bogus affix-merging code.
~~~

## Time based statistics

With timestamps, we can compute time-based reporting, such as how many
commits each project received each year in their whole history:

~~~ sql
  select extract(year from ats) as year,
         count(*) filter(where project = 'postgres') as postgres,
         count(*) filter(where project = 'pgloader') as pgloader
    from commitlog
group by year
order by year;
~~~

As we have only loaded two projects in our *commitlog* table, the output is
better with a *pivot* query. We can see more than 20 years of sustained
activity for the PostgreSQL project, and a less active project for pgloader:

~~~ psql
 year │ postgres │ pgloader 
══════╪══════════╪══════════
 1996 │      876 │        0
 1997 │     1698 │        0
 1998 │     1744 │        0
 1999 │     1788 │        0
 2000 │     2535 │        0
 2001 │     3061 │        0
 2002 │     2654 │        0
 2003 │     2416 │        0
 2004 │     2548 │        0
 2005 │     2418 │        0
 2006 │     2153 │        0
 2007 │     2188 │        0
 2008 │     1651 │        0
 2009 │     1389 │        0
 2010 │     1800 │        0
 2011 │     2030 │        0
 2012 │     1605 │        0
 2013 │     1368 │      385
 2014 │     1745 │      367
 2015 │     1815 │      202
 2016 │     2087 │      136
 2017 │     2469 │      193
 2018 │      765 │       40
(23 rows)
~~~

We can also build a reporting on the repartition of commits by weekday from
the beginning of the project, in order to guess if contributors are working
on the project on the job only, or mostly during their free time (weekend).

~~~ sql
  select extract(isodow from ats) as dow,
         to_char(ats, 'Day') as day,
         count(*) as commits,
         round(100.0*count(*)/sum(count(*)) over(), 2) as pct,
         repeat('■', (100*count(*)/sum(count(*)) over())::int) as hist
    from commitlog
   where project = 'postgres'
group by dow, day
order by dow;
~~~

It seems that our PostgreSQL committers tend to work whenever they feel like
it, but less so on the weekend. The project's lucky enough to have a solid
team of committers being paid to work on PostgreSQL:

~~~ psql
 dow │    day    │ commits │  pct  │       hist       
═════╪═══════════╪═════════╪═══════╪══════════════════
   1 │ Monday    │    6746 │ 15.06 │ ■■■■■■■■■■■■■■■
   2 │ Tuesday   │    7376 │ 16.46 │ ■■■■■■■■■■■■■■■■
   3 │ Wednesday │    6759 │ 15.09 │ ■■■■■■■■■■■■■■■
   4 │ Thursday  │    7357 │ 16.42 │ ■■■■■■■■■■■■■■■■
   5 │ Friday    │    7276 │ 16.24 │ ■■■■■■■■■■■■■■■■
   6 │ Saturday  │    4855 │ 10.84 │ ■■■■■■■■■■■
   7 │ Sunday    │    4434 │  9.90 │ ■■■■■■■■■■
(7 rows)
~~~

## Time Differences and Percentiles

Another report we can build compares the author commit timestamp with the
committer commit timestamp. Those are different, but by how much?

~~~ sql
with perc_arrays as
  (
       select project,
              avg(cts-ats) as average,
              percentile_cont(array[0.5, 0.9, 0.95, 0.99])
                 within group(order by cts-ats) as parr
         from commitlog
        where ats <> cts
    group by project
  )
 select project, average,
        parr[1] as median,
        parr[2] as "%90th",
        parr[3] as "%95th",
        parr[4] as "%99th"
   from perc_arrays;
~~~

Here's a detailed output of the time difference statistics, per project:

~~~ psql
─[ RECORD 1 ]─────────────────────────────────────
project │ pgloader
average │ @ 4 days 12 hours 43 mins 10.220859 secs
median  │ @ 4 mins 50 secs
%90th   │ @ 21 hours 49 mins 23.8 secs
%95th   │ @ 24 days 38 hours 54.5 secs
%99th   │ @ 163 days 35 hours 37 mins 40.84 secs
═[ RECORD 2 ]═════════════════════════════════════
project │ postgres
average │ @ 1 day 18 hours 48 mins 36.053773 secs
median  │ @ 2 mins 11 secs
%90th   │ @ 3 hours 27 mins 43 secs
%95th   │ @ 3 days 6 hours 1 min 31.2 secs
%99th   │ @ 49 days 22 hours 40 mins 59.84 secs
~~~

## Time Based Reporting

Reporting is a strong use case for SQL. Application will also send more
classic queries. We can show the commits for the PostgreSQL project for the
12th of April 2018:

~~~ sql
\set day '2018-04-12'

  select ats::time,
         substring(hash from 1 for 8) as hash,
         substring(subject from 1 for 40) || '…' as subject
    from commitlog
   where project = 'postgres'
     and ats >= date :'day'
     and ats  < date :'day' + interval '1 day'
order by ats;
~~~

It's tempting to use the *between* SQL operator, but we would then have to
remember that *between* includes both its lower and upper bound and we would
then have to compute the upper bound as the very last instant of the day.
Using explicit *greater than or equal* and *less than* operators makes it
possible to always compute the very first time of the day, which is easier,
and well supported by PostgreSQL.

Also, using explicit bound checks allows us to use a single date literal in
the query, so that's a single parameter to send from the application.

~~~ psql
   ats    │   hash   │                  subject                  
══════════╪══════════╪═══════════════════════════════════════════
 00:11:29 │ d1e90792 │ Ignore nextOid when replaying an ONLINE …
 02:27:12 │ 9e9befac │ Set relispartition correctly for index p…
 12:02:45 │ c9c875a2 │ Rename IndexInfo.ii_KeyAttrNumbers array…
 12:22:56 │ 08ea7a22 │ Revert MERGE patch…
 15:37:22 │ c266ed31 │ Cleanup covering infrastructure…
 16:25:13 │ 52405459 │ Fix interference between covering indexe…
 16:38:48 │ 3e110a37 │ Fix YA parallel-make hazard, this one in…
 20:08:10 │ a4d56f58 │ Use the right memory context for partkey…
 21:12:06 │ 2fe97771 │ YA attempt to stabilize the results of t…
 21:51:55 │ 181ccbb5 │ Add comment about default partition in c…
 21:53:27 │ b8ca984b │ Revert lowering of lock level for ATTACH…
(11 rows)
~~~

## Date and Time Formatting

Many [data type formatting
functions](https://www.postgresql.org/docs/current/static/functions-formatting.html)
are available in PostgreSQL. In the previous query, although we chose to
*cast* our timestamp with time zone entry down to a *time* value, we could
have chosen another representation thanks to the *to_char* function:

~~~ sql
set lc_time to 'fr_FR';

  select to_char(ats, 'TMDay TMDD TMMonth, HHam') as time,
         substring(hash from 1 for 8) as hash,
         substring(subject from 1 for 40) || '…' as subject
    from commitlog
   where project = 'postgres'
     and ats >= date :'day'
     and ats  < date :'day' + interval '1 day'
order by ats;
~~~

And this time we have a French localized output for the time value:

~~~ psql
         time         │   hash   │                  subject                  
══════════════════════╪══════════╪═══════════════════════════════════════════
 Jeudi 12 Avril, 12am │ d1e90792 │ Ignore nextOid when replaying an ONLINE …
 Jeudi 12 Avril, 02am │ 9e9befac │ Set relispartition correctly for index p…
 Jeudi 12 Avril, 12pm │ c9c875a2 │ Rename IndexInfo.ii_KeyAttrNumbers array…
 Jeudi 12 Avril, 12pm │ 08ea7a22 │ Revert MERGE patch…
 Jeudi 12 Avril, 03pm │ c266ed31 │ Cleanup covering infrastructure…
 Jeudi 12 Avril, 04pm │ 52405459 │ Fix interference between covering indexe…
 Jeudi 12 Avril, 04pm │ 3e110a37 │ Fix YA parallel-make hazard, this one in…
 Jeudi 12 Avril, 08pm │ a4d56f58 │ Use the right memory context for partkey…
 Jeudi 12 Avril, 09pm │ 2fe97771 │ YA attempt to stabilize the results of t…
 Jeudi 12 Avril, 09pm │ 181ccbb5 │ Add comment about default partition in c…
 Jeudi 12 Avril, 09pm │ b8ca984b │ Revert lowering of lock level for ATTACH…
(11 rows)
~~~

## Conclusion

Take some time to familiarize yourself with the time and date support that
PostgreSQL comes with out of the box. Some very useful functions such as
*date_trunc()* are not shown here, and you also will find more gems.

While most programming languages nowadays include the same kind of feature
set, having this processing feature set right in PostgreSQL makes sense in
several use cases:

  - It makes sense when the SQL logic or filtering you want to implement
    depends on the result of the processing (e.g. grouping by week).
    
  - When you have several applications using the same logic, it's often
    easier to share a SQL query than to set up a distributed service API
    offering the same result in XML or JSON (a data format you then have to
    parse).
    
  - When you want to reduce your run-time dependencies, it's a good idea to
    understand how much each architecture layer is able to support in your
    implementation.

This article is an extract from my book [The Art of
PostgreSQL](https://theartofpostgresql.com), which teaches SQL to developers
so that they may replace thousands of lines of code with very simple
queries. The book has a full chapter about data types in PostgreSQL, check
it out!

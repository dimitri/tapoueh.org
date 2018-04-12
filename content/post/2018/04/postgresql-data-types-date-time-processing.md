+++
title = "PostgreSQL Data Types: Date and Time Processing"
date = "2018-04-13T00:30:26+02:00"
tags = ["PostgreSQL","YeSQL","Data Types","Date","Time","Timestamp","Time Zone"]
categories = ["PostgreSQL","YeSQL"]
coverImage = "/img/World_Time_Zones_Map.png"
coverMeta = "in"
coverSize = "partial"
thumbnailImage = "/img/cog-256.png"
thumbnailImagePosition = "left"
draft = "True"

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

# Loading a Data Set: Git History

As an example data set this time we're playing with *git* history. The
PostgreSQL and pgloader project history have been loaded into the
*commitlog* table thanks to the *git log* command, with a custom format, and
some post-processing — properly splitting up the commit's subjects and
escaping its content. Here's for example the most recent commit registered
in our local *commitlog* table:

~~~ sql
  select project, hash, author, ats, committer, cts, subject
    from commitlog
   where project = 'postgresql'
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
─[ RECORD 1 ]────────────────────────────────────────────────────────────────
project   │ postgresql
hash      │ b1c2d76a2fcef812af0be3343082414d401909c8
author    │ Tom Lane
ats       │ 2017-08-19 19:39:37+02
committer │ Tom Lane
cts       │ 2017-08-19 19:39:51+02
subject   │ Fix possible core dump in parallel restore when using a TOC list.
~~~

# Time based statistics

With timestamps, we can compute time-based reporting, such as how many
commits each project received each year in their whole history:

~~~ sql
  select extract(year from ats) as year,
         count(*) filter(where project = 'postgresql') as postgresql,
         count(*) filter(where project = 'pgloader') as pgloader
    from commitlog
group by year
order by year;
~~~

As we have only loaded two projects in our *commitlog* table, the output is
better with a *pivot* query. We can see more than 20 years of sustained
activity for the PostgreSQL project, and a less active project for pgloader:

~~~ psql
 year │ postgresql │ pgloader 
══════╪════════════╪══════════
 1996 │        876 │        0
 1997 │       1698 │        0
 1998 │       1744 │        0
 1999 │       1788 │        0
 2000 │       2535 │        0
 2001 │       3061 │        0
 2002 │       2654 │        0
 2003 │       2416 │        0
 2004 │       2548 │        0
 2005 │       2418 │        3
 2006 │       2153 │        3
 2007 │       2188 │       42
 2008 │       1651 │       63
 2009 │       1389 │        3
 2010 │       1800 │       29
 2011 │       2030 │        2
 2012 │       1605 │        2
 2013 │       1368 │      385
 2014 │       1745 │      367
 2015 │       1815 │      202
 2016 │       2086 │      136
 2017 │       1721 │      142
(22 rows)
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
   where project = 'postgresql'
group by dow, day
order by dow;
~~~

It seems that our PostgreSQL committers tend to work whenever they feel like
it, but less so on the weekend. The project's lucky enough to have a solid
team of committers being paid to work on PostgreSQL:

~~~ psql
 dow │    day    │ commits │  pct  │       hist        
═════╪═══════════╪═════════╪═══════╪═══════════════════
   1 │ Monday    │    6552 │ 15.14 │ ■■■■■■■■■■■■■■■
   2 │ Tuesday   │    7164 │ 16.55 │ ■■■■■■■■■■■■■■■■■
   3 │ Wednesday │    6477 │ 14.96 │ ■■■■■■■■■■■■■■■
   4 │ Thursday  │    7061 │ 16.31 │ ■■■■■■■■■■■■■■■■
   5 │ Friday    │    7008 │ 16.19 │ ■■■■■■■■■■■■■■■■
   6 │ Saturday  │    4690 │ 10.83 │ ■■■■■■■■■■■
   7 │ Sunday    │    4337 │ 10.02 │ ■■■■■■■■■■
(7 rows)
~~~

# Time Differences and Percentiles

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
─[ RECORD 1 ]───────────────────────────────────
project │ pgloader
average │ @ 4 days 22 hours 7 mins 41.18 secs
median  │ @ 5 mins 21.5 secs
%90th   │ @ 1 day 20 hours 49 mins 49.2 secs
%95th   │ @ 25 days 15 hours 53 mins 48.15 secs
%99th   │ @ 169 days 24 hours 33 mins 26.18 secs
═[ RECORD 2 ]═══════════════════════════════════
project │ postgres
average │ @ 1 day 10 hours 15 mins 9.706809 secs
median  │ @ 2 mins 4 secs
%90th   │ @ 1 hour 46 mins 13.5 secs
%95th   │ @ 1 day 17 hours 58 mins 7.5 secs
%99th   │ @ 40 days 20 hours 36 mins 43.1 secs
~~~

# Time Based Reporting

Reporting is a strong use case for SQL. Application will also send more
classic queries. We can show the commits for the PostgreSQL project for the
1st of June 2017:

~~~ sql
\set day '2017-06-01'

  select ats::time,
         substring(hash from 1 for 8) as hash,
         substring(subject from 1 for 40) || '…' as subject
    from commitlog
   where project = 'postgresql'
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
 01:39:27 │ 3d79013b │ Make ALTER SEQUENCE, including RESTART, …
 02:03:10 │ 66510455 │ Modify sequence catalog tuple before inv…
 04:35:33 │ de492c17 │ doc: Add note that DROP SUBSCRIPTION dro…
 19:32:55 │ e9a3c047 │ Always use -fPIC, not -fpic, when buildi…
 23:45:53 │ f112f175 │ Fix typo…
(5 rows)
~~~

# Date and Time Formatting

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
   where project = 'postgresql'
     and ats >= date :'day'
     and ats  < date :'day' + interval '1 day'
order by ats;
~~~

And this time we have a French localized output for the time value:

~~~ psql
        time         │   hash   │                  subject                  
═════════════════════╪══════════╪═══════════════════════════════════════════
 Jeudi 01 Juin, 01am │ 3d79013b │ Make ALTER SEQUENCE, including RESTART, …
 Jeudi 01 Juin, 02am │ 66510455 │ Modify sequence catalog tuple before inv…
 Jeudi 01 Juin, 04am │ de492c17 │ doc: Add note that DROP SUBSCRIPTION dro…
 Jeudi 01 Juin, 07pm │ e9a3c047 │ Always use -fPIC, not -fpic, when buildi…
 Jeudi 01 Juin, 11pm │ f112f175 │ Fix typo…
(5 rows)
~~~

# Conclusion

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

{{< figure class="right"
             src="/img/MasteringPostgreSQLinAppDev-Cover-th.png"
            link="https://masteringpostgresql.com" >}}

This article is an extract from my book [Mastering PostgreSQL in Application
Development](https://masteringpostgresql.com), which teaches SQL to
developers so that they may replace thousands of lines of code with very
simple queries. The book has a full chapter about data types in PostgreSQL,
check it out!

+++
title = "Mastering PostgreSQL in Application Development launches!"
date = "2017-11-06T10:48:43+01:00"
tags = ["PostgreSQL","SQL","book"]
categories = ["PostgreSQL","book"]
coverImage = "/img/elephant.jpg"
coverMeta = "in"
coverSize = "partial"
thumbnailImage = "/img/tapoueh-logo.png"
thumbnailImagePosition = "left"

+++

Today is the day my book [Mastering PostgreSQL in Application
Development](http://masteringpostgresql.com) launches! I'm all excited that
everybody interested is now able to actually read my book!

*Mastering PostgreSQL in Application Development* targets application
developers who want to learn SQL properly, and actually master this
programming language. Most developers don't think of SQL as a programming
language, mainly because they don't have full control of the execution plan
of their queries.

<center>

{{< figure src="/img/MasteringPostgreSQLinAppDev-Cover-th.png"
          link="https://masteringpostgresql.com"
          title="Buy it online!" >}}

</center>

When using SQL, you don't get to write how to fetch the data you are
interested in, your job is to **describe** the data set you need in the
simplest possible way. Sometimes this is pretty easy: I want all the
information we have in the table *drivers* about the driver whom *driverid*
is *1* translates easily to

~~~ sql
   select driverid, driverref, number, code,
          forename, surname, dob, nationality, url
     from drivers
    where driverid = 1;
~~~

In this case we get the following result:

~~~
─[ RECORD 1 ]────────────────────────────────────────────
driverid    │ 1
driverref   │ hamilton
number      │ 44
code        │ HAM
forename    │ Lewis
surname     │ Hamilton
dob         │ 1985-01-07
nationality │ British
url         │ http://en.wikipedia.org/wiki/Lewis_Hamilton
~~~

In some other cases, describing the data set we are interested in is more
complex. If we want the top three drivers in terms of races won, by decade,
this is more work. We still need to describe the result set. PostgreSQL has
the job of coming up with the best solution possible to actually fetch the
data.

The best plan to fetch the data depends on the size of every table involved,
and also depends on the data itself: if some data in your query restriction
clauses is known to be rare in the dataset, then chances are we should use
an index to fetch it… but if it's pretty common, an index will only slow us
down.

The top-three per decade query looks like the following:

~~~ sql
with decades as
(
   select extract('year' from date_trunc('decade', date)) as decade
     from races
 group by decade
)
select decade,
       rank() over(partition by decade
                   order by wins desc)
       as rank,
       forename, surname, wins

  from decades
       left join lateral
       (
          select code, forename, surname, count(*) as wins
            from drivers

                 join results
                   on results.driverid = drivers.driverid
                  and results.position = 1

                 join races using(raceid)

           where   extract('year' from date_trunc('decade', races.date))
                 = decades.decade

        group by decades.decade, drivers.driverid
        order by wins desc
           limit 3
       )
       as winners on true

order by decade asc, wins desc;
~~~

How to implement a top-N query in SQL is explained in details in [Mastering
PostgreSQL in Application Development](http://masteringpostgresql.com), and
you will find this very query and its result set too!

Once you've read the book and practiced enough, then writing such a query is
going to be no sweat for you. Just think of how many lines of application
code (Python, PHP, Java, Go, or something else) you would have to write in
order to obtain the same result? How much data would you have to fetch over
the network from the database server? This query looks complex. The code you
would have to write when you don't know how to write this query would be
even more complex, and I bet, much less efficient.

Finally, here's the Table of Contents of [Mastering PostgreSQL in
Application Development](http://masteringpostgresql.com):

  1. Preface
  2. Introduction
       1. Some of the Code is Written in SQL
       2. A First Use Case
       3. Software Architecture
       4. Getting Ready to read this Book
  3. Writing Sql Queries 
       1. Business Logic
       2. A Small Application
       3. The SQL REPL — An Interactive Setup 
       4. SQL is Code
       5. Indexing Strategy 
       6. An Interview with Yohann Gabory
  4. SQL Toolbox
       1. Get Some Data
       2. Structured Query Language
       3. Queries, DML, DDL, TCL, DCL
       4. Select, From, Where
       5. Order By, Limit, No Offset
       6. Group By, Having, With, Union All
       7. Understanding Nulls
       8. Understanding Window Functions
       9. Understanding Relations and Joins
       10. An Interview with Markus Winand
  5. Data Types 
       1. Serialization and Deserialization
       2. Some Relational Theory
       3. PostgreSQL Data Types
       4. Denormalized Data Types 
       5. PostgreSQL Extensions
       6. An interview with Grégoire Hubert
  6. Data Modeling 
       1. Object Relational Mapping
       2. Tooling for Database Modeling
       3. Normalization
       4. Practical Use Case: Geonames 
       5. Modelization Anti-Patterns
       6. Denormalization
       7. Not Only SQL
       8. An interview with Álvaro Hernández Tortosa
  7. Data Manipulation and Concurrency Control 
       1. Another Small Application
       2. Insert, Update, Delete
       3. Isolation and Locking 
       4. Computing and Caching in SQL 
       5. Triggers 
       6. Listen and Notify 
       7. Batch Update, MoMA Collection 
       8. An Interview with Kris Jenkins
  8. Closing Thoughts
  
  
Your book companion to learning SQL is now available online at
<http://masteringpostgresql.com>, go buy it! I've spent the summer (and then
some) working on this title, and I hope you will enjoy reading it as much as
I enjoyed writing it!

<center>

{{< figure src="/img/MasteringPostgreSQLinAppDev-Cover-th.png"
          link="http://masteringpostgresql.com"
          title="Buy it online!" >}}

</center>

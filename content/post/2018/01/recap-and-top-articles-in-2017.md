+++
title = "A Year in Review: Most Read Articles in 2017"
date = "2018-01-28T22:46:43+01:00"
tags = ["PostgreSQL","YeSQL"]
categories = ["PostgreSQL","YeSQL"]
coverImage = "/img/2017Wave2018A.jpg"
coverMeta = "in"
coverSize = "partial"
thumbnailImage = "/img/2017-Logo.jpg"
thumbnailImagePosition = "left"
aliases = ["/blog/2018/01/working-at-citus/"]

+++

It seems to be usual nowadays to review the previous year, and readers
apparently like Top-N Lists — _that's you now, so let's hope that my
understanding works with you too_.

Of course 2018 will see its own amount of new and original content added to
this blog, with a continuous focus towards how to make the best out of the
SQL powerful programming language, and its advanced concurrency semantics.

<!--more-->

Also, now that I work at [Citus Data](https://www.citusdata.com) and
contribute to their Open Source products, the *scaling-out* angle might make
some appearances too.

So, here is the Top 8 Articles of 2017 on <http://tapoueh.org> in terms of
visits to this website's pages:

<!--toc-->

# Understanding Window Functions

> This seems more like the all-time favorite article on this website really.
> It's a practical introduction to the matter of window functions, using a
> real-world dataset that is available freely.
>
> To have a complete and thorough understanding of Window Functions,
> consider reading [Bruce Momjian's](https://momjian.us/) excellent slide
> deck [Postgres Window
> Magic](https://momjian.us/main/writings/pgsql/window.pdf)!
>
> Or even watch it in video here:
>
> {{< youtube D8Q4n6YXdpk >}}

<figure style="float: right; clear: left; display: block; width: 140px; height: 140px; margin-bottom: 2em;">
    <a href="/blog/2013/08/understanding-window-functions/">
        <img src="/img/old/moving_window.gif">
    </a>
</figure>

There was SQL
before
[window functions](http://www.postgresql.org/docs/current/static/tutorial-window.html) and
SQL after *window functions*: that's how powerful this tool is. Being that
of a deal breaker unfortunately means that it can be quite hard to grasp the
feature. This article aims at making it crystal clear so that you can begin
using it today and are able to reason about it and recognize cases where you
want to be using *window functions*.

# PostgreSQL, Aggregates and Histograms

> Only second to the Window Functions article from 2013 is the Histogram
> article from 2014, another big classic of this website. The article shows
> how to have some fun with PostgreSQL and draw plots directly in the
> console, no less!

<figure style="float: right; clear: left; display: block; width: 140px; height: 140px; margin-bottom: 2em;">
    <a href="/blog/2014/02/postgresql-aggregates-and-histograms/">
        <img src="/img/old/histogram.jpg">
    </a>
</figure>

In our previous article 
[Aggregating NBA data, PostgreSQL vs MongoDB](/blog/2014/02/17-aggregating-nba-data-PostgreSQL-vs-MongoDB) we spent
time comparing the pretty new 
*MongoDB Aggregation Framework* with the decades
old SQL aggregates. Today, let's showcase more of those SQL aggregates,
producing a nice 
*histogram* right from our SQL console.

> In the article you will learn how to produce this kind of chart right in
> your console while exploring a dataset:

~~~
 bucket |  range  | freq  |              bar               
--------+---------+-------+--------------------------------
      1 | [10,15) |    52 | 
      2 | [15,20) |  1363 | ■■
      3 | [20,25) |  8832 | ■■■■■■■■■■■■■
      4 | [25,30) | 20917 | ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■
      5 | [30,35) | 20681 | ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■
      6 | [35,40) |  9166 | ■■■■■■■■■■■■■
      7 | [40,45) |  2093 | ■■■
      8 | [45,50) |   247 | 
      9 | [50,54) |    20 | 
     10 | [54,55) |     1 | 
(10 rows)

Time: 53.570 ms
~~~

# SQL and Business Logic

> This one makes it in second position, which is quite impressive. Well my
> understanding is that this is a big and complex topic on which many people
> have many different opinions. Allow me to share mine!

<figure style="float: right; clear: left; display: block; width: 140px; height: 140px; margin-bottom: 2em;">
    <a href="/blog/2017/06/sql-and-business-logic/">
        <img src="/img/maze-inside-a-database.jpg">
    </a>
</figure>

Business logic is *supposed to be* the part of the application where you
deal with customer or user facing decisions and computations. It is often
argued that this part should be well separated from the rest of the
technical infrastructure of your code.

Of course, SQL and relational database design is meant to support your
business cases (or user stories), so then we can ask ourselves if SQL should
be part of your business logic implementation.

Or actually, how much of your business logic should be SQL?

# How to Write SQL

> Now that you want to use SQL more in your application, how actually would
> you do that? It's possible to find tooling that have been designed to
> answer exactly that question, and that allow a developer to deal with
> `.sql` files in their repositories. That's pretty neat!

<figure style="float: right; clear: left; display: block; width: 140px; height: 140px; margin-bottom: 2em;">
    <a href="/blog/2017/06/how-to-write-sql/">
        <img src="/img/sql-filetype.svg">
    </a>
</figure>

[Kris Jenkins](https://twitter.com/krisajenkins) cooked up a very nice way
to embed SQL in your
code: [YeSQL for Clojure](https://github.com/krisajenkins/yesql). The main
idea is that you should be writing your SQL queries in `.sql` files in your
code repository and maintain them there.

The idea is very good and it is now possible to find alternative
implementations of the [Clojure](https://clojure.org) *yesql* library in
other languages. Today, we are going to have a look at one of them for
the [python](https://www.python.org) programming
language: [anosql](https://github.com/honza/anosql).

# Mastering PostgreSQL in Application Development

> This summer, I wrote a book! In its pages, I share my knowledge of SQL
> with application developers, and we study ways to answer simple to
> advanced business questions — each time within a single SQL query.
>
> The book has been a continued success to unexpected degrees! Thanks to
> everyone buying and reading it, and also to a few of the readers sharing
> some feedback about the content in there. I'm still almost as excited as
> on the book's release date!

<figure style="float: right; clear: left; display: block; width: 140px; height: 140px; margin-bottom: 2em;">
    <a href="https://masteringpostgresql.com">
        <img src="/img/MasteringPostgreSQLinAppDev-Cover-th.png">
    </a>
</figure>

[Mastering PostgreSQL in Application
Development](https://masteringpostgresql.com) targets application developers
who want to learn SQL properly, and actually master this programming
language. Most developers don't think of SQL as a programming language,
mainly because they don't have full control of the execution plan of their
queries.

# Setting up psql, the PostgreSQL CLI

> New users of PostgreSQL are confronted with using `psql`, our beloved
> Command-Line Application. Some of them don't readily appreciate how
> powerful this environment can be when setup properly, with interactive
> query editing done using your prefered editor, for instance. 
>
> Not too surprisingly, this article have reached quite a good success on
> the blog. Also, the content comes straight from my book [Mastering
> PostgreSQL in Application Development](https://masteringpostgresql.com)!

<figure style="float: right; clear: left; display: block; width: 140px; height: 140px; margin-bottom: 2em;">
    <a href="/blog/2017/12/setting-up-psql-the-postgresql-cli/">
        <img src="/img/icon-cli.png">
    </a>
</figure>

PostgreSQL ships with an interactive console with the command line tool
named [psql](https://www.postgresql.org/docs/current/static/app-psql.html).
It can be used both for scripting and interactive usage and is moreover
quite a powerful tool. Interactive features includes *autocompletion*,
*readline* support (history searches, modern keyboard movements, etc), input
and output redirection, formatted output, and more.

# Exploring a Data Set in SQL

> If there's an article full of SQL query text and example on this
> <http://tapoueh.org> website, this is the one! Well apart from [on JSON
> and SQL](/blog/2017/09/on-json-and-sql/) maybe — which made it to number
> 12 on that Top list — no other article goes that deep into real world SQL
> sample queries.

<figure style="float: right; clear: left; display: block; width: 140px; height: 140px; margin-bottom: 2em;">
    <a href="/blog/2017/06/exploring-a-data-set-in-sql/">
        <img src="/img/explore-data.png">
    </a>
</figure>

Sometimes you need to dive in an existing data set that you know very little
about. Let's say we've been lucky to have had a high level description of
the business case covered by a database, and then access to it. Our next
step is figuring out data organisation, content and quality. Our tool box:
*the world's most advanced open source
database*, [PostgreSQL](https://www.postgresql.org), and its *Structured
Query Language*, SQL.

# PostgreSQL and the calendar

> Dealing with time is a political activity more so than an engineering
> occupation these days, with time zone handling and tracking being so very
> complex in many subtle ways. No wonder then that this entry made it to our
> Top list of 2017!

<figure style="float: right; clear: left; display: block; width: 140px; height: 140px; margin-bottom: 2em;">
    <a href="/blog/2017/06/postgresql-and-the-calendar/">
        <img src="/img/Calendar-Time.png">
    </a>
</figure>

The modern calendar is a trap for the young engineer's mind. We deal with
the calendar on a daily basis and until exposed to its insanity it's rather
common to think that calendar based computations are easy. That's until
you've tried to do it once. A very good read about how the current calendar
came to be the way it is now is Erik's Naggum [The Long, Painful History of
Time](http://naggum.no/lugm-time.html).



+++
date = "2013-08-20T12:04:00.000000+02:00"
title = "Understanding Window Functions"
tags = ["PostgreSQL", "Tricks", "Window-Functions", "YeSQL"]
categories = ["PostgreSQL","YeSQL"]
thumbnailImage = "/img/old/moving_window.gif"
thumbnailImagePosition = "left"
coverImage = "/img/motor-racing.jpg"
coverSize = "partial"
coverMeta = "in"
aliases = ["/blog/2013/08/20-Window-Functions",
           "/blog/2013/08/20-Window-Functions.html"]
+++

There was SQL
before
[window functions](http://www.postgresql.org/docs/current/static/tutorial-window.html) and
SQL after *window functions*: that's how powerful this tool is. Being that
of a deal breaker unfortunately means that it can be quite hard to grasp the
feature. This article aims at making it crystal clear so that you can begin
using it today and are able to reason about it and recognize cases where you
want to be using *window functions*.

<center>
{{< image classes="fig50 fancybox dim-margin" src="/img/old/moving_window.gif" >}}
</center>
<center>*We see a part of the data as if through a little window*</center>

<!--more-->

The whole idea behind *window functions* is to allow you to process several
values of the result set at a time: you see through the window some *peer*
rows and are able to compute a single output value from them, much like when
using an *aggregate* function.

<!--toc-->

# It's all about frames

[PostgreSQL](http://www.postgresql.org/) comes with plenty of features, one
of them will be of great help here to get a better grasp at what's happening
with *window functions*. The first step we are going through here is
understanding which ***data*** the function has access to. For each input
row you have access to a ***frame*** of the data, and the first thing to
understand here is that *frame*.

First, meet with `array_agg`, an *aggregate* function that will build an
array for you. Let's use this tool to understand *window frames*:

~~~ sql
 select x, array_agg(x) over (order by x)
   from generate_series(1, 3) as t(x);
~~~
~~~
 x | array_agg 
---+-----------
 1 | {1}
 2 | {1,2}
 3 | {1,2,3}
(3 rows)
~~~


The *array_agg* column in the previous query output allows us to see the
full exact content of the *windowing* we're going to process. The window
definition here is `over (order by x)` and actually means `over (order by x
rows between unbounded preceding and current row)`:

~~~ sql
 select x,
        array_agg(x) over (order by x
                           rows between unbounded preceding
                                    and current row)
   from generate_series(1, 3) as t(x);
~~~
~~~
 x | array_agg 
---+-----------
 1 | {1}
 2 | {1,2}
 3 | {1,2,3}
(3 rows)
~~~


It's possible to work with other kind of *frame specifications* too, as in
the following examples:

~~~ sql
select x,
       array_agg(x) over (rows between current row
                                   and unbounded following)
  from generate_series(1, 3) as t(x);
~~~
~~~
 x | array_agg 
---+-----------
 1 | {1,2,3}
 2 | {2,3}
 3 | {3}
(3 rows)
~~~


If no frame clause is used at all, then the default is to see the whole set
of rows in each of them, which can be really useful if you want to compute
sums and percentages for example:

~~~ sql
select x,
       array_agg(x) over () as frame,
       sum(x) over () as sum,
       x::float/sum(x) over () as part
  from generate_series(1, 3) as t(x);
~~~
~~~ 
 x |  frame  | sum |       part        
---+---------+-----+-------------------
 1 | {1,2,3} |   6 | 0.166666666666667
 2 | {1,2,3} |   6 | 0.333333333333333
 3 | {1,2,3} |   6 |               0.5
(3 rows)
~~~


Did you know you could compute both the total sum of a column and the
proportion of the current value against this total within a single SQL
query? That's the breakthrough we're talking about now with *window
functions*.

# Partitioning into different frames

Other frames are possible to define when using the clause `PARTITION BY`. To
understand the *partition by* window it is best to relate to a real world
use case. Let's use
the [historical record of motor racing data](http://ergast.com/mrd/)
available publicly.

{{< alert success >}}

The database is available in a single download file for MySQL only. Once you
have a local copy, use [pgloader](http://pgloader.io) to have the data set
in PostgreSQL, it's a single command line (once you have created a *f1db*
database):

~~~ bash
$ createdb f1db
$ pgloader mysql://root@localhost/f1db pgsql:///f1db
~~~

Also you might want to have *f1db* in your *search_path*, which you can do
with the following command:

~~~ sql
ALTER DATABASE f1db SET search_path TO f1db, public;
~~~
 
If you want to try the query at home and modify it, consider fetching and
loading the same data set.

{{< /alert >}}

So in the *Ergast* database we have a *results* table with results from all
the known races. We pick a race that happened just before this article was
first published:

~~~
-[ RECORD 1 ]-----------------------------------------------------
raceid    | 890
year      | 2013
round     | 10
circuitid | 11
name      | Hungarian Grand Prix
date      | 2013-07-28
time      | 12:00:00
url       | http://en.wikipedia.org/wiki/2013_Hungarian_Grand_Prix
~~~

And within that race we can now fetch the list of competing drivers in their
position order (winner first), and also their ranking compared to other
drivers from the same constructor in the race:

~~~ sql
select surname,
         constructors.name,
         position,
         format('%s / %s',
                row_number()
                  over(partition by constructorid
                           order by position nulls last),

                count(*) over(partition by constructorid)
               )
            as "pos same constr"
    from      results
         join drivers using(driverid)
         join constructors using(constructorid)
   where raceid = 890
order by position;
~~~

The *partition by* frame allow us to see *peer rows*, here the rows from
*results* where the *constructorid* is the same as the current row. We use
that partition twice in the previous SQL query, in the `format()` call. The
first time with the `row_number()` window function gives us the position in
the race with respect to other drivers from the same constructor, and the
second time with `count(*)` gives us how many drivers from the same
constructor participated in the race:

~~~
    surname    |    name     | position | pos same constr 
---------------+-------------+----------+-----------------
 Hamilton      | Mercedes    |        1 | 1 / 2
 Räikkönen     | Lotus F1    |        2 | 1 / 2
 Vettel        | Red Bull    |        3 | 1 / 2
 Webber        | Red Bull    |        4 | 2 / 2
 Alonso        | Ferrari     |        5 | 1 / 2
 Grosjean      | Lotus F1    |        6 | 2 / 2
 Button        | McLaren     |        7 | 1 / 2
 Massa         | Ferrari     |        8 | 2 / 2
 Pérez         | McLaren     |        9 | 2 / 2
 Maldonado     | Williams    |       10 | 1 / 2
 Hülkenberg    | Sauber      |       11 | 1 / 2
 Vergne        | Toro Rosso  |       12 | 1 / 2
 Ricciardo     | Toro Rosso  |       13 | 2 / 2
 van der Garde | Caterham    |       14 | 1 / 2
 Pic           | Caterham    |       15 | 2 / 2
 Bianchi       | Marussia    |       16 | 1 / 2
 Chilton       | Marussia    |       17 | 2 / 2
 di Resta      | Force India |       18 | 1 / 2
 Rosberg       | Mercedes    |       19 | 2 / 2
 Bottas        | Williams    |        ⦱ | 2 / 2
 Sutil         | Force India |        ⦱ | 2 / 2
 Gutiérrez     | Sauber      |        ⦱ | 2 / 2
(22 rows)
~~~

Drivers who didn't finish the race get a *null* position entry, that our
*psql* setup displays as the ⦱ character, for convenience.

In a single SQL query we can obtain information from each driver in the race
and add to that other information from the race as a whole. Remember that
the *window functions* only happens after the *where* clause, so you only
get to see rows from the available result set of the query.

# Available window functions

Any and all *aggregate* function you already know can be used against a
*window frame* rather than a *grouping clause*, so you can already go use
`sum`, `min`, `max`, `count`, `avg` and the other you're used to.

You might already know that it's possible with PostgreSQL to use
the
[CREATE AGGREGATE](http://www.postgresql.org/docs/current/static/sql-createaggregate.html) command
to register your own *custom aggregate*. Any such custom aggregate can then
be given a *window frame definition* to work against too.

{{< alert info no-icon >}}

As an exercize to the reader, implement a *weighted average* aggregate and
use it against a table where you have at least three columns: a date, a
weight and a measure, with several measures per day. Now compute your
*weighted average* by applying your own aggregate to your data set, either
in a *grouping clause* or a *window frame*.

{{< /alert >}}

PostgreSQL of course is included
with
[built-in aggregate functions](http://www.postgresql.org/docs/9.2/static/functions-aggregate.html) and
a number
of
[built-in window functions](http://www.postgresql.org/docs/9.2/static/functions-window.html).

~~~ sql
  select surname,
         position,
         row_number()
           over(order by fastestlapspeed::numeric)
           as "fastest",
         ntile(3) over w as "group",
         lag(surname, 1) over w as "previous",
         lead(surname, 1) over w as "next"
    from      results
         join drivers using(driverid)
   where raceid = 890
  window w as (order by position)
order by position;
~~~

In this example you can see that we are reusing the same *window definition*
several times, so we're giving it a name to simplify the SQL. In this query
we are fetching for each driver its position in the results, its position in
terms of *fastest lap speed*, a *group* number if we divide the drivers into
a set of 4 groups thanks to the *ntile* function, the name of the previous
driver who made it, and the name of the driver immediately next to the
current one, thanks to the *lag* an *lead* functions:

~~~
    surname    | position | fastest | group |   previous    |     next      
---------------+----------+---------+-------+---------------+---------------
 Hamilton      |        1 |      20 |     1 | ⦱             | Räikkönen
 Räikkönen     |        2 |      17 |     1 | Hamilton      | Vettel
 Vettel        |        3 |      21 |     1 | Räikkönen     | Webber
 Webber        |        4 |      22 |     1 | Vettel        | Alonso
 Alonso        |        5 |      15 |     1 | Webber        | Grosjean
 Grosjean      |        6 |      16 |     1 | Alonso        | Button
 Button        |        7 |      12 |     1 | Grosjean      | Massa
 Massa         |        8 |      18 |     1 | Button        | Pérez
 Pérez         |        9 |      13 |     2 | Massa         | Maldonado
 Maldonado     |       10 |      14 |     2 | Pérez         | Hülkenberg
 Hülkenberg    |       11 |       9 |     2 | Maldonado     | Vergne
 Vergne        |       12 |      11 |     2 | Hülkenberg    | Ricciardo
 Ricciardo     |       13 |       8 |     2 | Vergne        | van der Garde
 van der Garde |       14 |       6 |     2 | Ricciardo     | Pic
 Pic           |       15 |       5 |     2 | van der Garde | Bianchi
 Bianchi       |       16 |       3 |     3 | Pic           | Chilton
 Chilton       |       17 |       4 |     3 | Bianchi       | di Resta
 di Resta      |       18 |      10 |     3 | Chilton       | Rosberg
 Rosberg       |       19 |      19 |     3 | di Resta      | Bottas
 Sutil         |        ⦱ |       2 |     3 | Gutiérrez     | ⦱
 Gutiérrez     |        ⦱ |       1 |     3 | Bottas        | Sutil
 Bottas        |        ⦱ |       7 |     3 | Rosberg       | Gutiérrez
(22 rows)
~~~

And we can see that the *fastest lap speed* is not as important as one could
think, as both the two fastest drivers didn't even finish the race. In SQL
terms we also see that we can have two different orderings returned from the
same query, and again we can poke at other rows.

# Conclusion

{{< image classes="fig25 right dim-margin"
              src="/img/old/sql-logo.png"
           title="PostgreSQL is YeSQL" >}}

The real magic of what's called *window functions* is actually the
***frame*** of data they can see when using the `OVER ()` clause and its
`PARTITION BY` and `ORDER BY` and *frame* clauses.

You need to remember that the windowing clauses are always considered last
in the query, meaning after the `WHERE` clause. You can only see in any
*frame* rows that have been selected for output: e.g. it's not directly
possible to compute a percentage over values that you don't want to display.
You would need to use a subquery in that case.

For more concrete examples about the *window functions* usage, you can see
some other of my blog posts, such
as [Make the Most ouf of SQL](/blog/2013/07/02-dubpug)
and [Reset Counter](/blog/2012/10/05-reset-counter) that I just tagged
as [Window Functions Articles](/tags/window-functions).

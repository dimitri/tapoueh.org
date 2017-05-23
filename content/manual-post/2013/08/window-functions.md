+++
date = "2013-08-20T12:04:00+02:00"
draft = false
title = "Understanding Window Functions"
tags = ["PostgreSQL", "Window-Functions", "YeSQL"]
categories = ["PostgreSQL","YeSQL"]
thumbnailImage = "/img/segmentation.png"
thumbnailImagePosition = "left"
coverImage = "/img/segmentation.png"
coverSize = "partial"
coverMeta = "out"
+++

There was SQL
before
[window functions](http://www.postgresql.org/docs/current/static/tutorial-window.html) and
SQL after *window functions*: that's how powerful this tool is. Being that
of a deal breaker unfortunately means that it can be quite hard to grasp the
feature. This article aims at making it crystal clear so that you can begin
using it today and are able to reason about it and recognize cases where you
want to be using *window functions*.

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

```sql
# select x, array_agg(x) over (order by x)
    from generate_series(1, 3) as t(x);
 x | array_agg 
---+-----------
 1 | {1}
 2 | {1,2}
 3 | {1,2,3}
(3 rows)
```

The `array_agg` column in the previous query output allows us to see the
full exact content of the *windowing* we're going to process. The window
definition here is `over (order by x)` and actually means `over (order by x
rows between unbounded preceding and current row)`:

```sql
# select x,
         array_agg(x) over (order by x
                            rows between unbounded preceding
                                     and current row)
    from generate_series(1, 3) as t(x);
 x | array_agg 
---+-----------
 1 | {1}
 2 | {1,2}
 3 | {1,2,3}
(3 rows)
```

It's possible to work with other kind of *frame specifications* too, as in the
following examples:

```sql
# select x,
         array_agg(x) over (rows between current row
                                     and unbounded following)
    from generate_series(1, 3) as t(x);
 x | array_agg 
---+-----------
 1 | {1,2,3}
 2 | {2,3}
 3 | {3}
(3 rows)
```

If no frame clause is used at all, then the default is too see the whole set
of rows in each of them, which can be really useful if you want to compute
sums and percentages for example:

```sql
# select x,
         array_agg(x) over () as frame,
         sum(x) over () as sum,
         x::float/sum(x) over () as part
    from generate_series(1, 3) as t(x);
 x |  frame  | sum |       part        
---+---------+-----+-------------------
 1 | {1,2,3} |   6 | 0.166666666666667
 2 | {1,2,3} |   6 | 0.333333333333333
 3 | {1,2,3} |   6 |               0.5
(3 rows)
```

Did you know you could compute both the total sum of a column and the
proportion of the current value against this total within a single SQL
query? That's the breakthrough we're talking about now with *window
functions*.

# Partitioning into different frames

Other frames are possible to define when using the clause `PARTITION BY`. To
see that in action though we need some more data to work with. The following
query is setting up an example for us to work with and will produce three
values per day for three different days, thanks to an implicit `CROSS JOIN`
construct here:

```sql
# create table p as
     select date::date as date,
            1 + floor(x * random()) as x
       from generate_series(date 'yesterday', date 'tomorrow', '1 day') as a(date),
            generate_series(1, 3) as b(x);
SELECT 9

# table p;
    date    | x 
------------+---
 2013-08-19 | 1
 2013-08-19 | 2
 2013-08-19 | 3
 2013-08-20 | 1
 2013-08-20 | 1
 2013-08-20 | 3
 2013-08-21 | 1
 2013-08-21 | 1
 2013-08-21 | 3
(9 rows)
```

Now let's have a better look at the data we have here, counting how many
times each x has been returned by our `random()` calls, per date:

```sql
# select date, x,
         count(x) over (partition by date, x),
         array_agg(x) over(partition by date),
         array_agg(x) over(partition by date, x)
    from p;
    date    | x | count | array_agg | array_agg 
------------+---+-------+-----------+-----------
 2013-08-19 | 1 |     1 | {1,2,3}   | {1}
 2013-08-19 | 2 |     1 | {1,2,3}   | {2}
 2013-08-19 | 3 |     1 | {1,2,3}   | {3}
 2013-08-20 | 1 |     2 | {1,1,3}   | {1,1}
 2013-08-20 | 1 |     2 | {1,1,3}   | {1,1}
 2013-08-20 | 3 |     1 | {1,1,3}   | {3}
 2013-08-21 | 1 |     2 | {1,1,3}   | {1,1}
 2013-08-21 | 1 |     2 | {1,1,3}   | {1,1}
 2013-08-21 | 3 |     1 | {1,1,3}   | {3}
(9 rows)
```

# Available window functions

Any and all *aggregate* function you already know can be used against a
*window frame* rather than a *grouping clause*, so you can already go use
`sum`, `min`, `max`, `count`, `avg` and the other you're used to.

You might already know that it's possible with PostgreSQL to use the
[CREATE AGGREGATE](http://www.postgresql.org/docs/current/static/sql-createaggregate.html) command to register your own *custom aggregate*. Any such
custom aggregate can then be given a *window frame definition* to work against
too. As an exercize to my dear readers, I this time propose you implement a
*weighted average* aggregate and use it against a table where you have at
least three columns: a date, a weight and a measure, with several measures
per day. Now compute your *weighted average* by applying your own aggregate to
your data set, either in a *grouping clause* or a *window frame*.

PostgreSQL of course is included
with
[built-in aggregate functions](http://www.postgresql.org/docs/current/static/functions-aggregate.html) and
a number
of
[built-in window functions](http://www.postgresql.org/docs/current/static/functions-window.html).

```sql
# select x,
         row_number() over(),
         ntile(4) over w,
         lag(x, 1) over w,
         lead(x, 1) over w
    from generate_series(1, 15, 2) as t(x)
  window w as (order by x);
 x  | row_number | ntile | lag | lead 
----+------------+-------+-----+------
  1 |          1 |     1 |     |    3
  3 |          2 |     1 |   1 |    5
  5 |          3 |     2 |   3 |    7
  7 |          4 |     2 |   5 |    9
  9 |          5 |     3 |   7 |   11
 11 |          6 |     3 |   9 |   13
 13 |          7 |     4 |  11 |   15
 15 |          8 |     4 |  13 |     
(8 rows)
```

In this example you can see that we are reusing the same *window definition*
each time, so we're giving it a name to make it simpler.

# Conclusion

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

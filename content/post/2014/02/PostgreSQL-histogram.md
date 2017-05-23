+++
date = "2014-02-21T13:25:00.000000+01:00"
title = "PostgreSQL, Aggregates and Histograms"
tags = ["PostgreSQL", "MongoDB", "YeSQL"]
categories = ["PostgreSQL","YeSQL"]
thumbnailImage = "/img/old/histogram.jpg"
thumbnailImagePosition = "left"
coverImage = "/img/old/histogram.jpg"
coverSize = "partial"
coverMeta = "out"
aliases = ["/blog/2014/02/21-PostgreSQL-histogram",
           "/blog/2014/02/21-PostgreSQL-histogram.html"]
+++

In our previous article 
[Aggregating NBA data, PostgreSQL vs MongoDB](/blog/2014/02/17-aggregating-nba-data-PostgreSQL-vs-MongoDB) we spent
time comparing the pretty new 
*MongoDB Aggregation Framework* with the decades
old SQL aggregates. Today, let's showcase more of those SQL aggregates,
producing a nice 
*histogram* right from our SQL console.



## PostgreSQL and Mathematics

The other day while giving a 
[Practical SQL](http://2ndquadrant.com/en/training/course-catalog/practical-sql/) training my attention drifted to
the 
`width_bucket` function available as part of the
[Mathematical Functions and Operators](http://www.postgresql.org/docs/9.3/static/functions-math.html) PostgreSQL is offering to its fearless
SQL users.

Here's what the documentation says about it:

> The function width_bucket(op numeric, b1 numeric, b2 numeric, count int)
> returns (as an int) the bucket to which operand would be assigned in an
> equidepth histogram with count buckets, in the range b1 to b2.


Let's have a look at our dataset from the NBA games and statistics, and get
back to counting 
*rebounds* in the 
`drb` field. A preliminary query informs us
that we have stats ranging from 10 to 54 rebounds per team in a single game,
a good information we can use in the following query:

~~~
select width_bucket(drb, 10, 54, 9), count(*)
    from team_stats
group by 1
order by 1;
 width_bucket | count 
--------------+-------
            1 |    52
            2 |  1363
            3 |  8832
            4 | 20917
            5 | 20681
            6 |  9166
            7 |  2093
            8 |   247
            9 |    20
           10 |     1
(10 rows)
~~~



{{< image classes="fig50 center fancybox dim-margin" src="/img/old/calibrate.gif" >}}


*We've just calibrated our games in terms of rebounds here*

## Console Histograms

Now, what would it take to actually be able to display the full story right
into our 
`psql` console, for preview before actually integrating a new diagram
in our reporting solution? Turns out it's not very complex.

First, we want to avoid hard coding the range of 
*rebounds* we're processing,
so we are going to compute that in a first step. Then we want the 
*histogram*
data, which is a ordered list of ranges with a min and a max value and a
*frequency*, which is how many games were recorded with a number or 
*rebounds*
within any given 
*bucket* range. And last, we want to display something a
little more visual than just a list of numbers:

~~~
with drb_stats as (
    select min(drb) as min,
           max(drb) as max
      from team_stats
),
     histogram as (
   select width_bucket(drb, min, max, 9) as bucket,
          int4range(min(drb), max(drb), '[]') as range,
          count(*) as freq
     from team_stats, drb_stats
 group by bucket
 order by bucket
)
 select bucket, range, freq,
        repeat('*', (freq::float / max(freq) over() * 30)::int) as bar
   from histogram;

 bucket |  range  | freq  |              bar               
--------+---------+-------+--------------------------------
      1 | [10,15) |    52 | 
      2 | [15,20) |  1363 | **
      3 | [20,25) |  8832 | *************
      4 | [25,30) | 20917 | ******************************
      5 | [30,35) | 20681 | ******************************
      6 | [35,40) |  9166 | *************
      7 | [40,45) |  2093 | ***
      8 | [45,50) |   247 | 
      9 | [50,54) |    20 | 
     10 | [54,55) |     1 | 
(10 rows)

Time: 53.570 ms
~~~


The query is using the 
[Common Table Expressions](http://www.postgresql.org/docs/9.3/interactive/queries-with.html) 
`WITH` syntax so that it's
easier to read and understand, then the data-related magic happens in the
`histogram` CTE. We use the 
`width_bucket` function to get a 
*calibration* number
for each of our games, and we 
`GROUP BY bucket` to be able to 
***aggregate*** the
*min* and 
*max* values.

As we're using PostgreSQL though, just having two columns with the min and
max as separate values is not enough, what we actually need is a
[discrete range](http://www.postgresql.org/docs/9.3/static/rangetypes.html) of 
*rebounds* for each 
*bucket*, hence using the 
`int4range` range
constructor function.

Of course, within the same 
`GROUP BY` aggregation here it's still possible to
*count* the number of games having a 
*rebounds* stat within the bucket, defining
the histogram's frequency.

The only remaining step then consists into hacking our way into actually
displaying something 
*visual enough* for a quick less-than-1-minute effort of
data crunching, using the 
`repeat` function which is part of
[PostgreSQL String Functions and Operators](http://www.postgresql.org/docs/9.3/static/functions-string.html). Note that we're using the
[Window Function](blog/2013/08/20-Window-Functions) expression 
`max(freq) over()` to have access the highest
frequency value from each and every result row.

So... Did I mention lately?


{{< image classes="fig50 center fancybox dim-margin" src="/img/old/220px-Postgresql_elephant.svg.png" >}}


***PostgreSQL is YeSQL!***

By the way, the whole scripting and data and SQL is available at
[github/dimitri/nba](https://github.com/dimitri/nba), and there's an 
[Hacker News](https://news.ycombinator.com/item?id=7257555) entry to comment on the
article if you're interested.

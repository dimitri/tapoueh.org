+++
title = "Find the number of the longest continuously rising days for a stock"
date = "2018-02-06T23:24:17+01:00"
tags = ["PostgreSQL","YeSQL","Rising","Window Functions","Stock","Recursive"]
categories = ["PostgreSQL","YeSQL"]
coverImage = "/img/stocks-on-the-rise.jpg"
coverMeta = "in"
coverSize = "partial"
thumbnailImage = "/img/growing-stock-icon.jpg"
thumbnailImagePosition = "left"
aliases = ["/blog/2018/01/working-at-citus/"]

+++

Today I want to react to an article that claims that [Relational Algebra Is
the Root of SQL
Problems](https://www.datasciencecentral.com/profiles/blogs/relational-algebra-is-the-root-of-sql-problems)
in which the author hand-waves the following position:

> SQL becomes more a hindrance to data manipulation than an efficient tool.
> SQL’s greatest problem isn’t in the implementation level, but at its
> theory foundation. The problem can’t be solved by application
> optimization. Relational algebra isn’t sophisticated enough for handling
> the complicated data manipulation scenarios.

Then they go on to several _arguments from authority_ to “prove” their
point. My reading of the article is that SQL is very hard when you didn't
care to learn it, as most technologies are.

In this article, we're going to look at the _simple examples_ provided where
apparently SQL makes it so much harder to find a solution compared to
writing some Java or C++ code. Contrary to the original article, we go as
far as to actually writing both the SQL solution and a complete Python
solution, so that we can compare.

<!--more-->

Maybe SQL is even less forgiving than most other technologies in that it is
a *declarative* programming language. It's very hard in SQL to improvise a
query, you really need to understand both the data model under it and also
the business case. You need to be able to describe **what** you want to
achieve, without telling much to the system about **how** to do it. When
you're not used to it, this is hard.

The simple examples made in the article entitled [Relational Algebra Is the
Root of SQL
Problems](https://www.datasciencecentral.com/profiles/blogs/relational-algebra-is-the-root-of-sql-problems)
are the following:

  1. Find the number of the longest continuously rising days for a stock.

  2. Find the top 10 among one billion records.

Here follows implementations in SQL and Python of those two simple examples,
so that you can compare them.

<!--toc-->

## First Simple Example

Here's how the problem is described in the original article we are reacting
to here:

> It’s simple for Java or C++ programmers. They will create a counter whose
> initial value is 0, sort records by date and traverse them all, add 1 to
> the counter when the stock rises and reset it to 0 when it falls, and then
> find the largest number the counter ever has.
>
> The logic is natural, but it’s rather difficult to implement it in SQL.
>
> Relational algebra inherits the mathematical concept of unordered sets,
> which means the data sorting can only be performed at the output and the
> order of traversal can’t be specified, making it difficult to implement
> the logic in a natural way. Instead, programmers need to generate numbers
> for the dates, create a group mark, group a rising day with the previous
> day and put a falling day into another group, and then find the biggest
> COUNT() value among the groups. The logic is difficult to grasp. That is
> the issue of the translation of computing logic being more difficult than
> the solution itself.

Saying that the _logic is natural_ without writing a single line of code is
an argument from authority in my book. Of course writing this quick recipe
without thinking about any level of details is always easy and simple, and
might even feel natural in somes cases. As [Linus Torvalds
said](https://en.wikiquote.org/wiki/Linus_Torvalds#2000-04) once:

> Talk is cheap. Show me the code.
>
> ***Torvalds, Linus*** (2000-08-25), in a [message to linux-kernel mailing
> list](http://lkml.org/lkml/2000/8/25/132).

Saying then that _it's rather difficult to implement it in SQL_ without even
trying is another argument from authority. And then the description of how
to write the SQL query focuses on ***how*** to retrieve the data rather than
***what*** data we want to retrieve. That is going to make it really hard to
express in SQL.

Moreover, _the order of traversal can't be specified_ sounds dubious. The
SQL standard cover the ORDER BY clause in the main query, in subqueries, as
an aggregate control, and in a window function frame definition. That's many
places where to specify the order of traversal, and SQL knows how to ORDER
BY date, thanks.

So, first thing first, we need a data model and a data set.

## Having some data to play with

[Intercontinental Exchange](https://www.nyse.com/) provides a chart with
[Daily NYSE Group Volume in NYSE Listed,
2017](http://www.nyxdata.com/nysedata/asp/factbook/viewer_edition.asp?mode=table&key=3141&category=3).
We can fetch the *Excel* file which is actually a *CSV* file using *tab* as
a separator, remove the headings and load it into a PostgreSQL table.

{{< figure class="right"
             src="/img/MasteringPostgreSQLinAppDev-Cover-th.png"
            link="https://masteringpostgresql.com" >}}

I have used this data set previously both in my book [Mastering PostgreSQL
in Application Development](https://masteringpostgresql.com) and in the
article [How to use PostgreSQL to streamline Python
code](https://opensource.com/article/17/12/python-and-postgresql) published
at <http://opensource.com>. Here's what the `factbook` table looks like:

~~~ sql
table factbook limit 10;
~~~

The `TABLE` command is standard SQL and included in PostgreSQL of course,
and an easy way to quickly see what's a data model all about. In our case,
we have the *date* and *dollars* columns, and some more:

~~~
 year │    date    │   shares   │ trades  │   dollars   
══════╪════════════╪════════════╪═════════╪═════════════
 2010 │ 2010-01-04 │ 1425504460 │ 4628115 │ 38495460645
 2010 │ 2010-01-05 │ 1754011750 │ 5394016 │ 43932043406
 2010 │ 2010-01-06 │ 1655507953 │ 5494460 │ 43816749660
 2010 │ 2010-01-07 │ 1797810789 │ 5674297 │ 44104237184
 2010 │ 2010-01-08 │ 1545692647 │ 5008824 │ 40816677580
 2010 │ 2010-01-11 │ 1492666469 │ 4970320 │ 41341854839
 2010 │ 2010-01-12 │ 1685064003 │ 5600471 │ 45676531877
 2010 │ 2010-01-13 │ 1468586700 │ 4989082 │ 39877317621
 2010 │ 2010-01-14 │ 1357605480 │ 4552360 │ 36899505722
 2010 │ 2010-01-15 │ 1952486180 │ 5335459 │ 53482586973
(10 rows)
~~~

Given this data set, it's then possible to find the number of the longest
continuously rising days for a stock, by looking at the dollars column.

## SQL Foundamentals Needed

The first thing we need to know in SQL here is how to determine if the stock
is rising. This involves comparing the current row's *dollars* with the next
row's *dollars*. That is easily done with a [Window
Function](/tags/window-functions/) as in the following example:

~~~ sql
  select date,
         to_char(
           lag(dollars, 1) over(order by date),
           'L99G999G999G999'
         ) as "dollars previous day",

         to_char(dollars, 'L99G999G999G999') as dollars,

         case when   dollars
                   - lag(dollars, 1) over(order by date)
                   < 0
              then '-'
              else '+'
          end as diff
    from factbook
order by date
   limit 5;
~~~

The `lag(dollars, 1) over(order by date)` SQL window function expression
gives us the amount of dollars for the previous day. It's then easy to
compare that number with the current's one and output a `+` if the stock is
rising, or a `-` if it's lowering:

~~~
    date    │ dollars previous day │     dollars      │ diff 
════════════╪══════════════════════╪══════════════════╪══════
 2010-01-04 │ ¤                    │ $ 38,495,460,645 │ +
 2010-01-05 │ $ 38,495,460,645     │ $ 43,932,043,406 │ +
 2010-01-06 │ $ 43,932,043,406     │ $ 43,816,749,660 │ -
 2010-01-07 │ $ 43,816,749,660     │ $ 44,104,237,184 │ +
 2010-01-08 │ $ 44,104,237,184     │ $ 40,816,677,580 │ -
(5 rows)
~~~

Using that knowledge, we might be able to actually find our magic number,
right?

## Advanced SQL With Full Details

Once we have the `+` and `-` information computed from the data set, the
number of continuously raising days can be found by collecting all the `+`
that follow each-other in a row.

Another way to say that is to find the first following date for which the
computed *diff* column changes, either from `+` to `-` or the other way
around, from `-` to `+`. This kind of lookup to the next value is done with
a *self-join* in SQL, and can be implemented in the following way:

~~~ sql
with diffs as
  (
     --
     -- compute if the current stock is raising or lowering
     --
     select date, dollars,
            case when   dollars
                      - lag(dollars, 1) over(order by date)
                      < 0
                 then '-'
                 else '+'
             end as diff
          from factbook
      where date is not null
   order by date
  )
select diffs.diff,
       min(diffs.date) as start,
       diff_change.date as end,
       count(*) as days

  from diffs
       left join lateral
       -- 
       -- for each row of our "diffs" relation, compute the next
       -- day at which the stock change direction is changing, that is
       -- where a + becomes a - or the other way round
       -- 
       (
         select date
           from diffs d2
          where d2.date > diffs.date
            and d2.diff <> diffs.diff
       order by date
          limit 1
       ) as diff_change on true

--
-- we group by the date where the +/- change occurs
-- and count how many rows share that value
--
group by diffs.diff, diff_change.date
  having count(*) > 4
order by days desc, start;
~~~

In this query, we first compute the differences in between the current and
the next row, as in the previous section of this article. Equiped with that
in the common table expression *diffs*, we may now find the previous date
when there's a change of direction in the stock value.

That lookup is implemented in the LEFT JOIN LATERAL subquery thanks to the
use of the ORDER BY and LIMIT clauses. Once we have found the date at which
the current trend changes, then we can compute how many days have been
having the same trend value thanks to a GROUP BY clause.

In order to make it easier to follow all the steps of writing the SQL query,
this query focuses on more than just the single result that's asked for: we
go to some lengths in order to display the following *debug* output, where
not only we have the maximum number of rising days for our stock, but also
the first and last day when it happened. Oh and we have both the rising and
lowering values too…

~~~
 diff │   start    │    end     │ days 
══════╪════════════╪════════════╪══════
 +    │ 2017-07-17 │ 2017-07-26 │    7
 -    │ 2014-08-07 │ 2014-08-15 │    6
 -    │ 2014-08-18 │ 2014-08-26 │    6
 +    │ 2015-07-21 │ 2015-07-29 │    6
 -    │ 2010-05-07 │ 2010-05-14 │    5
 -    │ 2010-07-30 │ 2010-08-06 │    5
 -    │ 2010-12-20 │ 2010-12-28 │    5
 +    │ 2011-08-02 │ 2011-08-09 │    5
 -    │ 2011-08-09 │ 2011-08-16 │    5
 -    │ 2011-11-18 │ 2011-11-28 │    5
 +    │ 2012-05-14 │ 2012-05-21 │    5
 +    │ 2016-10-25 │ 2016-11-01 │    5
 -    │ 2017-03-01 │ 2017-03-08 │    5
(13 rows)
~~~

The HAVING clause implements a restriction of the output to limit how many
rows we display, we could have been using a LIMIT clause instead if we
wanted to show a known number of rows in advance rather than only the
interesting values from the data set.

## The SQL Solution

So, now that we have seen this *debug* oriented result set, we can write a
SQL query that answers the question we are tasked with in the first place:

> Find the number of the longest continuously rising days for a stock.

~~~ sql
with diffs as
  (
     --
     -- compute if the current stock is raising or lowering
     --
     select date, dollars,
            case when   dollars
                      - lag(dollars, 1) over(order by date)
                      < 0
                 then '-'
                 else '+'
             end as diff
       from factbook
      where date is not null
   order by date
  )
select count(*) as days

  from diffs
       left join lateral
       -- 
       -- for each row of our "diffs" relation, compute the next
       -- day at which the stock change direction is changing, that is
       -- where a + becomes a - or the other way round
       -- 
       (
         select date
           from diffs d2
          where d2.date > diffs.date
            and d2.diff <> diffs.diff
       order by date
          limit 1
       ) as diff_change on true

 where diffs.diff = '+'

--
-- we group by the date where the +/- change occurs
-- and count how many rows share that value
--
group by diff_change.date

--
-- and we only keep the longest continuously rising number of days
--
order by days desc
   limit 1;
~~~

The answer is the following, straight to the point, without any extra
information:

~~~
 days 
══════
    7
(1 row)
~~~

Now is that query simple and its logic natural? It's easy enough to already
have an opinion on that, but we might also want to refrain until we have a
competing implementation (e.g. in Python) to actually think about it.

## An Alternative SQL Implementation

The same problem can be solved with using a recursive approach that looks
like the classic [Loose Index
Scan](https://wiki.postgresql.org/wiki/Loose_indexscan) method. The idea is
to start with the last day from our table. That's easy enough.

Then we fetch the day before from there, and see if its sock has risen, in
which case we increment our computed *day* column to reflect the situation,
and repeat the operation as long as we have days:

~~~ sql
with recursive raising as
(
   (
       -- 
       -- start with the most recent date in our dataset
       -- and prepare our recursive computed data: series
       -- of increasing dollars values and number of days 
       -- in a row of seeing an increase
       --
   select date, dollars, array[dollars] as series, 0 as days
     from factbook
    where date is not null
 order by date desc
    limit 1
   )
   
   union all
    
  (
         --
         -- fetch the previous day of factbook data and compute
         -- the new series/days values depending on the value of
         -- the previous factbook day compared to the value of the
         -- current day in raising
         --
     select factbook.date,
            factbook.dollars,
            case when raising.dollars > factbook.dollars
                 then array[factbook.dollars] || raising.series
                 else array[factbook.dollars]
             end as series,
            case when raising.dollars > factbook.dollars
                 then days + 1
                 else 0
             end as days
       from factbook join raising on factbook.date < raising.date
   order by date desc
      limit 1
  )
)
--
-- display only the interesting part of the recursive
-- query results:
--
  select days, date, series
    from raising
   where days > 4
order by days desc;
~~~

And the query shows interesting information that's not asked for, such as
the series of *dollars* values that have been registered to the number of
days we're looking for:

~~~
 days │    date    │                                              series                                               
══════╪════════════╪═══════════════════════════════════════════════════════════════════════════════════════════════════
    7 │ 2017-07-14 │ {32588362765,33760987656,35165369201,35297166209,39376330787,42470589106,42701120315,54039073558}
    6 │ 2015-07-20 │ {35853995657,39619794106,42664392961,43134509170,43590746301,45959161744,47026897118}
    6 │ 2017-07-17 │ {33760987656,35165369201,35297166209,39376330787,42470589106,42701120315,54039073558}
    5 │ 2012-05-11 │ {35089242038,35385171602,38619918965,39640166828,42001050587,50014530108}
    5 │ 2017-07-18 │ {35165369201,35297166209,39376330787,42470589106,42701120315,54039073558}
    5 │ 2011-08-01 │ {51752009908,56183343867,62257561258,80176338229,94797260297,101033041245}
    5 │ 2016-10-24 │ {37480090453,39682504757,41375471787,44843918730,45627849370,49351786965}
    5 │ 2015-07-21 │ {39619794106,42664392961,43134509170,43590746301,45959161744,47026897118}
(8 rows)
~~~

It's easy from there to change the last part of the query so that it would
only returns the number 7 that we want to find here.

## A Python Implementation

A very classic implementation of the problem at hand in Python would look
like the following script:

~~~ Python
#! /usr/bin/env python3

import psycopg2
from collections import namedtuple

Record = namedtuple('Record', 'date days')
PGCONNSTRING = "dbname=appdev application_name=cont"

def cont():
    "Fetch data from the factbook table"

    conn = psycopg2.connect(PGCONNSTRING)
    curs = conn.cursor()
    sql = """
  SELECT date, dollars
    FROM factbook
   WHERE date is not null
ORDER BY date
"""
    curs.execute(sql)

    previous_date = None
    previous_dollars = None

    current = Record(date=None, days=0)
    best = Record(date=None, days=0)

    for date, dollars in curs.fetchall():
        if previous_dollars:
            if dollars > previous_dollars:
                current = Record(current.date, current.days + 1)
                if current.days > best.days:
                    best = Record(current.date, current.days)
            else:
                current = Record(date, 0)
        else:
            current = Record(date, 0)

        previous_date, previous_dollars = date, dollars

    return best

if __name__ == '__main__':
    rising = cont()

    print("Continuously rising days: %d days from %s" % (rising.days,
                                                         rising.date))
~~~

Running the code gives us the following result, as expected:

~~~
Continuously rising days: 7 days from 2017-07-14
~~~

The only bits that might be not the simplest possible implementation of the
problem set is that I chose to keep track of not only the number of days but
also the date when the rising series starts, and I've been using a
_namedtuple_ to keep track of that.

## Second Simple Example

The other simple example provided is to _find the top 10 among one billion
records_.

And the article's author goes on to mention that

> There’s no need to sort the one billion records to find the desired ones.
> We create a 10-member empty set, and traverse data while keeping the set
> contain the top 10 records of the traversed records. That way we just need
> to perform one traversal with a very small memory space occupied and a
> good computational performance.
>
> Unfortunately as the relational algebra doesn’t define an explicit set
> data type, SQL can’t express the algorithm.

## The SQL Solution

Well the way to express the algorithm in SQL is to describe the result we
are interested into, and for that here we can use the simple ORDER BY and
LIMIT clauses of the language:

~~~ sql
explain (analyze, verbose, buffers)

   select date, dollars
     from factbook
 order by dollars desc
    limit 10;
~~~

Now, we don't get to specify **how** the data is going to be retrieved by
our SQL engine. Of course, We can trust PostgreSQL to do the Right Thing™
here for us, as we can see in full details thanks to the previous EXPLAIN
(ANALYZE, VERBOSE, BUFFERS) command. It gives the following result:

~~~
 Limit  (cost=76.73..76.76 rows=10 width=12)
        (actual time=1.356..1.359 rows=10 loops=1)
   Output: date, dollars
   Buffers: shared hit=18
   ->  Sort  (cost=76.73..81.62 rows=1953 width=12)
             (actual time=1.354..1.354 rows=10 loops=1)
         Output: date, dollars
         Sort Key: factbook.dollars DESC
         Sort Method: top-N heapsort  Memory: 25kB
         Buffers: shared hit=18
         ->  Seq Scan on public.factbook
                   (cost=0.00..34.53 rows=1953 width=12)
                   (actual time=0.017..0.673 rows=1953 loops=1)
               Output: date, dollars
               Buffers: shared hit=15
 Planning time: 0.137 ms
 Execution time: 1.395 ms
(13 rows)
~~~

The mention of the *Top-N heapsort* that used exactly 25kB of memory is
exactly the suggested implementation. I don't suppose you could implement
that in 4 lines of your favorite programming language, but even if you
actually can, that's beyond the point. It is actually very simple to solve
that simple exercise in SQL, and that includes checking that the right
algorithm is in use.

The result of the query isn't very interesting, though we might want to
compare with the Python implementation of the Top-N algorithm:

~~~
    date    │   dollars    
════════════╪══════════════
 2014-12-19 │ 124663932012
 2015-09-18 │ 118869806099
 2014-09-19 │ 118622863491
 2013-12-20 │ 117924997250
 2015-03-20 │ 115466468635
 2016-06-24 │ 112434567771
 2015-06-26 │ 110931465892
 2010-06-25 │ 110901889417
 2015-12-18 │ 110329938339
 2014-03-21 │ 107923489435
(10 rows)
~~~

## A Python Implementation

In Python, implementing a Top-N heapsort can be done using the
[heapq](https://docs.python.org/3/library/heapq.html) standard library, as
in the following code:

~~~ Python
#! /usr/bin/env python3

import psycopg2
import heapq
import sys

PGCONNSTRING = "dbname=appdev application_name=cont"


def top(n):
    "Fetch data from the factbook table"

    conn = psycopg2.connect(PGCONNSTRING)
    curs = conn.cursor()
    sql = """
  SELECT date, dollars
    FROM factbook
   WHERE date is not null
"""
    curs.execute(sql)

    topn = [(0, None) for i in range(n)]
    heapq.heapify(topn)

    for date, dollars in curs.fetchall():
        heapq.heappushpop(topn, (dollars, date))

    return topn


if __name__ == '__main__':
    n = int(sys.argv[1])
    topn = top(n)

    for dollars, date in heapq.nlargest(n, topn):
        print("%s: %s" % (date, dollars))
~~~

The implementation is pretty straightforward, with the trick of using the
Python function `heappushpop` that requires an that we initialize a dummy
top-n result structure before looping over the result set.

~~~
2014-12-19: 124663932012
2015-09-18: 118869806099
2014-09-19: 118622863491
2013-12-20: 117924997250
2015-03-20: 115466468635
2016-06-24: 112434567771
2015-06-26: 110931465892
2010-06-25: 110901889417
2015-12-18: 110329938339
2014-03-21: 107923489435
~~~

The result is the same as before, of course.

## Is SQL That Bad Really?

To conclude this article, I think that
[Nicolas](https://twitter.com/ngollperrier) makes a very good point in his
tweet here:

<center>
<blockquote class="twitter-tweet" data-conversation="none" data-lang="en"><p lang="en" dir="ltr">1/ I&#39;m not complaining about SQL, I&#39;m citing a point of view. For most data use cases it probably still is the best tool for the job. However, I find the dogmatic approach to try to solve every problem with it unsettling.</p>&mdash; Nicolas Goll-Perrier (@ngollperrier) <a href="https://twitter.com/ngollperrier/status/959404749283094528?ref_src=twsrc%5Etfw">February 2, 2018</a></blockquote> <script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script> 
</center>

So rather than being dogmatic about it, my suggestion is to be pragmatic:

  1. Know your SQL and what can be done with this powerful programming
     language, and when being declarative is a strength.
     
  2. Remember that SQL and relational algebra are not exactly the same
     thing, as explained e.g. in [Relational Algebra and
     SQL](http://www.cs.cornell.edu/projects/btr/bioinformaticsschool/slides/gehrke.pdf)
     from professor [Johannes Gehrke](http://www.cs.cornell.edu/johannes/)
     from Cornell University.
     
  3. Pick the right tool for the right job, thinking about several
     dimensions in which you're making trade-offs, most importantly the
     modularity violations you might be making and how to properly document
     them and maintain the resulting code-base in the long term.

The trade-offs we make when choosing to solve problems in SQL or in
application code can be quite complex, and need proper thinking. My advice
is to first go with the more obvious solution in terms of architecture and
responsibility of the _modules_ or _systems_ you're using: make it obvious,
and only then, when you hit limitations that aren't compatible with your
constraints, then try and be clever about it, and maybe optimize by
violating your system's modularity.

It means that sometimes you will pick the SQL implementation even if it's
not so obvious and requires knowing about *lateral joins* and *window
functions*, and sometimes you'll be implementing it in the application
middleware or even in the front-end UI code because that's where it belongs,
and you'll bypass the database entirely, or use it a an dumb object store.

My opinion is that either choice is ok as long as you took a concious
decision. What I still often see in the field though, is developers who have
no idea about what can be done in SQL these days and are stuck in a kind of
[Greenspun's tenth
rule](https://en.wikipedia.org/wiki/Greenspun%27s_tenth_rule) only applied
to SQL rather than Common-Lisp:

> Any sufficiently complicated C or Fortran program contains an ad-hoc,
> informally-specified, bug-ridden, slow implementation of half of Common
> Lisp.

## Conclusion

PostgreSQL implements a very advanced version of SQL, so that's even more
the case when focusing on PostgreSQL, as I do.

{{< figure class="right" src="/img/MasteringPostgreSQLinAppDev-Cover-th.png"
             link="https://masteringpostgresql.com" >}}

The idea behing both this article and my book [Mastering PostgreSQL in
Application Development](https://masteringpostgresql.com) is that as a
developer using some SQL in your application code, you should master it and
know what's possible to implement with the PostgreSQL system, so that you
can make the right choices in terms of your application's architecture.



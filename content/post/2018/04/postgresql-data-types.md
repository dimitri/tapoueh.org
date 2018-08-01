+++
title = "PostgreSQL Data Types: an intro"
date = "2018-04-06T11:32:43+02:00"
tags = ["PostgreSQL","YeSQL","Data Types","Boolean"]
categories = ["PostgreSQL","YeSQL"]
coverImage = "/img/Spectrum-Grey-Scale.jpg"
coverMeta = "in"
coverSize = "partial"
thumbnailImage = "/img/boolean-logo.png"
thumbnailImagePosition = "left"

+++

Today, we're going to begin a dive into the PostgreSQL Data Types. As my
colleague [Will Leinweber](https://bitfission.com) said recently in his talk
[Constraints: a Developer's Secret
Weapon](https://www.postgresql.eu/events/pgdayparis2018/schedule/session/1835-constraints-a-developers-secret-weapon/)
that he gave at [pgDay Paris](https://2018.pgday.paris): [database
constraints in Postgres are the last line of
defense](https://www.citusdata.com/blog/2018/03/19/postgres-database-constraints/).

The most important of those constraints is the data type, or the _attribute
domain_ in normalization slang. By declaring an attribute to be of a certain
data type, then PostgreSQL ensures that this property is always true, and
then implements advanced processing features for each data type, so that you
may push the computation to the data, when needed.

This article is the first of a series that will go through many of the
PostgreSQL data types, and we open the journey with `boolean`.

<!--more-->
<!--toc-->

## PostgreSQL Data Types

PostgreSQL comes with a long list of data types. The following query limits
the types to the ones directly interesting to someone who is an application
developer, and still it lists 72 data types:

~~~ sql
select nspname, typname
    from      pg_type t
         join pg_namespace n
           on n.oid = t.typnamespace
   where nspname = 'pg_catalog'
     and typname !~ '(^_|^pg_|^reg|_handler$)'
order by nspname, typname;
~~~

Let's take only a sample of those with the help of the *TABLESAMPLE* feature
of PostgreSQL, documented in the [select SQL
from](https://www.postgresql.org/docs/current/static/sql-select.html#SQL-FROM)
page of the documentation:

~~~ sql
select nspname, typname
    from      pg_type t TABLESAMPLE bernoulli(20)
         join pg_namespace n
           on n.oid = t.typnamespace
   where nspname = 'pg_catalog'
     and typname !~ '(^_|^pg_|^reg|_handler$)'
order by nspname, typname;
~~~

In this run here's what I get as a random sample of about 20% of the
available PostgreSQL types. If you run the same query again, you will have a
different result set:

~~~ psql
  nspname   │    typname    
════════════╪═══════════════
 pg_catalog │ abstime
 pg_catalog │ anyelement
 pg_catalog │ bool
 pg_catalog │ cid
 pg_catalog │ circle
 pg_catalog │ date
 pg_catalog │ event_trigger
 pg_catalog │ line
 pg_catalog │ macaddr
 pg_catalog │ oidvector
 pg_catalog │ polygon
 pg_catalog │ record
 pg_catalog │ timestamptz
(13 rows)
~~~

So, let's open our journey with the `boolean` attribute domain.

## SQL Boolean: Three-Valued Logic

SQL introduces a NULL value in the boolean attribute domain, adding it to
the usual TRUE and FALSE values. That gives us ***three-valued logic***.
Where that's very different from other languages None or NULL is when
comparing values. Let's have a look at the SQL NULL truth table:

~~~ sql
select a::text, b::text,
       (a=b)::text as "a=b",
       format('%s = %s',
              coalesce(a::text, 'null'),
              coalesce(b::text, 'null')) as op,
       format('is %s',
              coalesce((a=b)::text, 'null')) as result
  from (values(true), (false), (null)) v1(a)
       cross join
       (values(true), (false), (null)) v2(b);
~~~

As you can see *cross join* is very useful for producing a truth table. It
implements a Cartesian product over our columns, here listing the first
value of *a* (*true*) with every value of *b* in order (*true*, then
*false*, then NULL), then again with the second value of *a* (*false*) and
then again with the third value of *a* (NULL).

We are using *format* and *coalesce* to produce an easier to read results
table here. The *coalesce* function returns the first of its argument which
is not null, with the restriction that all of its arguments must be of the
same data type, here *text*. Here's the nice truth table we get:

~~~ psql
   a   │   b   │  a=b  │      op       │  result  
═══════╪═══════╪═══════╪═══════════════╪══════════
 true  │ true  │ true  │ true = true   │ is true
 true  │ false │ false │ true = false  │ is false
 true  │ ¤     │ ¤     │ true = null   │ is null
 false │ true  │ false │ false = true  │ is false
 false │ false │ true  │ false = false │ is true
 false │ ¤     │ ¤     │ false = null  │ is null
 ¤     │ true  │ ¤     │ null = true   │ is null
 ¤     │ false │ ¤     │ null = false  │ is null
 ¤     │ ¤     │ ¤     │ null = null   │ is null
(9 rows)
~~~

We can think of NULL as meaning *I don't know what this is* rather than
*no value here*. Say you have in A (left hand) something (hidden) that you
don't know what it is and in B (right hand) something (hidden) that you
don't know what it is. You're asked if A and B are the same thing. Well, you
can't know that, can you?

So in SQL *null = null* returns NULL, which is the proper answer to the
question, but not always the one you expect, or the one that allows you to
write your query and have the expected result set.

That's why we have other SQL operators to work with data that might be
NULL: they are *IS DISTINCT FROM* and *IS NOT DISTINCT FROM*. Those two
operators not only have a very long name, they also pretend that NULL is
the same thing as NULL.

So if you want to pretend that SQL doesn't implement three-valued logic you
can use those operators and forget about Boolean comparisons returning
NULL.

We can even easily obtain the *truth table* from a SQL query directly:

~~~ sql
select a::text as left, b::text as right,
       (a = b)::text as "=",
       (a <> b)::text as "<>",
       (a is distinct from b)::text as "is distinct",
       (a is not distinct from b)::text as "is not distinct from"
  from            (values(true),(false),(null)) t1(a)
       cross join (values(true),(false),(null)) t2(b);
~~~

With this complete result this time:

~~~ psql
 left  │ right │   =   │  <>   │ is distinct │ is not distinct from 
═══════╪═══════╪═══════╪═══════╪═════════════╪══════════════════════
 true  │ true  │ true  │ false │ false       │ true
 true  │ false │ false │ true  │ true        │ false
 true  │ ¤     │ ¤     │ ¤     │ true        │ false
 false │ true  │ false │ true  │ true        │ false
 false │ false │ true  │ false │ false       │ true
 false │ ¤     │ ¤     │ ¤     │ true        │ false
 ¤     │ true  │ ¤     │ ¤     │ true        │ false
 ¤     │ false │ ¤     │ ¤     │ true        │ false
 ¤     │ ¤     │ ¤     │ ¤     │ false       │ true
(9 rows)
~~~

You can see that we have not a single NULL in the last two columns.

## Boolean Aggregates

You can have tuple attributes as Booleans too, and PostgreSQL includes
specific aggregates for them:

~~~ sql
  select year,
         format('%s %s', forename, surname) as name,
         count(*) as ran,
         count(*) filter(where position = 1) as won,
         count(*) filter(where position is not null) as finished,
         sum(points) as points
    from      races
         join results using(raceid)
         join drivers using(driverid)
group by year, drivers.driverid
  having bool_and(position = 1) is true
order by year, points desc;
~~~

In this query, we show the *bool_and()* aggregates that returns true when
all the Boolean input values are true. Like every *aggregate* it silently
bypasses NULL by default, so in our expression of *bool_and(position = 1)*
we will filter F1 drivers who won all the races they finished in a specific
season.

{{< alert info >}}

The database used in the next example is available in a single download file
for MySQL only. Once you have a local copy, use
[pgloader](https://pgloader.io) to have the data set in PostgreSQL, it's a
single command line (once you have created a *f1db* database):

~~~ bash
$ createdb f1db
$ pgloader mysql://root@localhost/f1db pgsql:///f1db
~~~

{{< /alert >}}

And here's the result of our query:

~~~ psql
 year │        name         │ ran │ won │ finished │ points 
══════╪═════════════════════╪═════╪═════╪══════════╪════════
 1950 │ Juan Fangio         │   7 │   3 │        3 │     27
 1950 │ Johnnie Parsons     │   1 │   1 │        1 │      9
 1951 │ Lee Wallard         │   1 │   1 │        1 │      9
 1952 │ Alberto Ascari      │   7 │   6 │        6 │   53.5
 1952 │ Troy Ruttman        │   1 │   1 │        1 │      8
 1953 │ Bill Vukovich       │   1 │   1 │        1 │      9
 1954 │ Bill Vukovich       │   1 │   1 │        1 │      8
 1955 │ Bob Sweikert        │   1 │   1 │        1 │      8
 1956 │ Pat Flaherty        │   1 │   1 │        1 │      8
 1956 │ Luigi Musso         │   4 │   1 │        1 │      5
 1957 │ Sam Hanks           │   1 │   1 │        1 │      8
 1958 │ Jimmy Bryan         │   1 │   1 │        1 │      8
 1959 │ Rodger Ward         │   2 │   1 │        1 │      8
 1960 │ Jim Rathmann        │   1 │   1 │        1 │      8
 1961 │ Giancarlo Baghetti  │   3 │   1 │        1 │      9
 1966 │ Ludovico Scarfiotti │   2 │   1 │        1 │      9
 1968 │ Jim Clark           │   1 │   1 │        1 │      9
(17 rows)
~~~

If we want to restrict the results to drivers who finished *and* won every
race they entered in a season we need to then write *having
bool_and(position is not distinct from 1) is true*, and then the result set
only contains those drivers who participated in a single race in the season.

The main thing about Booleans is the set of operators to use with them:

  - The `=` operator doesn't work as you think it would.
  
  - Use `is` to test against literal TRUE, FALSE or NULL rather than `=`.
    
  - Remember to use the IS DISTINCT FROM and IS NOT DISTINCT FROM operators
    when you need them.
    
  - Booleans can be aggregated thanks to *bool_and()* and *bool_or()*.
  
The main thing about Booleans in SQL is that they have three possible
values: TRUE, FALSE and NULL. Moreover the behavior with NULL is entirely
ad-hoc, so either you remember it or you remember to check your assumptions.
For more about this topic, you can read [What is the deal with
NULLs?](http://thoughts.davisjeff.com/2009/08/02/what-is-the-deal-with-nulls/)
from PostgreSQL Contributor [Jeff Davis](http://thoughts.davisjeff.com/).

## Conclusion

{{< figure class="right"
             src="/img/MasteringPostgreSQLinAppDev-Cover-th.png"
            link="https://masteringpostgresql.com" >}}
            
This article is an extract from my book [Mastering PostgreSQL in Application
Development](https://masteringpostgresql.com), which teaches SQL to
developers so that they may replace thousands of lines of code with very
simple queries. The book has a full chapter about data types in PostgreSQL,
check it out!

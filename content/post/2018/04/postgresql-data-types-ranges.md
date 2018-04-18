+++
title = "PostgreSQL Data Types: Ranges"
date = "2018-04-18T13:41:12+02:00"
tags = ["PostgreSQL","YeSQL","Data Types","Ranges","Date Ranges"]
categories = ["PostgreSQL","YeSQL"]
coverImage = "/img/pulse-of-the-city-1-thumb-620x620-27919.jpg"
coverMeta = "in"
coverSize = "partial"
thumbnailImage = "/img/reloading-timer-clock-icon-by-vexels.png"
thumbnailImagePosition = "left"

+++

Continuing our series of [PostgreSQL Data Types](/tags/data-types/) today
we're going to introduce the PostgreSQL ranges data type.

Range types are a unique feature of PostgreSQL, managing two dimensions of
data in a single column, and allowing advanced processing. The main example
is the *daterange* data type, which stores as a single value a lower and an
upper bound of the range as a single value. This allows PostgreSQL to
implement a concurrent safe check against *overlapping* ranges, as we're
going to see in this article.

<!--more-->
<!--toc-->


# Ranges

As usual, read the PostgreSQL documentation chapters with the titles [Range
Types](https://www.postgresql.org/docs/current/static/rangetypes.html) and
[Range Functions and
Operators](https://www.postgresql.org/docs/current/static/functions-range.html)
for complete information.

The [International Monetary Fund](http://www.imf.org/external/index.htm)
publishes [exchange rate archives by
month](https://www.imf.org/external/np/fin/data/param_rms_mth.aspx) for lots
of currencies. An exchange rate is relevant from its publication until the
next rate is published, which makes a very good use case for our PostgreSQL
range types.

The following SQL script is the main part of the *ELT* script that has been
used for my book [Mastering PostgreSQL in Application
Development](https://masteringpostgresql.com). Only missing from this book's
pages is the transformation script that pivots the available *tsv* file into
the more interesting format we use here:

~~~ sql
begin;

create schema if not exists raw;

-- Must be run as a Super User in your database instance
-- create extension if not exists btree_gist;

drop table if exists raw.rates, rates;

create table raw.rates
 (
  currency text,
  date     date,
  rate     numeric
 );

\copy raw.rates from 'rates.csv' with csv delimiter ';'

create table rates
 (
  currency text,
  validity daterange,
  rate     numeric,

  exclude using gist (currency with =,
                      validity with &&)
 );

insert into rates(currency, validity, rate)
     select currency,
            daterange(date,
                      lead(date) over(partition by currency
                                          order by date),
                      '[)'
                     )
            as validity,
            rate
       from raw.rates
   order by date;

commit;
~~~

In this SQL script, we first create a target table for loading the CSV file.
The file contains lines with a currency name, a date of publication, and a
rate as a *numeric* value. Once the data is loaded into this table, we can
transform it into something more interesting to work with from an
application, the *rates* table.

# Ranges Exclusion Constraints

The *rates* table registers the rate value for a currency and a *validity*
period, and uses an [exclusion
constraint](https://www.postgresql.org/docs/current/static/sql-createtable.html#SQL-CREATETABLE-EXCLUDE)
that guarantees non-overlapping *validity* periods for any given *currency*:

~~~ sql
exclude using gist (currency with =, validity with &&)
~~~

This expression reads: exclude any tuple where the currency is *=* to an
existing currency in our table *AND* where the *validity* is overlapping
with (*&&*) any existing validity in our table. This exclusion constraint is
implemented in PostgreSQL using a *GiST* index.

By default, *GiST* in PostgreSQL doesn't support one-dimensional data types
that are meant to be covered by *B-tree* indexes. With exclusion constraints
though, it's very interesting to extend *GiST* support for one-dimensional
data types, and so we install the *btree_gist* extension, provided in
PostgreSQL contrib package.

The script then fills in the *rates* table from the *raw.rates* we'd been
importing in the previous step. The query uses the *lead()* window function
to implement the specification spelled out in English earlier: _an exchange
rate is relevant from its publication until the next rate is published_.

# Querying Ranges

Here's how the data looks, with the following query targeting Euro rates:

~~~ sql
  select currency, validity, rate
    from rates
   where currency = 'Euro'
order by validity
   limit 10;
~~~

We can see that the validity is a range of dates, and the standard output
for this type is a closed range which includes the first entry and excludes
the second one:

~~~ psql
 currency │        validity         │   rate   
══════════╪═════════════════════════╪══════════
 Euro     │ [2017-05-02,2017-05-03) │ 1.254600
 Euro     │ [2017-05-03,2017-05-04) │ 1.254030
 Euro     │ [2017-05-04,2017-05-05) │ 1.252780
 Euro     │ [2017-05-05,2017-05-08) │ 1.250510
 Euro     │ [2017-05-08,2017-05-09) │ 1.252880
 Euro     │ [2017-05-09,2017-05-10) │ 1.255280
 Euro     │ [2017-05-10,2017-05-11) │ 1.255300
 Euro     │ [2017-05-11,2017-05-12) │ 1.257320
 Euro     │ [2017-05-12,2017-05-15) │ 1.255530
 Euro     │ [2017-05-15,2017-05-16) │ 1.248960
(10 rows)
~~~

Having this data set with the exclusion constraint means that we know we
have at most a single rate available at any point in time, which allows an
application needing the rate for a specific time to write the following
query:

~~~ sql
select rate
  from rates
 where currency = 'Euro' 
   and validity @> date '2017-05-18';
~~~

The operator *@>* reads *contains*, and PostgreSQL uses the exclusion
constraint's index to solve that query efficiently:

~~~ psql
   rate   
══════════
 1.240740
(1 row)
~~~

# Conclusion

PostgreSQL has support for more than date ranges, ranges of numbers are also
suppored. While it's possible to manually manage the lower and upper bounds
of the ranges in one's own application code, it's almost impossible to deal
with the concurrency issues right when dealing with non-overlapping ranges.

Think about it that way: a user inserts a new value in you table, and its
lower and upper bounds are not overlapping any other value in your table, so
it's fine. But in between when you check for that, from the application
code, and when you INSERT your new entry, a concurrent user does the same
thing, with a range that overlapps the first one being inserted. As the
first user's new range is not inserted yet, the check passes. And now you
insert two overlapping ranges. Ooops.

Rather than having to serialize all write access to the data set, PostgreSQL
indexes are allowed special visibility rules in order to guarantee unicity
at all times, and EXCLUDE USING is an extension of the unicity constraint:
it benefits from this capacity and guarantee your data set even when dealing
with concurrency updates to it!

{{< figure class="right"
             src="/img/MasteringPostgreSQLinAppDev-Cover-th.png"
            link="https://masteringpostgresql.com" >}}
            
This article is an extract from my book [Mastering PostgreSQL in Application
Development](https://masteringpostgresql.com), which teaches SQL to
developers so that they may replace thousands of lines of code with very
simple queries. The book has a full chapter about data types in PostgreSQL,
check it out!

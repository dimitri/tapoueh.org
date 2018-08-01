+++
title = "PostgreSQL Data Types"
date = "2018-05-24T14:47:05+02:00"
tags = ["PostgreSQL","YeSQL","Data Types"]
categories = ["PostgreSQL","YeSQL"]
coverImage = "/img/rubix_cubes.jpg"
coverMeta = "in"
coverSize = "partial"
thumbnailImage = "/img/boolean-logo.png"
thumbnailImagePosition = "left"

+++

Today it's time to conclude our series of [PostgreSQL Data
Types](/tags/data-types/) articles with a recap. The series cover lots of
core PostgreSQL data types and shows how to benefit from the PostgreSQL
concept of a data type: more than input validation, a PostgreSQL data type
also implements expected behaviors and processing functions.

This allows an application developer to rely on PostgreSQL for more complex
queries, having the processing happen where the data is, for instance when
implementing advanced JOIN operations, then retrieving only the data set
that is interesting for the application.


<!--more-->
<!--toc-->

## Boolean

<figure style="float: right; clear: left; display: block; width: 140px; height: 140px; margin-bottom: 2em;">
    <a href="/blog/2018/04/postgresql-data-types-an-intro/">
        <img src="/img/boolean-logo.png">
    </a>
</figure>

In [PostgreSQL Data Types: an
intro](/blog/2018/04/postgresql-data-types-an-intro/) we learnt how
PostgreSQL deals with booleans, including an example use of the `bool_and()`
aggregate.

The following query can be used to display a boolean Truth Table:

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

And we get the following result:

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


## Text Encoding

<figure style="float: right; clear: left; display: block; width: 140px; height: 140px; margin-bottom: 2em;">
    <a href="/blog/2018/04/postgresql-data-types-text-encoding/">
        <img src="/img/220px-Wikipedia_Logo_1.0.png">
    </a>
</figure>

In [PostgreSQL Data Types: Text
Encoding](/blog/2018/04/postgresql-data-types-text-encoding/) we refreshed
our understanding of text encoding and how to use them properly with
PostgreSQL.

In particular we had a look at a table containing text in many different
unicode scripts and explained the following situation:

~~~ psql
yesql# set client_encoding to latin1;
SET
yesql# select * from hello where language ~ 'Georgian';
ERROR:  character with byte sequence 0xe1 0x83 0xa5 in encoding "UTF8" ⏎
has no equivalent in encoding "LATIN1"
yesql# reset client_encoding ;
RESET
~~~

## Text Processing

<figure style="float: right; clear: left; display: block; width: 140px; height: 140px; margin-bottom: 2em;">
    <a href="/blog/2018/04/postgresql-data-types-text-processing/">
        <img src="/img/text-processing-logo.png">
    </a>
</figure>

In [PostgreSQL Data Types: Text
Processing](/blog/2018/04/postgresql-data-types-text-processing/) we put in
practice some of the [string functions and
operators](https://www.postgresql.org/docs/current/static/functions-string.html)
documentation chapter — with functions such as *overlay()*, *substring()*,
*position()* or *trim()*. And also aggregates such as *string_agg()*. And
then also *regular expression* functions, including the very powerful
*regexp_split_to_table()*.

In this article we saw how to normalize some content thanks to those text
processing functions, as in the following query where we want to have the
categories in their own separate columns, say *category* and *subcategory*:

~~~ sql
with categories(id, categories) as
 (
   select id,
          regexp_split_to_array(
            regexp_split_to_table(themes, ','),
            ' > ')
          as categories
     from opendata.archives_planete
 )
 select id,
        categories[1] as category,
        categories[2] as subcategory
   from categories
  where id = 'IF39599';
~~~

And now we make sense of the open data:

~~~ psql
   id    │         category          │       subcategory        
═════════╪═══════════════════════════╪══════════════════════════
 IF39599 │ Habillement               │ Habillement traditionnel
 IF39599 │ Etres humains             │ Homme
 IF39599 │ Etres humains             │ Portrait
 IF39599 │ Relations internationales │ Présence étrangère
(4 rows)
~~~

## Date and Time Processing

<figure style="float: right; clear: left; display: block; width: 140px; height: 140px; margin-bottom: 2em;">
    <a href="/blog/2018/04/postgresql-data-types-date-and-time-processing/">
        <img src="/img/cog-256.png">
    </a>
</figure>

In [PostgreSQL Data Types: Date and Time
Processing](/blog/2018/04/postgresql-data-types-date-and-time-processing/)
we saw that PostgreSQL allows implementing all the processing you need to
apply to your *timestamp with time zone values*.

We even built a reporting on the repartition of commits by weekday from the
beginning of the PostgreSQL project, in order to guess if contributors are
working on the project on the job only, or mostly during their free time
(weekend).

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

The query results show that our PostgreSQL committers tend to work whenever
they feel like it, but less so on the weekend. The project's lucky enough to
have a solid team of committers being paid to work on PostgreSQL:

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

## Network Addresses

<figure style="float: right; clear: left; display: block; width: 140px; height: 140px; margin-bottom: 2em;">
    <a href="/blog/2018/04/postgresql-data-types-network-addresses/">
        <img src="/img/Network-Ip-Address-icon.png">
    </a>
</figure>

In [PostgreSQL Data Types: Network
Addresses](/blog/2018/04/postgresql-data-types-network-addresses/) we had a
look at PostgreSQL support for both *cidr*, *inet*, and *macaddr* data types.

Equipped with this *set_masklen()* function, we could easily analyze our
access logs using arbitrary CIDR network definitions.

~~~ sql
  select set_masklen(ip::cidr, 24) as network,
         count(*) as requests,
         array_length(array_agg(distinct ip), 1) as ipcount
    from access_log
group by network
  having array_length(array_agg(distinct ip), 1) > 1
order by requests desc, ipcount desc;
~~~

And we got the following result:

~~~ psql
     network      │ requests │ ipcount 
══════════════════╪══════════╪═════════
 4.152.207.0/24   │      140 │       2
 222.95.35.0/24   │       59 │       2
 211.59.0.0/24    │       32 │       2
 61.10.7.0/24     │       25 │      25
 222.166.160.0/24 │       25 │      24
 219.153.10.0/24  │        7 │       3
 218.78.209.0/24  │        6 │       4
 193.109.122.0/24 │        5 │       5
 204.102.106.0/24 │        3 │       3
 66.134.74.0/24   │        2 │       2
 219.133.137.0/24 │        2 │       2
 61.180.25.0/24   │        2 │       2
(12 rows)
~~~

## Ranges

<figure style="float: right; clear: left; display: block; width: 140px; height: 140px; margin-bottom: 2em;">
    <a href="/blog/2018/04/postgresql-data-types-ranges/">
        <img src="/img/reloading-timer-clock-icon-by-vexels.png">
    </a>
</figure>

In [PostgreSQL Data Types:
Ranges](/blog/2018/04/postgresql-data-types-ranges/) we learnt that range
types are a unique feature of PostgreSQL, managing two dimensions of data in
a single column, and allowing advanced processing. The main example is the
daterange data type, which stores as a single value a lower and an upper
bound of the range as a single value. This allows PostgreSQL to implement a
concurrent safe check against overlapping ranges.

Then having a data set with the exclusion constraint, we could find the
valid rate for a specific time with the following query:

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

## Arrays

<figure style="float: right; clear: left; display: block; width: 140px; height: 140px; margin-bottom: 2em;">
    <a href="/blog/2018/04/postgresql-data-types-arrays/">
        <img src="/img/matrix.png">
    </a>
</figure>

In [PostgreSQL Data Types:
Arrays](/blog/2018/04/postgresql-data-types-arrays/) we saw that arrays can
be used to denormalize data and avoid lookup tables. A good rule of thumb
for using arrays that way is that you mostly use the array as a whole, even
if you might at times search for elements in the array. Heavier processing
is going to be more complex than a lookup table.

In this article we added hashtags arrays to tweets, allowing us to then
discover popular tags easily with the following query:

~~~ sql
  select tag, count(*)
    from hashtag, unnest(hashtags) as t(tag)
group by tag
order by count desc
   limit 10;
~~~

We got the following result of popular hashtags in our extract of 200,000
tweets that we used:

~~~ psql
     tag      │ count 
══════════════╪═══════
 #Hiring      │ 37964
 #Jobs        │ 24776
 #CareerArc   │ 21845
 #Job         │ 21368
 #job         │ 17763
 #Retail      │  7867
 #Hospitality │  7664
 #job?        │  7569
 #hiring!     │  6860
 #Job:        │  5953
(10 rows)
~~~

## XML

<figure style="float: right; clear: left; display: block; width: 140px; height: 140px; margin-bottom: 2em;">
    <a href="/blog/2018/04/postgresql-data-types-xml/">
        <img src="/img/XML-icon.png">
    </a>
</figure>

In [PostgreSQL Data Types: XML](/blog/2018/04/postgresql-data-types-xml/) we
introduced the SQL standard type XML, and also the PL/XSTL processing
facility for it.

~~~ sql
create extension plxslt;

CREATE OR REPLACE FUNCTION striptags(xml) RETURNS text
	LANGUAGE xslt
AS $$<?xml version="1.0"?>
<xsl:stylesheet version="1.0"
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns="http://www.w3.org/1999/xhtml"
>

  <xsl:output method="text" omit-xml-declaration="yes"/>

  <xsl:template match="/">
    <xsl:apply-templates/>
  </xsl:template>

</xsl:stylesheet>
$$;
~~~

## JSON

<figure style="float: right; clear: left; display: block; width: 140px; height: 140px; margin-bottom: 2em;">
    <a href="/blog/2018/04/postgresql-data-types-json/">
        <img src="/img/json-logo.png">
    </a>
</figure>

In [PostgreSQL Data Types: JSON](/blog/2018/04/postgresql-data-types-json/)
we had a look at the built-in support for JSON and its great range of
processing functions and operators, and complete indexing support.

We introduced JSON specific queries for PostgreSQL, such as the following:

~~~ sql
select * from js where extra @> '[2,4]';
~~~

In our test case, that found a single row:

~~~ psql
 id │    extra     
════╪══════════════
  1 │ [1, 2, 3, 4]
(1 row)
~~~

<figure style="float: right; clear: left; display: block; width: 140px; height: 140px; margin-bottom: 2em;">
    <a href="/blog/2017/09/on-json-and-sql/">
        <img src="/img/JSON.png">
    </a>
</figure>

To know a lot more about processing JSON and normalizing a database model
based on a set of JSON documents, you can read my article [on Json and
SQL](https://tapoueh.org/blog/2017/09/on-json-and-sql/) which contains a
full detailed example using data from [Magic: the Gathering card data in
JSON format](https://mtgjson.com).


## ENUM

<figure style="float: right; clear: left; display: block; width: 140px; height: 140px; margin-bottom: 2em;">
    <a href="/blog/2018/04/postgresql-data-types-enum/">
        <img src="/img/shop_set-31-512.png">
    </a>
</figure>

In [PostgreSQL Data Types: ENUM](/blog/2018/05/postgresql-data-types-enum/)
we introduced a data type has been added to PostgreSQL in order to make it
easier to support migrations from MySQL. Proper relational design would use
a reference table and a foreign key instead.

This article also showed how to do a reference table lookup at INSERT INTO
time, with a SELECT and that implements a JOIN for us:

~~~ sql
create table color(id serial primary key, name text);

create table cars
 (
   brand   text,
   model   text,
   color   integer references color(id)
 );

insert into color(name)
     values ('blue'), ('red'),
            ('gray'), ('black');

insert into cars(brand, model, color)
     select brand, model, color.id
      from (
            values('ferari', 'testarosa', 'red'),
                  ('aston martin', 'db2', 'blue'),
                  ('bentley', 'mulsanne', 'gray'),
                  ('ford', 'T', 'black')
           )
             as data(brand, model, color)
           join color on color.name = data.color;
~~~

## Geometry and Points

<figure style="float: right; clear: left; display: block; width: 140px; height: 140px; margin-bottom: 2em;">
    <a href="/blog/2018/04/postgresql-data-types-point/">
        <img src="/img/earth-tz-logo.jpg">
    </a>
</figure>

In [PostgreSQL Data Types:
Point](/blog/2018/05/postgresql-data-types-point/) we did download a
complete geolocation data set and normalized it, thus making good use of both
the normalization good practice and those other PostgreSQL data types we’ve
been learning about in the previous articles of this series.

Once the data set is fully normalized, we can do some reports query to
understand how it's distributed:

~~~ sql
  select continent.name,
         count(*),
         round(100.0 * count(*) / sum(count(*)) over(), 2) as pct,
         repeat('■', (100 * count(*) / sum(count(*)) over())::int) as hist
    from geoname.geoname
         join geoname.country using(isocode)
         join geoname.continent
           on continent.code = country.continent
group by continent.name order by continent.name;
~~~

We can see that the *GeoNames* data is highly skewed towards Asia, North
America, and then Europe. Of course, the Antartica data is not very dense.

~~~ psql
     name      │  count  │  pct  │               hist                
═══════════════╪═════════╪═══════╪═══════════════════════════════════
 Africa        │ 1170043 │ 10.14 │ ■■■■■■■■■■
 Antarctica    │   21125 │  0.18 │ 
 Asia          │ 3772195 │ 32.70 │ ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■
 Europe        │ 2488807 │ 21.58 │ ■■■■■■■■■■■■■■■■■■■■■■
 North America │ 3210802 │ 27.84 │ ■■■■■■■■■■■■■■■■■■■■■■■■■■■■
 Oceania       │  354325 │  3.07 │ ■■■
 South America │  517347 │  4.49 │ ■■■■
(7 rows)
~~~


## Object-Relational Database Management System

PostgreSQL is an [Object-Relational Database Management
System](/blog/2018/03/object-relational-database-management-system/): it
provides a complete implementation of function overloading and operator
overloading and uses it a basis for advanced indexing support. Each
PostgreSQL data type comes with its own set of operators and functions,
implementing advanced behavior tailored to the data type.

For instance, adding an interval of one month to a given data isn't going to
add the same number of days depending on if your *anchor* date is in
February or August, and PostgreSQL knows that! It also knows how to count
with IP addresses, and more:

~~~ sql
select x, 
       1 + x as "1+",
       '127.0.2.252'::inet + x as "ip address",
       set_masklen(('127.0.2.252'::inet + x)::cidr, 28) as "CIDR/28",
       date '2016-02-26' + x as date,
       cast(date_trunc('month', date '2016-02-26' +x)
          + interval '1 month - 1 day' as date) as "last day"
  from generate_series(0, 6) as t(x);
~~~

We can see that PostgreSQL `+` operator depend on the data types it's being
used with:

~~~
 x │ 1+ │ ip address  │    CIDR/28     │    date    │  last day  
═══╪════╪═════════════╪════════════════╪════════════╪════════════
 0 │  1 │ 127.0.2.252 │ 127.0.2.240/28 │ 2016-02-26 │ 2016-02-29
 1 │  2 │ 127.0.2.253 │ 127.0.2.240/28 │ 2016-02-27 │ 2016-02-29
 2 │  3 │ 127.0.2.254 │ 127.0.2.240/28 │ 2016-02-28 │ 2016-02-29
 3 │  4 │ 127.0.2.255 │ 127.0.2.240/28 │ 2016-02-29 │ 2016-02-29
 4 │  5 │ 127.0.3.0   │ 127.0.3.0/28   │ 2016-03-01 │ 2016-03-31
 5 │  6 │ 127.0.3.1   │ 127.0.3.0/28   │ 2016-03-02 │ 2016-03-31
 6 │  7 │ 127.0.3.2   │ 127.0.3.0/28   │ 2016-03-03 │ 2016-03-31
(7 rows)
~~~

## Conclusion

PostgreSQL comes with a rich set of data types, and an extensibility
framework that allows extension authors to add to the list. Each data type
is both a consistency constraint when it checks for valid input data input
times, and also implements the expected behavior in terms of operators and
processing functions on top of each data type.

Use PostgreSQL data types and rely on their advanced processing functions
and operators to distribute computations near to the data when it makes
sense for you!

{{< figure class="right"
             src="/img/MasteringPostgreSQLinAppDev-Cover-th.png"
            link="https://masteringpostgresql.com" >}}
            
This articles in this series are an extract from my book [Mastering
PostgreSQL in Application Development](https://masteringpostgresql.com),
which teaches SQL to developers so that they may replace thousands of lines
of code with very simple queries. The book has a full chapter about data
types in PostgreSQL, check it out!


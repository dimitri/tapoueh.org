+++
title = "PostgreSQL Data Types: Arrays"
date = "2018-04-20T14:47:25+02:00"
tags = ["PostgreSQL","YeSQL","Data Types","Arrays"]
categories = ["PostgreSQL","YeSQL"]
coverImage = "/img/Voyjbk.jpg"
coverMeta = "in"
coverSize = "partial"
thumbnailImage = "/img/matrix.png"
thumbnailImagePosition = "left"

+++

Continuing our series of [PostgreSQL Data Types](/tags/data-types/) today
we're going to introduce the PostgreSQL array data types.

Arrays can be used to denormalize data and avoid lookup tables. A good rule
of thumb for using them that way is that you mostly use the array as a
whole, even if you might at times search for elements in the array. Heavier
processing is going to be more complex than a lookup table.

<!--more-->
<!--toc-->


## A Data Type to Play With

A classic example of a good use case for PostgreSQL arrays is user-defined
tags. For the next example, [200,000 USA geolocated
tweets](http://followthehashtag.com/datasets/free-twitter-dataset-usa-200000-free-usa-tweets/)
have been loaded into PostgreSQL thanks to the following script:

~~~ sql
begin;

create table tweet
 (
   id         bigint primary key,
   date       date,
   hour       time,
   uname      text,
   nickname   text,
   bio        text,
   message    text,
   favs       bigint,
   rts        bigint,
   latitude   double precision,
   longitude  double precision,
   country    text,
   place      text,
   picture    text,
   followers  bigint,
   following  bigint,
   listed     bigint,
   lang       text,
   url        text
 );

\copy tweet from 'tweets.csv' with csv header delimiter ';'

commit;
~~~

Once the data is loaded we can have a look at it:

~~~ sql
\pset format wrapped
\pset columns 70
table tweet limit 1;
~~~

Here's what it looks like:

~~~ psql
─[ RECORD 1 ]────────────────────────────────────────────────────────
id        │ 721318437075685382
date      │ 2016-04-16
hour      │ 12:44:00
uname     │ Bill Schulhoff
nickname  │ BillSchulhoff
bio       │ Husband,Dad,GrandDad,Ordained Minister, Umpire, Poker Pla…
          │…yer, Mets, Jets, Rangers, LI Ducks, Sons of Anarchy, Surv…
          │…ivor, Apprentice, O&A, & a good cigar
message   │ Wind 3.2 mph NNE. Barometer 30.20 in, Rising slowly. Temp…
          │…erature 49.3 °F. Rain today 0.00 in. Humidity 32%
favs      │ ¤
rts       │ ¤
latitude  │ 40.76027778
longitude │ -72.95472222
country   │ US
place     │ East Patchogue, NY
picture   │ http://pbs.twimg.com/profile_images/378800000718469152/53…
          │…5032cf772ca04524e0fe075d3b4767_normal.jpeg
followers │ 386
following │ 705
listed    │ 24
lang      │ en
url       │ http://www.twitter.com/BillSchulhoff/status/7213184370756…
          │…85382
~~~

We can see that the raw import schema is not a good fit for PostgreSQL
capabilities. The *date* and *hour* fields are separated for no good reason,
and it makes processing them less easy than when they form a *timestamptz*
together. PostgreSQL does know how to handle *longitude* and *latitude* as a
single *point* entry, allowing much more interesting processing again. We
can create a simpler relation to manage and process a subset of the data
we're interested in for this chapter.

## Introduction to Arrays

PostgreSQL has built-in support for arrays, which are documented in the
[Arrays](https://www.postgresql.org/docs/current/static/arrays.html) and the
[Array Functions and
Operators](https://www.postgresql.org/docs/current/static/functions-array.html)
chapters. As introduced above, what's interesting with PostgreSQL is its
ability to process array elements from SQL directly. This capability
includes indexing facilities thanks to
[GIN](https://www.postgresql.org/docs/current/static/gin-intro.html)
indexing.

As we are interested in the tags used in the messages, the next query also
extracts all the tags from the Twitter messages as an array of text.

~~~ sql
begin;

create table hashtag
 (
   id         bigint primary key,
   date       timestamptz,
   uname      text,
   message    text,
   location   point,
   hashtags   text[]
 );

with matches as (
  select id,
         regexp_matches(message, '(#[^ ,]+)', 'g') as match
    from tweet
),
    hashtags as (
  select id,
         array_agg(match[1] order by match[1]) as hashtags
    from matches
group by id
)    
insert into hashtag(id, date, uname, message, location, hashtags)
     select id,
            date + hour as date,
            uname,
            message,
            point(longitude, latitude),
            hashtags
       from      hashtags
            join tweet using(id);

commit;
~~~

The PostgreSQL matching function *regexp_matches()* implements what we need
here, with the *g* flag to return every match found and not just the first
tag in a message. Those multiple matches are returned one per row, so we
then *group by* tweet id and *array_agg* over them, building our array of
tags. Here's what the computed data looks like:

~~~ sql
select id, hashtags
  from hashtag
 limit 10;
~~~

In the following data output, you can see that we kept the *#* signs in
front of the hashtags, making it easier to recognize what this data is:

~~~ psql
         id         │                    hashtags                     
════════════════════╪═════════════════════════════════════════════════
 720553447402160128 │ {#CriminalMischief,#ocso,#orlpol}
 720553457015324672 │ {#txwx}
 720553458596757504 │ {#DrugViolation,#opd,#orlpol}
 720553466804989952 │ {#Philadelphia,#quiz}
 720553475923271680 │ {#Retail,#hiring!,#job}
 720553508190052352 │ {#downtown,#early…,#ghosttown,#longisland,#morn…
                    │…ing,#portjeff,#portjefferson}
 720553522966581248 │ {"#CapitolHeights,",#Retail,#hiring!,#job}
 720553530088669185 │ {#NY17}
 720553531665682434 │ {#Endomondo,#endorphins}
 720553532273795072 │ {#Job,#Nursing,"#Omaha,",#hiring!}
(10 rows)
~~~

## Indexing PostgreSQL Arrays for Statistics and Profit

Before processing the tags, we create a specialized *GIN* index. This index
access method allows PostgreSQL to index the *contents* of the arrays, the
tags themselves, rather than each array as an opaque value.

~~~ sql
create index on hashtag using gin (hashtags);
~~~

A popular tag in the dataset is *#job*, and we can easily see how many times
it's been used, and confirm that our previous index makes sense for looking
inside the *hashtags* array:

~~~ sql
explain (analyze, verbose, costs off, buffers)
 select count(*)
  from hashtag
 where hashtags @> array['#job'];
~~~ 

And we can see the execution plan:

~~~ psql
                              QUERY PLAN                              
══════════════════════════════════════════════════════════════════════
 Aggregate (actual time=27.227..27.227 rows=1 loops=1)
   Output: count(*)
   Buffers: shared hit=3715
   ->  Bitmap Heap Scan on public.hashtag (actual time=13.023..23.453…
… rows=17763 loops=1)
         Output: id, date, uname, message, location, hashtags
         Recheck Cond: (hashtag.hashtags @> '{#job}'::text[])
         Heap Blocks: exact=3707
         Buffers: shared hit=3715
         ->  Bitmap Index Scan on hashtag_hashtags_idx (actual time=1…
…1.030..11.030 rows=17763 loops=1)
               Index Cond: (hashtag.hashtags @> '{#job}'::text[])
               Buffers: shared hit=8
 Planning time: 0.596 ms
 Execution time: 27.313 ms
(13 rows)
~~~

That was done supposing we already know one of the popular tags. How do we
get to discover that information, given our data model and data set? We do
it with the following query:

~~~ sql
  select tag, count(*)
    from hashtag, unnest(hashtags) as t(tag)
group by tag
order by count desc
   limit 10;
~~~

This time, as the query must scan all the hashtags in the table, it won't
use the previous index of course. The *unnest()* function is a must-have
when dealing with arrays in PostgreSQL, as it allows processing the array's
content as if it were just another relation. And SQL comes with all the
tooling to process relations, as we see in great details in my book
[Mastering PostgreSQL in Application
Development](https://masteringpostgresql.com).

So we can see the most popular hashtags in our dataset:

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

The hiring theme is huge in this dataset. We could then search for mentions
of job opportunities in the *#Retail* sector (another popular hashtag we
just discovered into the data set), and have a look at the locations where
they are saying they're hiring:

~~~ sql
  select name,
         substring(timezone, '/(.*)') as tz,
         count(*)
    from hashtag
    
         left join lateral
         (
            select *
              from geonames
          order by location <-> hashtag.location
             limit 1
         )
         as geoname
         on true
  
   where hashtags @> array['#Hiring', '#Retail']
   
group by name, tz
order by count desc
   limit 10;
~~~

For this query a dataset of *geonames* has been imported. The *left join
lateral* allows picking the nearest location to the tweet location from our
*geoname* reference table. The *where* clause only matches the hashtag
arrays containing both the *#Hiring* and the *#Retail* tags. Finally, we
order the data set by most promising opportunities:

~~~ psql
                       name                       │     tz      │ count 
══════════════════════════════════════════════════╪═════════════╪═══════
 San Jose City Hall                               │ Los_Angeles │    31
 Sleep Inn & Suites Intercontinental Airport East │ Chicago     │    19
 Los Angeles                                      │ Los_Angeles │    14
 Dallas City Hall Plaza                           │ Chicago     │    12
 New York City Hall                               │ New_York    │    11
 Jw Marriott Miami Downtown                       │ New_York    │    11
 Gold Spike Hotel & Casino                        │ Los_Angeles │    10
 San Antonio                                      │ Chicago     │    10
 Shoppes at 104                                   │ New_York    │     9
 Fruitville Elementary School                     │ New_York    │     8
(10 rows)
~~~

## Conclusion

PostgreSQL arrays are very powerful, and
[GIN](https://www.postgresql.org/docs/current/static/gin-intro.html)
indexing support makes them efficient to work with. Nonetheless, it's still
not so efficient that you would replace a lookup table with an array in
situations where you do a lot of lookups, though.

Also, some PostgreSQL array functions show a quadratic behavior: looping
over arrays elements really is inefficient, so learn to use *unnest()*
instead, and filter elements with a *where* clause. If you see yourself
doing that a lot, it might be a good sign that you really needed a lookup
table!

{{< figure class="right"
             src="/img/MasteringPostgreSQLinAppDev-Cover-th.png"
            link="https://masteringpostgresql.com" >}}
            
This article is an extract from my book [Mastering PostgreSQL in Application
Development](https://masteringpostgresql.com), which teaches SQL to
developers so that they may replace thousands of lines of code with very
simple queries. The book has a full chapter about data types in PostgreSQL,
check it out!

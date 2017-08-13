+++
date = "2013-10-24T13:40:00.000000+02:00"
title = "Denormalizing Tags"
tags = ["PostgreSQL", "Extensions", "intagg", "YeSQL"]
categories = ["PostgreSQL","YeSQL"]
thumbnailImage = "/img/old/wordpres-seo-categories-tags.jpg"
thumbnailImagePosition = "left"
coverImage = "/img/rock-punk-metal-music-news.jpg"
coverSize = "partial"
coverMeta = "in"
aliases = ["/blog/2013/10/23-denormalizing-tags",
           "/blog/2013/10/23-denormalizing-tags.html"]
+++

In our 
[Tour of Extensions](/tags/extensions) today's article is about advanced tag indexing. We
have a great data collection to play with and our goal today is to be able
to quickly find data matching a complex set of tags. So, let's find out
those 
[lastfm](http://www.lastfm.fr/) tracks that are tagged as 
*blues* and 
*rhythm and blues*, for
instance.

<!--toc-->

We're going to use
the
[Last.fm dataset from the Million Song Dataset project](http://labrosa.ee.columbia.edu/millionsong/lastfm) here.
The article where they present their similarity searches is providing the
whole dataset they have as a [SQLite](http://www.sqlite.org/) database and
they propose to read their python
script
[demo_tags_db.py](http://labrosa.ee.columbia.edu/millionsong/sites/default/files/lastfm/demo_tags_db.py) to
make sense of it.

# The Setup

First, we need to import this dataset into a PostgreSQL database. To do that
the easier path I could think of was to hack the capability into 
*pgloader* of
course, so here we go:

~~~
                    table name       read   imported     errors       time
------------------------------  ---------  ---------  ---------  ---------
                          tags          0     522366          0      3.922
                          tids          0     505216          0      3.746
                       tid_tag          0    8598630          0    115.349
        index build completion          0          0          0     33.099
------------------------------  ---------  ---------  ---------  ---------
                  create index          0          8          0     38.839
               reset sequences          0          0          0      0.064
------------------------------  ---------  ---------  ---------  ---------
          Total streaming time          0    9626212          0  2m36.180s
~~~


{{< image classes="fig25 right dim-margin" src="/img/old/sqlite.gif" >}}

Here, 
*pgloader* extracted the table and index definitions from the SQLite
database using the 
`sqlite_master` catalog and the 
`PRAGMA table_info()`
commands, and migrated the data in a streaming fashion down to PostgreSQL,
using the 
*COPY protocol*.

Having a look at the 
*demo_tags.py* script we can actually see how to use the
relations here, and we realize they are using the
[64-bit signed integer ROWID](http://www.sqlite.org/autoinc.html) system column. We need something comparable to
be able to make sense of the data:

~~~ sql
> alter table tags add column rowid serial;
ALTER TABLE
Time: 3177.603 ms

> alter table tids add column rowid serial;
ALTER TABLE
Time: 2528.680 ms

> SELECT tags.tag, COUNT(tid_tag.tid)
    FROM tid_tag, tags
   WHERE tid_tag.tag=tags.ROWID and tags.tag ~* 'setzer'
GROUP BY tags.tag;
             
             tag             | count 
-----------------------------+-------
 the brian setzer orchestra  |     1
 Setzer                      |    13
 rockabilly Setzer style     |     4
 setzer is a true guitarhero |     9
 brian setzer orchestra      |     3
 brian setzer is GOD         |     1
 Brian Setzer                |     1
 brain setzer orchestra      |     2
(8 rows)

Time: 644.826 ms
~~~


Here the query is mainly doing a 
*JOIN* in between the 
*tid* table (containing
track ids) and the 
*tid_tag* table (containing association in between tracks
and tags), filtering on the 
*case insensitive regular expression* 
`'setzer'`. As
we can imagine from reading the query execution time, we don't have any
index to implement the filtering here.

# Advanced tag indexing

PostgreSQL comes with plenty of interesting datatypes, one of them is known
as the 
[Arrays Type](http://www.postgresql.org/docs/9.3/interactive/arrays.html). PostgreSQL also provides a very rich set of extensions,
some of them found under the 
*contrib* package; one of them is 
[intarray](http://www.postgresql.org/docs/9.3/interactive/intarray.html). Let
me quote for you the most interesting part of the documentation for that
extension:

> The @@ and ~~ operators test whether an array satisfies a query, which is
> expressed as a value of a specialized data type query_int. A query consists
> of integer values that are checked against the elements of the array,
> possibly combined using the operators & (AND), | (OR), and ! (NOT).
> Parentheses can be used as needed. For example, the query 1&(2|3) matches
> arrays that contain 1 and also contain either 2 or 3.


~~~ sql
> create extension intarray;
CREATE EXTENSION
~~~


The way the 
*intarray* extension works, we need to build a new table that
contains for each track the list of tags it's been associated with, as an
array of integers. We're going to use our 
*rowid* identifier for that purpose,
as in the following query:

~~~ sql
>  SELECT tt.tid, array_agg(tags.rowid) as tags
     FROM tags JOIN tid_tag tt ON tags.rowid = tt.tag
 GROUP BY tt.tid
    LIMIT 3;
    
 tid |   tags    
-----+-----------
   1 | {1,2}
   2 | {3,4}
   3 | {5,6,7,8}
(3 rows)

Time: 942.074 ms
~~~


So let's build the full table then index it:

~~~ sql
> CREATE TABLE track_tags AS
   SELECT tt.tid, array_agg(tags.rowid) as tags
     FROM tags join tid_tag tt on tags.rowid = tt.tag
 GROUP BY tt.tid;
SELECT 505216
Time: 45388.424 ms

> create index on track_tags using gin(tags gin__int_ops);
CREATE INDEX
Time: 18645.931 ms
~~~



# Searches

Now 
[PostgreSQL](http://www.postgresql.org/) is ready for the real magic. Let's find all the tracks we
have that have been tagged as both 
*blues* and 
*rhythm and blues*:

~~~ sql
> select array_agg(rowid)
    from tags
   where tag = 'blues' or tag = 'rhythm and blues';
 
 array_agg 
-----------
 {3,739}
(1 row)

Time: 0.684 ms
~~~


Now what we want is a 
*query_int* query string, which looks like
`'(1880&179879)'`, so rather than just 
`array_agg` we're going to use the
following query:

~~~ sql
> select format('(%s)',
            array_to_string(array_agg(rowid), '&')
         )::query_int as query
    from tags
   where tag = 'blues' or tag = 'rhythm and blues';
  
  query  
---------
 3 & 739
(1 row)

Time: 0.747 ms
~~~


That query here allows us to easily inject as many tags as we want to, so
that it's easy to use it as a 
*template* from within an application where the
user is going to provide for the tags list. The 
*intarray* extension's 
*query*
format also accepts other operators (
*or* and 
*not*) as we saw before, so if you
want to expose those to your users you would need to tweak the 
*query_int*
building part of the SQL.

Now, how many tracks have been tagged with 
***both*** the 
*blues* and the 
*rhythm and
blues* tags, will you ask me:

~~~ sql
> with t(query) as (
       select format('(%s)',
                array_to_string(array_agg(rowid), '&')
             )::query_int as query
        from tags
       where tag = 'blues' or tag = 'rhythm and blues'
  )
select count(*) from track_tags, t
 where tags @@ query;
 
 count 
-------
  2278
(1 row)

Time: 8.242 ms
~~~


Now of course you might want to fetch some track meta-data, here the only
one we have is the track 
*hash id*:

~~~ sql
> with t(query) as (
    select format('(%s)',
                    array_to_string(array_agg(rowid), '&')
           )::query_int as query
      from tags
       where tag = 'blues' or tag = 'rhythm and blues'
)
 select track.tid
   from track_tags tt join tids track on tt.tid = track.rowid, t
  where tt.tags @@ t.query
  limit 10;
        tid         
--------------------
 TRCJLCC12903CBF4AE
 TRCIFOV128F92F6F4C
 TRCYUVJ128F425C8F1
 TRCNTFO128F92F6564
 TRCDRGT12903CE64BF
 TRCWAED128F42A837B
 TRCWFEM128F9320F94
 TRCQCQH128F932E707
 TRCUMTA12903CD67EE
 TRJJYUT12903CFB13B
(10 rows)

Time: 7.630 ms
~~~



# Conclusion

{{< image classes="fig25 right dim-margin" src="/img/old/rhythm-blues-final4.320.jpg" >}}

The usual way to handle a set of user defined tags and query against it
involves join against a reference table of tags, but then it's quite
complicated to express the full search query: we want tracks tagged with
both 
*blues* and 
*rhythm and blues* and might want then to exclude 
*finger
picking*.

The 
[intarray](http://www.postgresql.org/docs/9.3/interactive/intarray.html) extension provides a powerful 
*query specialized language* with
direct index support, so that you can build dynamic indexes searches
directly from your application.

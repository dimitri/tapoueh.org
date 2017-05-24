+++
date = "2013-09-06T16:15:00.000000+02:00"
title = "Using trigrams against typos"
tags = ["PostgreSQL", "Extensions", "pg_trgm", "YeSQL"]
categories = ["PostgreSQL","YeSQL"]
thumbnailImage = "/img/old/trigramme.png"
thumbnailImagePosition = "left"
coverImage = "/img/yin_yang_trigrams_by_chrysdives.jpg"
coverSize = "partial"
coverMeta = "out"
aliases = ["/blog/2013/09/06-pg_trgm-suggestions",
           "/blog/2013/09/06-pg_trgm-suggestions.html"]
+++

In our ongoing 
[Tour of Extensions](/tags/extensions) we played with 
[earth distance](http://www.postgresql.org/docs/current/static/earthdistance.html) in
[How far is the nearest pub?](/blog/2013/08/05-earthdistance) then with 
[hstore](http://www.postgresql.org/docs/current/static/hstore.html) in a series about trigger,
first to generalize 
[Trigger Parameters](/blog/2013/08/23-parametrized-triggers) then to enable us to
[Auditing Changes with Hstore](/blog/2013/08/27-auditing-changes-with-hstore). Today we are going to work with 
[pg_trgm](http://www.postgresql.org/docs/current/static/pgtrgm.html) which
is the 
*trigrams* PostgreSQL extension: its usage got seriously enhanced in
recent PostgreSQL releases and it's now a poor's man 
[Full Text Search](http://www.postgresql.org/docs/current/static/textsearch.html)
engine.

<!--more-->
<!--toc-->

Of course we also have the rich men version with 
[Text Search Parsers](http://www.postgresql.org/docs/current/static/textsearch-parsers.html) and
several kinds of 
[dictionnaries](http://www.postgresql.org/docs/current/static/textsearch-dictionaries.html) with support for 
*stemming*, 
*thesaurus* or
*synomyms* support, and 
[a full text query language](http://www.postgresql.org/docs/current/static/textsearch-controls.html#TEXTSEARCH-PARSING-QUERIES) and tools for
[ranking search result](http://www.postgresql.org/docs/current/static/textsearch-controls.html#TEXTSEARCH-RANKING). So if what you need really is 
**Full Text Search** then
go check the docs.

The use 
***trigrams*** is often complementary to 
*Full Text Search*. With trigrams
we can implement typing correction suggestions or index 
`like` and
[POSIX Regular Expressions](http://www.postgresql.org/docs/current/static/functions-matching.html) searches.

Whatever the use case, it all begins as usual by enabling the extension
within your database server. If you're running from 
[PostgreSQL packages](http://www.postgresql.org/download/) be
sure to always install the 
`contrib` package, really. A time will come when
you need it and you will then be happy to only have to type 
`CREATE EXTENSION`
to get started.

~~~ sql
# create extension pg_trgm;
CREATE EXTENSION
~~~



# Setting up the use case

The use case I want to show today is to suggest corrections to some words
the user did obviously typoed, because your search form is not finding any
result. Or to offer suggest as you type feature maybe, doing a database
search for approximate matching strings in a kind of 
*catalog* that you have
to offer auto-completion.

One easy to use catalog here is the 
[Dell DVD Store Database Test Suite](http://linux.dell.com/dvdstore/) that
you can download also as a ready to use PostgreSQL text dump at
[http://pgfoundry.org/frs/download.php/543/dellstore2-normal-1.0.tar.gz](http://pgfoundry.org/frs/download.php/543/dellstore2-normal-1.0.tar.gz).

{{< image classes="fig25 right dim-margin" src="/img/old/ComputerCat.jpg" >}}

This small database offers ten thousands 
*products* and simplifies the schema
so much as to offer a single column 
*actor* in the 
*products* table. Let's
pretend we just filled in a search box to find products by actor name, but
we don't know the right spelling of the actor's name or maybe the cat really
wanted to help us on the keyboard that day.

The 
*trigram* extension comes with two operators of interest for this
situation here, which are the 
*similarity* operator named 
`%` and the 
*distance*
operator named 
`<->`. The 
*similarity* operator will compare the list of
trigrams extracted from the query terms with those extracted from each data
of our table, and filter out those rows where the data is considered not
similar enough.

~~~ sql
> select show_trgm('tomy') as tomy,
         show_trgm('Tomy') as "Tomy",
         show_trgm('tom torn') as "tom torn",
         similarity('tomy', 'tom'),
         similarity('dim', 'tom');

-[ RECORD 1 ]-------------------------------------
tomy       | {"  t"," to","my ",omy,tom}
Tomy       | {"  t"," to","my ",omy,tom}
tom torn   | {"  t"," to","om ",orn,"rn ",tom,tor}
similarity | 0.5
similarity | 0
~~~


As you can read in the 
[PostgreSQL trigram extension documentation](http://www.postgresql.org/docs/current/static/pgtrgm.html) the
default similarity threshold is 
`0.3` and you can tweak it by using the
functions 
`set_limit()`.

Now let's find out all those actors whose name looks like 
*tomy*, as clearly
the user did enter that in the search box but we found no exact match for
it:

~~~ sql
> select *
    from products
   where actor ~* 'tomy';
 
 prod_id | category | title | actor | price | special | common_prod_id 
---------+----------+-------+-------+-------+---------+----------------
(0 rows)

> select actor
    from products
   where actor % 'tomy';
  
  actor   
----------
 TOM TORN
 TOM DAY
(2 rows)

Time: 26.972 ms
~~~


# Trigram indexing

{{< image classes="fig25 left dim-margin" src="/img/old/macrex-index.320.gif" >}}


That's a little too much time on that query when we consider only 10,000
entries in our table, let's try and do better than that:

~~~ sql
# create index on products using gist(actor gist_trgm_ops);
CREATE INDEX
~~~


Now if we run the exact same query we get our result in less than 
*3
milliseconds*, which is more like something we can push to production.

~~~ sql
> select actor
    from products
   where actor % 'tomy';
  
  actor   
----------
 TOM TORN
 TOM DAY
(2 rows)

Time: 2.695 ms
~~~


Oh and by the way, did you know that the 
`~*` operator we used above to
discover that there's not a single 
*Tony* actor in our products table, that 
`~*`
operator implements a 
*case insensitive posix regex search* in PostgreSQL?
Isn't that awesome? Now, on to the next surprise, have a look at that
explain plan:

~~~ sql
> explain (costs off)
  select * from products where actor ~* 'tomy';
                   QUERY PLAN                    
-------------------------------------------------
 Index Scan using products_actor_idx on products
   Index Cond: ((actor)::text ~* 'tomy'::text)
(2 rows)
~~~


In PostgreSQL 9.3 the trigram extension is able to solve regular expression
searches. The first production release of 9.3 should happen as soon as next
week, I hope you're ready for it!

# Auto Completion

What if you want to offer as-you-type completion to the names of the actors
we know in our catalog? Then maybe you will find the following query useful:

~~~ sql
>   select actor
      from products
     where actor % 'fran'
  order by actor <-> 'fran'
     limit 10;
    actor     
--------------
 FRANK HAWKE
 FRANK BERRY
 FRANK POSEY
 FRANK HAWKE
 FRANCES DEE
 FRANK LEIGH
 FRANCES DAY
 FRANK FOSTER
 FRANK HORNE
 FRANK TOMEI
(10 rows)

Time: 2.960 ms
~~~


Note that without the 
`WHERE` clause to filter on the trigram similarity I get
run times of 30ms rather than 3ms in my tests here, because the GiST index
is not used then. As usual 
`EXPLAIN` is your friend and remember that a query
plan will change depending on the volume of your data set as known by the
[PostgreSQL planner statistics](http://www.postgresql.org/docs/current/static/routine-vacuuming.html#VACUUM-FOR-STATISTICS).


# Conclusion

{{< image classes="fig25 right fancybox dim-margin" src="/img/old/logo_man_tool_open_300x_watermark.jpg" >}}

The 
[trigram extension](http://www.postgresql.org/docs/current/static/pgtrgm.html) allows indexing 
`like` searches and 
`regular expression`
searches, and also know how to compute 
*similarity* and 
*distance* in between
texts, and how to index that. That's another power tool included with
PostgreSQL. Another reason why you won't believe how much behind the other
database systems you know of really are, if you ask me.

Oh, and get ready for PostgreSQL 9.3. Another release packed with awesome.

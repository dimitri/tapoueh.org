+++
date = "2013-08-05T08:11:00.000000+02:00"
title = "How far is the nearest pub?"
tags = ["PostgreSQL", "Extensions", "earthdistance", "YeSQL"]
categories = ["PostgreSQL","YeSQL"]
thumbnailImage = "/img/old/latitude_and_longitude.jpg"
thumbnailImagePosition = "left"
coverImage = "/img/old/latitude_and_longitude.jpg"
coverSize = "partial"
coverMeta = "out"
aliases = ["/blog/2013/08/05-earthdistance",
           "/blog/2013/08/05-earthdistance.html"]
+++

In our recent article about 
[The Most Popular Pub Names](/blog/2013/08/02-pub-names-knn) we did have a look at
how to find the pubs nearby, but didn't compute the 
**distance** in between that
pub and us. That's because how to compute a distance given a position on the
earth expressed as 
*longitude* and 
*latitude* is not that easy. Today, we are
going to solve that problem nonetheless, thanks to 
[PostgreSQL Extensions](http://www.postgresql.org/docs/9.2/interactive/extend-extensions.html).


*Some math are required to go from (long, lat) to distance on earth*

## The earthdistance PostgreSQL contrib

As the maths are complex enough to easily make mistakes when implementing
them again, we want to find an existing implementation that's been tested
already. PostgreSQL provides several 
[contrib](http://www.postgresql.org/docs/9.2/static/contrib.html) extensions, one of those is
named 
[earthdistance](http://www.postgresql.org/docs/9.2/static/earthdistance.html) and is made to solve our problem. Time to try it!

~~~
# create extension cube;
# create extension earthdistance;
~~~


Equiped with that extension we can now use its 
`<@>` operator and compute a
distance in miles at the surface of the earth, given points as 
*(longitude,
latitude)*. So I had to import our data set again with points in the right
representation, then I could run this query:

~~~
#  select id, name, pos,
          round((pos <@> point(-0.12,51.516))::numeric, 3) as miles
     from pubnames
 order by pos <-> point(-0.12,51.516)
    limit 10;
     id     |          name          |           pos           | miles 
------------+------------------------+-------------------------+-------
   21593238 | All Bar One            | (-0.1192746,51.5163499) | 0.039
   26848690 | The Shakespeare's Head | (-0.1194731,51.5167871) | 0.059
  371049718 | The Newton Arms        | (-0.1209811,51.5163032) | 0.047
  438488621 | Marquis Cornwallis     | (-0.1199612,51.5146691) | 0.092
   21593236 | Ship Tavern            | (-0.1192378,51.5172525) | 0.093
  312156665 | The Prince of Wales    | (-0.121732,51.5145794)  | 0.123
  312156722 | O'Neills               | (-0.1220195,51.5149538) | 0.113
   25508632 | Friend at Hand         | (-0.1224717,51.5148694) | 0.132
  338507304 | The Square Pig         | (-0.1191744,51.5187089) | 0.191
 1975855516 | Holborn Whippet        | (-0.1216925,51.5185189) | 0.189
(10 rows)

Time: 1.335 ms
~~~


So the nearest pub is 
*All Bar One*, 0.039 miles away, or 68.64 yards
apparently. And we can see that adding the computation to get the distance
in 
*miles* didn't add that much to the query timing.


{{< image classes="fig50 center fancybox dim-margin" src="/img/old/pubstopslondon.jpg" >}}



## Pubs and cities

Just as easily as we have 
*nearest* pubs we can also of course query for pubs
*farthest* away from any location.

~~~
#   select name, round((pos <@> point(-0.12,51.516))::numeric, 3) as miles
      from pubnames
  order by pos <-> point(-0.12,51.516) desc
     limit 5;
      name       |  miles  
-----------------+---------
 Tig Bhric       | 440.194
 TP's            | 439.779
 Begley's        | 439.752
 Ventry Inn      | 438.962
 Fisherman's Bar | 439.153
(5 rows)

Time: 74.780 ms
~~~


Now we want to know what city are those pubs in right? With the following
URL and using the 
[Open Street Map](http://www.openstreetmap.org/) APIs, I've been able to download a list of
cities in the same area as where the pub names were fetched in:
`http://www.overpass-api.de/api/xapi?*[place=city][bbox=-10.5,49.78,1.78,59]`.

Tweaking the parser and import code at 
[https://github.com/dimitri/pubnames](https://github.com/dimitri/pubnames)
was easy, and allowed to import those city names and locations in 
*0.087
seconds of real time*, with the following schema:

~~~
# create table if not exists cities (id bigint, pos point, name text);
# create index on cities using gist(pos);
~~~


Now let's see where are those far away pubs:

~~~
#   select name,
          (select name from cities c order by c.pos <-> p.pos limit 1) as city,
          round((pos <@> point(-0.12,51.516))::numeric, 3) as miles
     from pubnames p
 order by pos <-> point(-0.12,51.516) desc
    limit 5;
      name       |  city  |  miles  
-----------------+--------+---------
 Tig Bhric       | Galway | 440.194
 TP's            | Galway | 439.779
 Begley's        | Galway | 439.752
 Ventry Inn      | Galway | 438.962
 Fisherman's Bar | Cork   | 439.153
(5 rows)

Time: 686.444 ms
~~~



{{< image classes="fig50 center fancybox dim-margin" src="/img/old/ninedotslines.gif" >}}


*Using `LATERAL` Joins is not a form of lateral thinking*

As you can see we are fetching the pubs at a distance from our given point
and then the nearest city from where the pub is. The way it's implemented
here is called a 
*correlated subquery*, and starting with 9.3 we will be able
to use the 
[LATERAL](http://www.postgresql.org/docs/devel/static/queries-table-expressions.html#QUERIES-LATERAL) standard join construct, as in the following example:

~~~
#   select c.name as city, p.name,
           round((pos <@> point(-0.12,51.516))::numeric, 3) as miles
      from pubnames p,
           lateral (select name
                      from cities c
                  order by c.pos <-> p.pos
                     limit 1) c
  order by pos <-> point(-0.12,51.516) desc
     limit 5;
  city  |      name       |  miles  
--------+-----------------+---------
 Galway | Tig Bhric       | 440.194
 Galway | TP's            | 439.779
 Galway | Begley's        | 439.752
 Galway | Ventry Inn      | 438.962
 Cork   | Fisherman's Bar | 439.153
(5 rows)

Time: 636.445 ms
~~~


So apparently the 
*bounded box* that we've been given
(
`[bbox=-10.5,49.78,1.78,59]`) includes Ireland too... and more
importantly the query execution penalty is quite important. That's because
the planner only know how to solve that query by doing 
`Index Scan
using cities_pos_idx on public.cities c (cost=0.14..9.60 rows=73 width=25)
(actual time=0.016..0.016 rows=1 loops=27878)`, which means scanning
the position index of the cities 27878 times (once per pubnames entry).

It's possible to force the planner into doing it the obvious way though:

~~~
#   with pubs as (
        select name, pos,
               round((pos <@> point(-0.12,51.516))::numeric, 3) as miles
          from pubnames
      order by pos <-> point(-0.12,51.516) desc
         limit 5
    )
    select c.name as city, p.name, p.miles
      from pubs p, lateral (select name
                              from cities c
                          order by c.pos <-> p.pos
                             limit 1) c;
  city  |      name       |  miles  
--------+-----------------+---------
 Galway | Tig Bhric       | 440.194
 Galway | TP's            | 439.779
 Galway | Begley's        | 439.752
 Galway | Ventry Inn      | 438.962
 Cork   | Fisherman's Bar | 439.153
(5 rows)

Time: 76.467 ms
~~~



{{< image classes="fig50 center fancybox dim-margin" src="/img/old/bite-top-ten-pub-names.jpg" >}}



## The Most Popular Pub Names, per City

Let's now find which cities have the highest count of pubs, considering that
a pub is affiliated to a city if it's within 5 miles of the single point we
have as city location in our data set.

~~~
#   select c.name, count(cp)
      from cities c, lateral (select name
                                from pubnames p
                               where (p.pos <@> c.pos) < 5) as cp
  group by c.name
  order by count(cp) desc
  limit 10;

    name     | count 
-------------+-------
 London      |  1388
 Westminster |  1383
 Dublin      |   402
 Manchester  |   306
 Bristol     |   292
 Leeds       |   292
 Edinburgh   |   286
 Liverpool   |   258
 Nottingham  |   218
 Glasgow     |   217
(10 rows)

Time: 562.678 ms
~~~


If we look at a map we see that 
*Westminster* is in fact within 
*London* given
our arbitrary rule of 
*within 5 miles*, so in the next query we will simply
filter it out. Exercise left to the reader: write a query allowing to remove
from London's count the pubs that are actually in Westminster (when within 1
mile of the location we have for it). Then extend that query to address any
other situation like that in the whole data set.


{{< image classes="fig50 center fancybox dim-margin" src="/img/old/sql-logo.png" >}}


*Often the most powerful tool you have to make sense of your data...*

And now what about the most popular pub names per city? Of course we want to
normalize again our pub names here but only for counting: we still display
all the names we did count.

~~~
#   select c.name,
           array_to_string(array_agg(distinct(cp.name) order by cp.name), ', '),
           count(*)
      from cities c,
           lateral (select name
                      from pubnames p
                     where (p.pos <@> c.pos) < 5) as cp
     where c.name <> 'Westminster'
  group by c.name, replace(replace(cp.name, 'The ', ''), 'And', '&')
  order by count(*) desc
     limit 10;

   name   |            array_to_string             | count 
----------+----------------------------------------+-------
 London   | Prince of Wales, The Prince of Wales   |    15
 London   | All Bar One                            |    12
 London   | The Beehive                            |     8
 London   | O'Neills                               |     7
 London   | The Crown                              |     7
 London   | The Windmill                           |     7
 London   | Red Lion, The Red Lion                 |     6
 Bradford | New Inn, The New Inn                   |     6
 London   | Coach and Horses, The Coach and Horses |     6
 London   | The White Horse, White Horse           |     6
(10 rows)

Time: 729.866 ms
~~~



## Conclusion

As said in the previous article on the same theme, SQL when using 
[PostgreSQL](http://www.postgresql.org/)
is indeed quite powerful! We've been able to easily add an implementation of
the earth distance computation from 
*longitude* and 
*latitude* as found in the
[earthdistance](http://www.postgresql.org/docs/9.2/static/earthdistance.html) contrib extension (already packaged for your Operating System
of choice, be sure to install 
*contribs* by default), then to use it to solve
some interesting problems with our data set.

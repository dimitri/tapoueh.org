+++
date = "2013-05-02T11:34:00.000000+02:00"
title = "Nearest Big City"
tags = ["PostgreSQL", "KNN", "YeSQL", "Geolocation", "Point"]
categories = ["PostgreSQL","YeSQL"]
thumbnailImage = "/img/old/global_accessibility-640.png"
thumbnailImagePosition = "left"
coverImage = "/img/old/global_accessibility-640.png"
coverSize = "partial"
coverMeta = "in"
aliases = ["/blog/2013/05/02-nearest-big-city",
           "/blog/2013/05/02-nearest-big-city.html"]
+++

In this article, we want to find the town with the greatest number of
inhabitants near a given location.

<!--toc-->

## A very localized example

We first need to find and import some data, and I found at the following
place a 
[CSV listing of french cities with coordinates and population](http://www.lion1906.com/Pages/francais/utile/telechargements.html) and
some numbers of interest for the exercise here.

To import the data set, we first need a table, then a 
`COPY` command:

~~~ sql
CREATE TABLE lion1906 (
  insee       text,
  nom         text,
  altitude    integer,
  code_postal text,
  longitude   double precision,
  latitude    double precision,
  pop99       bigint,
  surface     double precision
);

\copy lion1906 from 'villes.csv' with csv header delimiter ';' encoding 'latin1'
~~~


With that data in place, we can find the 10 nearest towns of a random
choosing of us, let's pick 
*Villeurbanne* which is in the region of 
*Lyon*.

~~~ sql
select code_postal, nom, pop99
     from lion1906
 order by point(longitude, latitude) <->
          (select point(longitude, latitude)
             from lion1906
            where nom = 'Villeurbanne')
    limit 10;

 code_postal |          nom           | pop99  
-------------+------------------------+--------
 69100       | Villeurbanne           | 124215
 69300       | Caluire-et-Cuire       |  41233
 69120       | Vaulx-en-Velin         |  39154
 69580       | Sathonay-Camp          |   4336
 69140       | Rillieux-la-Pape       |  28367
 69000       | Lyon                   | 445452
 69500       | Bron                   |  37369
 69580       | Sathonay-Village       |   1693
 01700       | Neyron                 |   2157
 69660       | Collonges-au-Mont-d'Or |   3420
(10 rows)
~~~


We find Lyon in our list in there, and we want the query now to return only
that one as it has the greatest number of inhabitants in the list:

~~~ sql
with neighbours as (
   select code_postal, nom, pop99
     from lion1906
 order by point(longitude, latitude) <->
          (select point(longitude, latitude)
             from lion1906 where nom = 'Villeurbanne')
    limit 10
)
  select *
    from neighbours
order by pop99 desc
   limit 1;

 code_postal | nom  | pop99  
-------------+------+--------
 69000       | Lyon | 445452
(1 row)
~~~


Well, thank you PostgreSQL, that was easy!

Note that you can actually index such queries, that's called a 
*KNN index*.
PostgreSQL knows how to use some kind of indexes to fetch data matching an
expression such as 
`ORDER BY a <-> b`, which allow you to consider a 
*KNN*
search in your application.


## Let's get worldwide

The real scope of our exercise is to associate every known town in the world
with some big city around, so let's first fetch and import some worldwide
data this time, from
[http://download.maxmind.com/download/worldcities/worldcitiespop.txt.gz](maxmind).

<center>
{{< image classes="fig50 fancybox dim-margin" src="/img/old/map_nearest_city_01.gif" >}}
</center>

~~~ sql
CREATE TABLE maxmind_worldcities (
	country_code text,
	city_lower text,
	city_normal text,
	region_code text DEFAULT '',
	population INT DEFAULT '0',
	latitude float8 DEFAULT '0',
	longitude float8 DEFAULT '0'
);
 
\copy maxmind_worldcities FROM '/tmp/worldcitiespop.txt' WITH  DELIMITER ',' QUOTE E'\f' CSV HEADER ENCODING 'LATIN1';

alter table maxmind_worldcities add column loc point;
update maxmind_worldcities set loc = point(longitude, latitude);
~~~


This time you can see that I created an extra column with the 
*location* in
there, so that I don't have to compute it each time I need it, like I did
before.

Now is the time to test that data set and hopefully fetch the same result as
before when we only had french cities loaded:

~~~ sql
with neighbours as (
   select country_code, city_lower, population
     from maxmind_worldcities
    where population is not null
 order by loc <->
          (select loc
             from maxmind_worldcities
            where city_lower = 'villeurbanne')
   limit 10
)
   select * from neighbours order by population desc limit 1;

  country_code | city_lower | population 
--------------+------------+------------
 fr           | lyon       |     463700
(1 row)
~~~


Ok, looks like we're all set for the real problem. Now we want to pick for
each of those cities it's nearest neighboor, so here's how to do that:

~~~ sql
create index on maxmind_worldcities(country_code, region_code, city_lower);
create index on maxmind_worldcities using gist(loc);

create table maxmind_neighbours as
  select country_code, region_code, city_lower,
         (with neighbours as (
             select country_code, city_lower, population
               from maxmind_worldcities
              where population is not null
                    and country_code = wc.country_code
                    and region_code = wc.region_code
           order by loc <-> wc.loc
             limit 10)
             select city_lower
               from neighbours
           order by population desc
              limit 1
         ) as neighbour
    from maxmind_worldcities wc ;         
~~~


To be fair, I have to tell you that this query took almost 2 hours to
complete on my laptop here, but as I'm doing that for friend and a blog
article, I've been lazy and didn't try to optimise it. It could be using
`LATERAL` for sure, I don't know if that would help very much with
performances: I didn't try.

With that in hands we can now check some cities and their 
*biggest*
neighbours, as in the following query:

~~~ sql
select * from maxmind_neighbours where city_lower = 'villeurbanne';
 country_code | region_code |  city_lower  | neighbour 
--------------+-------------+--------------+-----------
 fr           | B9          | villeurbanne | lyon
(1 row)
~~~


And looking for New-York City suburbs I did find a 
*chinatown*, which is a
pretty common smaller town name apparently:

~~~ sql
select * from maxmind_neighbours where city_lower = 'chinatown';
 country_code | region_code | city_lower |   neighbour   
--------------+-------------+------------+---------------
 sb           | 08          | chinatown  | honiara
 us           | CA          | chinatown  | san francisco
 us           | DC          | chinatown  | washington
 us           | HI          | chinatown  | honolulu
 us           | IL          | chinatown  | chicago
 us           | MT          | chinatown  | missoula
 us           | NV          | chinatown  | reno
 us           | NY          | chinatown  | new york
(8 rows)
~~~



## Big Cities in the big world

<center>
{{< image classes="fig50 fancybox dim-margin" src="/img/old/Old-Photos-of-Big-Cities-21.jpg" >}}
</center>

<center>*We might need to change some of our views*</center>

So, let's see how many smaller towns each of those random big cities have:

~~~ sql
select country_code, region_code, neighbour, count(*)
    from maxmind_neighbours
   where neighbour in ('london', 'new york', 'moscow',
                       'paris', 'tokyo', 'sao polo', 'chicago')
group by country_code, region_code, neighbour;
 country_code | region_code | neighbour | count 
--------------+-------------+-----------+-------
 gb           | H9          | london    |     2
 jp           | 40          | tokyo     |   414
 us           | NY          | new york  |   131
 ca           | 08          | london    |    16
 ru           | 48          | moscow    |   245
 fr           | A8          | paris     |    16
 us           | IL          | chicago   |    13
(7 rows)
~~~


And now let's be fair and see where are the cities with the greatest number
of towns nearby them, with the following query:

~~~ sql
select country_code, region_code, neighbour, count(*)
    from maxmind_neighbours
   where neighbour is not null
group by country_code, region_code, neighbour
order by 4 desc
   limit 25;

 country_code | region_code | neighbour  | count 
--------------+-------------+------------+-------
 cn           | 03          | nanchang   | 16759
 cn           | 26          | xian       | 12864
 id           | 18          | kupang     | 10715
 cn           | 24          | taiyuan    | 10550
 mm           | 11          | taunggyi   | 10253
 id           | 38          | makasar    |  9471
 ir           | 15          | ahvaz      |  9461
 id           | 01          | banda aceh |  9161
 cn           | 14          | lasa       |  8841
 cn           | 15          | lanzhou    |  8618
 ir           | 29          | kerman     |  8579
 id           | 26          | medan      |  7787
 ir           | 04          | iranshahr  |  7249
 ir           | 07          | shiraz     |  7219
 ma           | 55          | agadir     |  7121
 ir           | 42          | mashhad    |  7107
 af           | 08          | gazni      |  7011
 ir           | 33          | tabriz     |  6586
 cn           | 01          | hefei      |  6521
 bd           | 81          | dhaka      |  6480
 ir           | 08          | rasht      |  6471
 id           | 17          | mataram    |  6467
 id           | 33          | cilegon    |  6287
 af           | 23          | qandahar   |  6213
 cn           | 07          | fuzhou     |  6089
(25 rows)
~~~


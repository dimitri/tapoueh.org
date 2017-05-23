+++
date = "2013-10-09T17:42:00.000000+02:00"
title = "Geolocation with PostgreSQL"
tags = ["PostgreSQL", "Extensions", "ip4r", "Catalogs", "YeSQL"]
categories = ["PostgreSQL","YeSQL"]
thumbnailImage = "/img/old/geolocation.png"
thumbnailImagePosition = "left"
coverImage = "/img/old/geolocation.png"
coverSize = "partial"
coverMeta = "out"
aliases = ["/blog/2013/10/09-ip4r",
           "/blog/2013/10/09-ip4r.html"]
+++

Let's get back to our 
[Tour of Extensions](/tags/extensions) that had to be kept aside for
awhile with other concerns such as last chance 
[PostgreSQL data recovery](/blog/2013/09/16-PostgreSQL-data-recovery). Now
that we have a 
*data loading* tool up to the task (read about it in the
[Loading Geolocation Data](/blog/2013/10/01-loading-geolocation-data) article) we're going to be able to play with the
awesome 
[ip4r](https://github.com/RhodiumToad/ip4r) extension from 
[RhodiumToad](http://blog.rhodiumtoad.org.uk/).


*The name of the game is to put IP adresses on a map*

In this article we are going to 
*geolocalize* our users given their IP
address. That can be useful when you know the 
*timezone* settings of the
different locations on earth that you have in the database, to automatically
adapt to your user's current location for example. Of course we're going to
do something more exciting than that. Read on!


## Geolocation data loading

So the first step is to find an 
*geolocation* database, and several providers
are offering that. The one I did choose for that example is the
[http://www.maxmind.com](http://www.maxmind.com) free database available at
[GeoLite Free Downloadable Databases](http://dev.maxmind.com/geoip/legacy/geolite/).

After having had a look at the files in there, we define the table schema we
want and load the archive, using the pgloader command we saw in our previous
article 
[Loading Geolocation Data](/blog/2013/10/01-loading-geolocation-data):

~~~
$ ./pgloader.exe --quiet pgloader/test/archive.load 
Now logging in '//tmp/pgloader//pgloader.log'.
Archive:  /Users/dim/Downloads/GeoLiteCity-latest.zip
  inflating: /private/var/folders/w7/9n8v8pw54t1gngfff0lj16040000gn/T/pgloader/GeoLiteCity-latest/GeoLiteCity_20130903/GeoLiteCity-Blocks.csv  
  inflating: /private/var/folders/w7/9n8v8pw54t1gngfff0lj16040000gn/T/pgloader/GeoLiteCity-latest/GeoLiteCity_20130903/GeoLiteCity-Location.csv  
                    table name       read   imported     errors       time
------------------------------  ---------  ---------  ---------  ---------
                       extract          0          0          0      0.925
                   before load          0          0          0      0.086
------------------------------  ---------  ---------  ---------  ---------
              geolite.location     438386     438386          0      8.841
                geolite.blocks    1790461    1790461          0     16.738
------------------------------  ---------  ---------  ---------  ---------
                       finally          0          0          0     32.646
------------------------------  ---------  ---------  ---------  ---------
             Total import time    2228847    2228847          0    59.236s
~~~


So we now have the following tables to play with:

~~~
~# \dt+ geolite.
\dt+ geolite.
                    List of relations
 Schema  |   Name   | Type  | Owner | Size  | Description 
---------+----------+-------+-------+-------+-------------
 geolite | blocks   | table | dim   | 76 MB | 
 geolite | location | table | dim   | 30 MB | 
(2 rows)

~# \d geolite.
    Table "geolite.blocks"
 Column  |  Type   | Modifiers 
---------+---------+-----------
 iprange | ip4r    | 
 locid   | integer | 
Indexes:
    "blocks_ip4r_idx" gist (iprange)

     Table "geolite.location"
   Column   |  Type   | Modifiers 
------------+---------+-----------
 locid      | integer | not null
 country    | text    | 
 region     | text    | 
 city       | text    | 
 postalcode | text    | 
 location   | point   | 
 metrocode  | text    | 
 areacode   | text    | 
Indexes:
    "location_pkey" PRIMARY KEY, btree (locid)
~~~



{{< image classes="fig50 center fancybox dim-margin" src="/img/old/cidr-ip.png" >}}



## Finding an IP address in our ranges

Here's what the main data look like:

~~~
~# table geolite.blocks limit 10;
        iprange        | locid 
-----------------------+-------
 1.0.0.0/24            |    17
 1.0.1.0-1.0.3.255     |    49
 1.0.4.0/23            | 14409
 1.0.6.0/23            |    17
 1.0.8.0/21            |    49
 1.0.16.0/20           | 14614
 1.0.32.0/19           | 47667
 1.0.64.0/18           |   111
 1.0.128.0-1.0.147.255 |   209
 1.0.148.0/24          | 22537
(10 rows)
~~~


What we have here is a classic 
*ip range* column where we can see that the
datatype output function is smart enough to display ranges either in their
[CIDR notation](http://en.wikipedia.org/wiki/Classless_Inter-Domain_Routing) or in the more general 
*start-end* notation when no CIDR
applies.


{{< image classes="fig50 center fancybox dim-margin" src="/img/old/PlusMinusTimesDivide.png" >}}


*The ***IP4R*** operators are not written this way*

The 
*ip4r* extension provides several operators to work with the dataset we
have, some of those operators are supported by the index we just created.
And just for the fun of it here's a catalog query to inquire about them:

~~~
ip4r# select amopopr::regoperator
        from pg_opclass c
             join pg_am am on am.oid = c.opcmethod
             join pg_amop amop on amop.amopfamily = c.opcfamily
       where opcintype = 'ip4r'::regtype and am.amname = 'gist';
    amopopr     
----------------
 >>=(ip4r,ip4r)
 <<=(ip4r,ip4r)
 >>(ip4r,ip4r)
 <<(ip4r,ip4r)
 &&(ip4r,ip4r)
 =(ip4r,ip4r)
(6 rows)
~~~


Note that we could have been using the psql 
`\dx+ ip4r` command instead of
course, but that query directly list operators that the 
*GiST* index knows how
to solve. The operator 
`>>=` reads as 
*contains* and is the one
we're going to use here.

~~~
ip4r# select * from geolite.blocks where iprange >>= '91.121.37.122';
          iprange          | locid 
---------------------------+-------
 91.121.0.0-91.121.159.255 |    75
(1 row)

Time: 1.220 ms
~~~



{{< image classes="fig50 center fancybox dim-margin" src="/img/old/geolocation-clic.png" >}}



## Geolocation meta-data

Now with the 
*MaxMind* schema that we are using in that example, the
interesting data actually is to be found in the other table, the
`geolite.localtion` one. Let's use another IP address now, I'm told that
`google.us has address 74.125.195.147`, where is that IP from:

~~~
ip4r# select *
        from geolite.blocks join geolite.location using(locid)
       where iprange >>= '74.125.195.147';
-[ RECORD 1 ]----------------------------
locid      | 2703
iprange    | 74.125.189.24-74.125.255.255
country    | US
region     | CA
city       | Mountain View
postalcode | 94043
location   | (-122.0574,37.4192)
metrocode  | 807
areacode   | 650

Time: 1.335 ms
~~~


Now you can actually draw that on a map as you have the location information
as a 
*point* datatype containing both the 
*longitude* and 
*latitude*.

You might remember that we already saw how to use the 
[earthdistance](http://www.postgresql.org/docs/9.2/static/earthdistance.html)
extension in our recent enouth article 
[How far is the nearest pub?](/blog/2013/08/05-earthdistance) Time to
try something more interesting then!


{{< image classes="fig50 center fancybox dim-margin" src="/img/old/pubstopslondon.jpg" >}}



## Emergency Pub

What if you want to make an application to help lost souls find the nearest
pub from where they are currently? Now that you know their location from the
*IP address* they are using in their browser, it should be easy enough right?

The pub names I got last time where all located in the UK, so we need an UK
based IP address now: it seems that 
`bbc.co.uk has address 212.58.251.195`.

~~~
ip4r# select *
        from geolite.location l join geolite.blocks using(locid)
       where iprange >>= '212.58.251.195';
-[ RECORD 1 ]---------------------------
locid      | 14023
country    | GB
region     | N7
city       | Tadworth
postalcode | 
location   | (-0.2333,51.2833)
metrocode  | 
areacode   | 
iprange    | 212.58.232.0-212.58.255.255
~~~


What are the ten nearest pubs around if you're just out of the BBC hosting
services? Well, let's figure that out before we get thirsty!

~~~
ip4r# with geoloc as
   (
    select location
      from geolite.location l
           join geolite.blocks using(locid)
     where iprange >>= '212.58.251.195'
   )
    select name,
           round((pos <@> (select location from geoloc))::numeric, 3) as miles
      from pubnames
  order by pos <-> (select location from geoloc)
     limit 10;
          name          | miles 
------------------------+-------
 Blue Anchor            | 0.299
 The Dukes Head         | 0.360
 The Blue Ball          | 0.337
 The Bell (aka The Rat) | 0.481
 Inn on the Green       | 0.602
 The Fox & Hounds       | 0.549
 The Chequers           | 0.712
 The Sportsman          | 1.377
 Kingswood Arms         | 1.205
 Tattenham Corner       | 2.007
(10 rows)

Time: 3.275 ms
~~~



{{< image classes="fig50 center fancybox dim-margin" src="/img/old/simple-graphical-explain.png" >}}


For this query to be executed that fast, of course we had to create the
right set of indexes. Here's the explain plan we got:

~~~
QUERY PLAN                               
------------------------------------------------------------------------
 Limit
   CTE geoloc
     ->  Nested Loop
           ->  Bitmap Heap Scan on blocks
                 Recheck Cond: (iprange >>= '212.58.251.195'::ip4r)
                 ->  Bitmap Index Scan on blocks_ip4r_idx
                       Index Cond: (iprange >>= '212.58.251.195'::ip4r)
           ->  Index Scan using location_pkey on location l
                 Index Cond: (locid = blocks.locid)
   InitPlan 2 (returns $2)
     ->  CTE Scan on geoloc
   InitPlan 3 (returns $3)
     ->  CTE Scan on geoloc geoloc_1
   ->  Index Scan using pubnames_pos_idx on pubnames
         Order By: (pos <-> $3)
(15 rows)
~~~



## Conclusion

While some 
*geolocation* data provider are giving you some libs and code to do
quite fast lookups, any interesting thing you want to do with the
*geolocation* data is about the 
***meta data***. And that's where yet again
[PostgreSQL](http://www.postgresql.org/) shines: you can actually use specialized data types and
operators, 
*JOINs* and 
*KNN* searches, all from within a single query. You get
back only those results you are interested into, and the application is then
responsible for adding value to that, rather than processing the data
itself.

Typically what the application here would be doing is drawing a map and
locating the pubs on it, adding maybe descriptions and votes and notes on
each address, maybe even the draft menu. An ideal application might even be
able to join the draft menu of each nearby pub against your own preferences
and offer you a nice short list ordered by what you're most likely to want
to drink at this hour.

Living in the future is both exciting and frightening!

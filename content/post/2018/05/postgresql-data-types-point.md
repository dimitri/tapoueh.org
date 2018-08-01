+++
title = "PostgreSQL Data Types: Point"
date = "2018-05-07T10:46:17+02:00"
tags = ["PostgreSQL","YeSQL","Data Types","Point","Geometry","Geolocation"]
categories = ["PostgreSQL","YeSQL"]
coverImage = "/img/earth.jpg"
coverMeta = "in"
coverSize = "partial"
thumbnailImage = "/img/earth-tz-logo.jpg"
thumbnailImagePosition = "left"

+++

Continuing our series of [PostgreSQL Data Types](/tags/data-types/) today
we're going to introduce the PostgreSQL Point type.

In order to put the Point datatype in a context where it makes sense, we're
going to download a complete geolocation data set and normalize it, thus
making good use of both the normalization good practice and those other
PostgreSQL data types we've been learning about in the previous articles of
this series.

Buckle-up, this is a long article with a lot of SQL inside.

<!--more-->
<!--toc-->

## Geonames

> _The [GeoNames](http://www.geonames.org) geographical database covers all
> countries and contains over eleven million place names that are available
> for download free of charge._

The website offers online querying and all the data is made available to
download and use. As is often the case, it comes in an ad-hoc format and
requires some processing and normalization before it's usable in a
PostgreSQL database.

~~~ sql
begin;

create schema if not exists raw;

create table raw.geonames
 (
   geonameid         bigint,
   name              text,
   asciiname         text,
   alternatenames    text,
   latitude          double precision,
   longitude         double precision,
   feature_class     text,
   feature_code      text,
   country_code      text,
   cc2               text,
   admin1_code       text,
   admin2_code       text,
   admin3_code       text,
   admin4_code       text,
   population        bigint,
   elevation         bigint,
   dem               bigint,
   timezone          text,
   modification      date
 );

create table raw.country
 (
  iso                 text,
  iso3                text,
  isocode             integer,
  fips                text,
  name                text,
  capital             text,
  area                double precision,
  population          bigint,
  continent           text,
  tld                 text,
  currency_code       text,
  currency_name       text,
  phone               text,
  postal_code_format  text,
  postal_code_regex   text,
  languages           text,
  geonameid           bigint,
  neighbours          text,
  fips_equiv          text
 );

\copy raw.country from 'countryInfoData.txt' with csv delimiter E'\t'

create table raw.feature
 (
  code        text,
  description text,
  comment     text
 );

\copy raw.feature from 'featureCodes_en.txt' with csv delimiter E'\t'

create table raw.admin1
 (
  code       text,
  name       text,
  ascii_name text,
  geonameid  bigint
 );

\copy raw.admin1 from 'admin1CodesASCII.txt' with csv delimiter E'\t'

create table raw.admin2
 (
  code       text,
  name       text,
  ascii_name text,
  geonameid  bigint
 );

\copy raw.admin2 from 'admin2Codes.txt' with csv delimiter E'\t'

commit;
~~~

Once we have loaded the raw data from the published files at
<http://download.geonames.org/export/dump/>, we can normalize the content
and begin to use the data.

You might notice that the SQL file above is missing the `\copy` command for
the *raw.geonames* table. That's because *copy* failed to load the file
properly: some location names include single and double quotes, and those
are not properly quoted… and not properly escaped. So we resorted to
pgloader to load the file, with the following command:

~~~
load csv
  from /tmp/geonames/allCountries.txt
  into pgsql://appdev@/appdev
  target table raw.geonames

  with fields terminated by '\t',
       fields optionally enclosed by '§',
       fields escaped by '%',
       truncate;
~~~

Here's the summary obtained when loading the dataset on the laptop used to
prepare this book:

~~~
             table name     errors       rows      bytes      total time
-----------------------  ---------  ---------  ---------  --------------
                  fetch          0          0                     0.009s
-----------------------  ---------  ---------  ---------  --------------
           raw.geonames          0   11540466     1.5 GB       6m43.218s
-----------------------  ---------  ---------  ---------  --------------
        Files Processed          0          1                     0.026s
COPY Threads Completion          0          2                  6m43.319s
-----------------------  ---------  ---------  ---------  --------------
      Total import time          ✓          3     1.5 GB       6m43.345s
~~~

## Normalizing our Geonames data model

To normalize the schema, we apply the rules from the definition of the
*normal forms*. Basically, we want to avoid any dependency in between the
attributes of our models. Any dependency means that we need to create a
separate table where to manage a set of data that makes sense in isolation
is managed.

{{< alert info >}}

I did a full talk on the topic of [Data Modeling, Normalization and
Denormalization](https://www.postgresql.eu/events/nordicpgday2018/schedule/session/1896-data-modeling-normalization-and-denormalization/)
at [pgday Nordic](https://2018.nordicpgday.org) this year in Oslo, and the
slides are of course available in the [Talks](/conf/) section here. Check-it
out!

{{< /alert >}}

The *raw.geonames* table uses several reference data that *GeoNames* provide
as separate downloads. We then need to begin with fixing the reference data
used in the model.

## Features

The *GeoNames* model tags all of its geolocation data with a *feature* class
and a feature. The description for those codes are detailed on the [GeoNames
codes](http://www.geonames.org/export/codes.html) page and available for
download in the *featureCodes_en.txt* file. Some of the information we need
is only available in a text form and has to be reported manually.

~~~ sql
begin;

create schema if not exists geoname;

create table geoname.class
 (
  class        char(1) not null primary key,
  description  text
 );

insert into geoname.class (class, description)
     values ('A', 'country, state, region,...'),
            ('H', 'stream, lake, ...'),
            ('L', 'parks,area, ...'),
            ('P', 'city, village,...'),
            ('R', 'road, railroad '),
            ('S', 'spot, building, farm'),
            ('T', 'mountain,hill,rock,... '),
            ('U', 'undersea'),
            ('V', 'forest,heath,...');

create table geoname.feature
 (
  class       char(1) not null references geoname.class(class),
  feature     text    not null,
  description text,
  comment     text,

  primary key(class, feature)
 );

insert into geoname.feature
     select substring(code from 1 for 1) as class,
            substring(code from 3) as feature,
            description,
            comment
       from raw.feature
      where feature.code <> 'null';

commit;
~~~

As we see in this file we have to deal with an explicit *'null'* entry:
there's a text that is four letters long in the last line (and reads `null`)
and that we don't want to load.

Also, the provided file uses the notation *A.ADM1* for an entry of class *A*
and feature *ADM1*, which we split into proper attributes in our
normalization process. The natural key for the *geoname.feature* table is
the combination of the *class* and the *feature*.

Once all the data is loaded and normalized, we can get some nice statistics:

~~~ sql
  select class, feature, description, count(*)
    from feature
         left join geoname using(class,feature)
group by class, feature
order by count desc
   limit 10;
~~~

This is a very simple top-10 query, per feature:

~~~ psql
 class │ feature │   description   │  count  
═══════╪═════════╪═════════════════╪═════════
 P     │ PPL     │ populated place │ 1711458
 H     │ STM     │ stream          │  300283
 S     │ CH      │ church          │  236394
 S     │ FRM     │ farm            │  234536
 S     │ SCH     │ school          │  223402
 T     │ HLL     │ hill            │  212659
 T     │ MT      │ mountain        │  192454
 S     │ HTL     │ hotel           │  170896
 H     │ LK      │ lake            │  162922
 S     │ BLDG    │ building(s)     │  143742
(10 rows)
~~~

## Countries

The *raw.country* table has several normalization issues. Before we list
them, having a look at some data will help us:

~~~ psql
─[ RECORD 1 ]──────┬─────────────────────────
iso                │ FR
iso3               │ FRA
isocode            │ 250
fips               │ FR
name               │ France
capital            │ Paris
area               │ 547030
population         │ 64768389
continent          │ EU
tld                │ .fr
currency_code      │ EUR
currency_name      │ Euro
phone              │ 33
postal_code_format │ #####
postal_code_regex  │ ^(\d{5})$
languages          │ fr-FR,frp,br,co,ca,eu,oc
geonameid          │ 3017382
neighbours         │ CH,DE,BE,LU,IT,AD,MC,ES
fips_equiv         │ ¤
~~~

The main normalization failures we see are:

  - Nothing guarantees the absence of duplicate rows in the table, so we
    need to add a *primary key* constraint.

    Here the *isocode* attribute looks like the best choice, as it's both
    unique and an integer.

  - The *languages* and *neighbours* attributes both contain multiple-valued
    content, a comma-separated list of either languages or country codes.

  - To reach *2NF* then, all non-key attributes should be dependent on the
    entire of the key, and the currencies and postal code formats are not
    dependent on the country.

A good way to check for dependencies on the key attributes is with the
following type of query:

~~~ sql
  select currency_code, currency_name, count(*)
    from raw.country
group by currency_code, currency_name
order by count desc
   limit 5;
~~~

In our dataset, we have the following result, showing 34 countries using the
Euro currency:

~~~ psql
 currency_code │ currency_name │ count 
═══════════════╪═══════════════╪═══════
 EUR           │ Euro          │    34
 USD           │ Dollar        │    16
 AUD           │ Dollar        │     8
 XOF           │ Franc         │     8
 XCD           │ Dollar        │     8
(5 rows)
~~~

In the context of this article, we're going to pass on the currency,
language, and postal code formats of countries and focus on some information
only. That gives us the following normalization process:

~~~ sql
begin;

create schema if not exists geoname;

create table geoname.continent
 (
  code    char(2) primary key,
  name    text
 );

insert into geoname.continent(code, name)
     values ('AF', 'Africa'),
            ('NA', 'North America'),
            ('OC', 'Oceania'),
            ('AN', 'Antarctica'),
            ('AS', 'Asia'),
            ('EU', 'Europe'),
            ('SA', 'South America');

create table geoname.country
 (
  isocode   integer primary key,
  iso       char(2) not null,
  iso3      char(3) not null,
  fips      text,
  name      text,
  capital   text,
  continent char(2) references geoname.continent(code),
  tld       text,
  geonameid bigint
 );

insert into geoname.country
     select isocode, iso, iso3, fips, name,
            capital, continent, tld, geonameid
       from raw.country;

create table geoname.neighbour
 (
  isocode   integer not null references geoname.country(isocode),
  neighbour integer not null references geoname.country(isocode),

  primary key(isocode, neighbour)
 );

insert into geoname.neighbour
   with n as(
     select isocode,
            regexp_split_to_table(neighbours, ',') as neighbour
       from raw.country
   )
   select n.isocode,
          country.isocode
     from n
          join geoname.country
            on country.iso = n.neighbour;

commit;
~~~

Note that we add the continent list (for completeness in the region drill
down) and then introduce the *geoname.neighbour* part of the model. Having
an association table that *links* every country with its neighbours on the
map (a neighbour has a common border) allows us to easily query for the
information:

~~~ sql
select neighbour.iso,
       neighbour.name,
       neighbour.capital,
       neighbour.tld

  from geoname.neighbour as border
       
       join geoname.country as country
         on border.isocode = country.isocode
       
       join geoname.country as neighbour
         on border.neighbour = neighbour.isocode

 where country.iso = 'FR';
~~~

So we get the following list of neighbor countries for France:

~~~ psql
 iso │    name     │     capital      │ tld 
═════╪═════════════╪══════════════════╪═════
 CH  │ Switzerland │ Bern             │ .ch
 DE  │ Germany     │ Berlin           │ .de
 BE  │ Belgium     │ Brussels         │ .be
 LU  │ Luxembourg  │ Luxembourg       │ .lu
 IT  │ Italy       │ Rome             │ .it
 AD  │ Andorra     │ Andorra la Vella │ .ad
 MC  │ Monaco      │ Monaco           │ .mc
 ES  │ Spain       │ Madrid           │ .es
(8 rows)
~~~

## Administrative Zoning

The raw data from the *GeoNames* website then offers an interesting
geographical breakdown in the *country_code*, *admin1_code* and
*admin2_code*.

~~~ sql
select geonameid, name, admin1_code, admin2_code
  from raw.geonames
 where country_code = 'FR'
 limit 5
offset 50;
~~~

To get an interesting result set, we select randomly from the data for
France, where the code has to be expanded to be meaningful. With a USA based
data set, we get states codes as *admin1_code* (e.g. *IL* for Illinois), and
the necessity for normalized data might then be less visible.

Of course, never use *offset* in your application queries, as seen on [No
Offset](https://use-the-index-luke.com/no-offset). Here, we are doing
interactive discovery of the data, so it is found acceptable, to some
extent, to play with the *offset* facility.

Here's the data set we get:

~~~ psql
 geonameid │        name         │ admin1_code │ admin2_code 
═══════════╪═════════════════════╪═════════════╪═════════════
   2967132 │ Zintzel du Nord     │ 44          │ 67
   2967133 │ Zinswiller          │ 44          │ 67
   2967134 │ Ruisseau de Zingajo │ 94          │ 2B
   2967135 │ Zincourt            │ 44          │ 88
   2967136 │ Zimming             │ 44          │ 57
(5 rows)
~~~

The *GeoNames* website provides files *admin1CodesASCII.txt* and
*admin2Codes.txt* for us to use to normalize our data. Those files again use
admin codes spelled as *AD.06* and *AF.01.1125426* where the *raw.geonames*
table uses them as separate fields. That's a good reason to split them now.

Here's the SQL to normalize the admin breakdowns, splitting the codes and
adding necessary constraints, to ensure data quality:

~~~ sql
begin;

create schema if not exists geoname;

create table geoname.region
 (
  isocode   integer not null references geoname.country(isocode),
  regcode   text not null,
  name      text,
  geonameid bigint,

  primary key(isocode, regcode)
 );

insert into geoname.region
   with admin as
   (
     select regexp_split_to_array(code, '[.]') as code,
            name,
            geonameid
       from raw.admin1
   )
   select country.isocode as isocode,
          code[2] as regcode,
          admin.name,
          admin.geonameid
     from admin
          join geoname.country
            on country.iso = code[1];

create table geoname.district
 (
  isocode   integer not null,
  regcode   text not null,
  discode   text not null,
  name      text,
  geonameid bigint,

  primary key(isocode, regcode, discode),
  foreign key(isocode, regcode)
   references geoname.region(isocode, regcode)
 );

insert into geoname.district
   with admin as
   (
     select regexp_split_to_array(code, '[.]') as code,
            name,
            geonameid
       from raw.admin2
   )
     select region.isocode,
            region.regcode,
            code[3],
            admin.name,
            admin.geonameid
       from admin
            
            join geoname.country
              on country.iso = code[1]
            
            join geoname.region
              on region.isocode = country.isocode
             and region.regcode = code[2];

commit;
~~~

The previous query can now be rewritten, showing region and *district* names
rather than *admin1_code* and *admin2_code*, which we still have internally
in case we need them of course.

~~~ sql
select r.name, reg.name as region, d.name as district
  from raw.geonames r
       
       left join geoname.country
              on country.iso = r.country_code
       
       left join geoname.region reg
              on reg.isocode = country.isocode
             and reg.regcode = r.admin1_code
       
       left join geoname.district d
              on d.isocode = country.isocode
             and d.regcode = r.admin1_code
             and d.discode = r.admin2_code
 where country_code = 'FR'
 limit 5
offset 50;
~~~

The query uses *left join* operations because we have geo-location data
without the *admin1* or *admin2* levels of details — more on that later.
Here's the same list of French areas, this time with proper names:

~~~ psql
        name         │  region   │           district            
═════════════════════╪═══════════╪═══════════════════════════════
 Zintzel du Nord     │ Grand Est │ Département du Bas-Rhin
 Zinswiller          │ Grand Est │ Département du Bas-Rhin
 Ruisseau de Zingajo │ Corsica   │ Département de la Haute-Corse
 Zincourt            │ Grand Est │ Département des Vosges
 Zimming             │ Grand Est │ Département de la Moselle
(5 rows)
~~~

## Geolocation Data

Now that we have loaded the reference data, we can load the main geolocation
data with the following script. Note that we skip parts of the data we don't
need for this book, but that you might want to load in your application's
background data.

Before loading the raw data into a normalized version of the table, which
will make heavy use of the references we normalized before, we have to study
and understand how the breakdown works:

~~~ sql
select count(*) as all,
       count(*) filter(where country_code is null) as no_country,
       count(*) filter(where admin1_code is null) as no_region,
       count(*) filter(where admin2_code is null) as no_district,
       count(*) filter(where feature_class is null) as no_class,
       count(*) filter(where feature_code is null) as no_feat
  from raw.geonames ;
~~~

We have lots of entries without reference for a *country*, and even more
without detailed breakdown (*admin1_code* and *admin2_code* are not always
part of the data). Moreover we also have points without any reference
feature and class, some of them in the Artic.

~~~ psql
   all    │ no_country │ no_region │ no_district │ no_class │ no_feat 
══════════╪════════════╪═══════════╪═════════════╪══════════╪═════════
 11540466 │       5821 │     45819 │     5528455 │     5074 │   95368
(1 row)
~~~

Given that, our normalization query must be careful to use *left join*
operations, so as to allow for fields to be *null* when the foreign key
reference doesn't exist. Be careful to drill down properly to the country,
then the region, and only then the district, as the data set contains points
of several layers of precision as seen in the query above.

~~~ sql
begin;

create table geoname.geoname
 (
   geonameid         bigint primary key,
   name              text,
   location          point,
   isocode           integer,
   regcode           text,
   discode           text,
   class             char(1),
   feature           text,
   population        bigint,
   elevation         bigint,
   timezone          text,

   foreign key(isocode)
    references geoname.country(isocode),
   
   foreign key(isocode, regcode)
    references geoname.region(isocode, regcode),
    
   foreign key(isocode, regcode, discode)
    references geoname.district(isocode, regcode, discode),

   foreign key(class)
    references geoname.class(class),

   foreign key(class, feature)
    references geoname.feature(class, feature)
 );

insert into geoname.geoname
  with geo as
  (
     select geonameid,
            name,
            point(longitude, latitude) as location,
            country_code,
            admin1_code,
            admin2_code,
            feature_class,
            feature_code,
            population,
            elevation,
            timezone
       from raw.geonames
   )
     select geo.geonameid,
            geo.name,
            geo.location,
            country.isocode,
            region.regcode,
            district.discode,
            feature.class,
            feature.feature,
            population,
            elevation,
            timezone
       from geo
            left join geoname.country
              on country.iso = geo.country_code

            left join geoname.region
              on region.isocode = country.isocode
             and region.regcode = geo.admin1_code

            left join geoname.district
              on district.isocode = country.isocode
             and district.regcode = geo.admin1_code
             and district.discode = geo.admin2_code

           left join geoname.feature
             on feature.class = geo.feature_class
            and feature.feature = geo.feature_code;

create index on geoname.geoname using gist(location);

commit;
~~~

Now that we have a proper data set loaded, it's easier to make sense of the
administrative breakdowns and the geo-location data.

The real use case for this data comes later: thanks to the *GiST* index over
the *geoname.location* column we are now fully equipped to do a names lookup
from the geo-localized information.

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

## Geolocation GiST Indexing

The previous *geoname* table creation script contains the following index
definition:

~~~ sql
create index on geoname.geoname using gist(location);
~~~

Such an index is useful when searching for a specific location within our
table, which contains about 11.5 million entries. PostgreSQL supports *index
scan* based lookups in several situations, including the *kNN* lookup, also
known as the *nearest neighbor* lookup.

In the [arrays](/blog/2018/04/postgresql-data-types-arrays/) non-relational
data type example we loaded a data set of 200,000 geo-localized tweets in
the *hashtag* table. Here's an extract of this table's content:

~~~ psql
─[ RECORD 1 ]────────────────────────────────────────────────
id       │ 720553458596757504
date     │ 2016-04-14 10:05:00+02
uname    │ Police Calls 32801
message  │ #DrugViolation at 335 N Magnolia Ave. #orlpol #opd
location │ (-81.3769794,28.5469591)
hashtags │ {#DrugViolation,#opd,#orlpol}
~~~

It's possible to retrieve more information from the *GeoNames* data thanks
to the following *lateral left join* lookup in which we implement a *kNN*
search with `order by ... <-> ... limit k` clause:

~~~ sql
  select id,
         round((hashtag.location <-> geoname.location)::numeric, 3) as dist,
         country.iso,
         region.name as region,
         district.name as district
    from hashtag
         left join lateral
         (
            select geonameid, isocode, regcode, discode, location
              from geoname.geoname
          order by location <-> hashtag.location
             limit 1
         )
         as geoname
         on true
         left join geoname.country using(isocode)
         left join geoname.region using(isocode, regcode)
         left join geoname.district using(isocode, regcode, discode)
order by id
   limit 5;
~~~

The `<->` operator computes the distance in between its argument, and by
using the *limit 1* clause we select the nearest known entry in the
*geoname.geoname* table for each entry in the *hashtag* table.

Then it's easy to add our normalized *GeoNames* information from the
*country*, *region* and *district* tables. Here's the result we get here:

~~~ psql
         id         │ dist  │ iso │    region    │      district       
════════════════════╪═══════╪═════╪══════════════╪═════════════════════
 720553447402160128 │ 0.004 │ US  │ Florida      │ Orange County
 720553457015324672 │ 0.004 │ US  │ Texas        │ Smith County
 720553458596757504 │ 0.001 │ US  │ Florida      │ Orange County
 720553466804989952 │ 0.001 │ US  │ Pennsylvania │ Philadelphia County
 720553475923271680 │ 0.000 │ US  │ New York     │ Nassau County
(5 rows)
~~~

To check that our *GiST* index is actually used, we use the *explain*
command of PostgreSQL, with the spelling `explain (costs off)` followed by
the whole query as above, and we get the following query plan:

~~~
\pset format wrapped
\pset columns 70

                              QUERY PLAN                              
══════════════════════════════════════════════════════════════════════
 Limit
   ->  Nested Loop Left Join
         ->  Nested Loop Left Join
               ->  Nested Loop Left Join
                     Join Filter: (geoname.isocode = country.isocode)
                     ->  Nested Loop Left Join
                           ->  Index Scan using hashtag_pkey on hasht…
…ag
                           ->  Limit
                                 ->  Index Scan using geoname_locatio…
…n_idx on geoname
                                       Order By: (location <-> hashta…
…g.location)
                     ->  Materialize
                           ->  Seq Scan on country
               ->  Index Scan using region_pkey on region
                     Index Cond: ((geoname.isocode = isocode) AND (ge…
…oname.regcode = regcode))
         ->  Index Scan using district_pkey on district
               Index Cond: ((geoname.isocode = isocode) AND (geoname.…
…regcode = regcode) AND (geoname.discode = discode))
(16 rows)
~~~

The *index scan using geoname_location_idx on geoname* is clear: the index
has been used. On the laptop on which this book has been written, we get the
result in about 13 milliseconds.

## A Sampling of Countries

This dataset of more than 11 million rows is not practical to include in the
book's material for the *Full Edition* and *Enterprise Edition*, where you
have a database dump or Docker image to play with. We instead take a random
sample of 1% of the table's content, and here's how the magic is done:

~~~ sql
begin;

create schema if not exists sample;

drop table if exists sample.geonames;

create table sample.geonames
   as select /*
              * We restrict the “export” to some columns only, so as to
              * further reduce the size of the exported file available to
              * download with the book.
              */
             geonameid,
             name,
             longitude,
             latitude,
             feature_class,
             feature_code,
             country_code,
             admin1_code,
             admin2_code,
             population,
             elevation,
             timezone
             /*
              * We only keep 1% of the 11 millions rows here.
              */
        from raw.geonames TABLESAMPLE bernoulli(1);

\copy sample.geonames to 'allCountries.sample.copy'

commit;
~~~

In this script, we use the *tablesample* feature of PostgreSQL to only keep
a random selection of 1% of the rows in the table. The *tablesample* accepts
several methods, and you can see the PostgreSQL documentation entitled
[Writing A Table Sampling
Method](https://www.postgresql.org/docs/current/static/tablesample-method.html)
yourself if you need to.

Here's what the [from
clause](https://www.postgresql.org/docs/current/static/sql-select.html#SQL-FROM)
documentation of the *select* statement has to say about the choice of
*bernouilli* and *system*, included by default in PostgreSQL:

> The BERNOULLI and SYSTEM sampling methods each accept a single argument
> which is the fraction of the table to sample, expressed as a percentage
> between 0 and 100. This argument can be any real-valued expression. (Other
> sampling methods might accept more or different arguments.) These two
> methods each return a randomly-chosen sample of the table that will
> contain approximately the specified percentage of the table's rows. The
> BERNOULLI method scans the whole table and selects or ignores individual
> rows independently with the specified probability. The SYSTEM method does
> block-level sampling with each block having the specified chance of being
> selected; all rows in each selected block are returned. The SYSTEM method
> is significantly faster than the BERNOULLI method when small sampling
> percentages are specified, but it may return a less-random sample of the
> table as a result of clustering effects.

Running the script, here's what we get:

~~~
yesql# \i geonames.sample.sql
BEGIN
CREATE SCHEMA
DROP TABLE
SELECT 115904
COPY 115904
COMMIT
~~~

Our *sample.geonames* table only contains 115,904 rows. Another run of the
same query yielded 115,071 instead. After all the sampling is made following
a random-based algorithm.

## Conclusion

When dealing with geolocation data, it's possible to put the PostgreSQL data
type POINT to good use. The PostgreSQL support for GiST indexes makes it
easy to then query the data, including for kNN searches and in complex
queries.

While it's possible to just use a flat table with a point column, it might
be best to normalize your geolocation database schema. This helps with data
quality!

{{< figure class="right"
             src="/img/MasteringPostgreSQLinAppDev-Cover-th.png"
            link="https://masteringpostgresql.com" >}}
            
This article is an extract from my book [Mastering PostgreSQL in Application
Development](https://masteringpostgresql.com), which teaches SQL to
developers so that they may replace thousands of lines of code with very
simple queries. The book has a full chapter about data types in PostgreSQL,
check it out!


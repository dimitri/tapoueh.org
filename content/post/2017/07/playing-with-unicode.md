+++
title = "Playing with Unicode"
date = "2017-07-03T14:32:29+02:00"
tags = ["PostgreSQL","YeSQL","SQL","unicode", "flags"]
categories = ["PostgreSQL","YeSQL"]
coverImage = "/img/Taamey-Ashkenaz-CLM.png"
coverMeta = "out"
coverSize = "partial"
thumbnailImage = "/img/Unicode_logo.gif"
thumbnailImagePosition = "left"

+++

The reason why I like Unicode a lot is because it allows me to code in text
based environments and still have nice output. Today, we're going to play
with
[Regional Indicator Symbol](https://en.wikipedia.org/wiki/Regional_Indicator_Symbol),
which is implemented as a Unicode combinaison of letters from 🇦 to 🇿. For
instance, if you display 🇫 then 🇷 concatenated together, you get 🇫🇷. Let's
try that from our [PostgreSQL](https://www.postgresql.org/) prompt!

<!--more-->
<!--toc-->

# Context and Necessary data

In the [historical record of motor racing data](http://ergast.com/mrd/)
database we have all the data to reproduce the table from
the
[List of Formula One seasons](https://en.wikipedia.org/wiki/List_of_Formula_One_seasons) with
driver's champions and constructors champion.

{{< alert success >}}

Grab the MySQL version of it, the only one available now, and import its
schema and data in a single command line
with [pgloader](http://pgloader.io):

~~~ bash
$ createdb f1db
$ pgloader mysql://root@localhost/f1db pgsql:///f1db
$ psql -d f1db -c 'ALTER DATABASE f1db SET search_path TO f1db, public;'
~~~

{{< /alert >}}

In
the
[List of Formula One seasons](https://en.wikipedia.org/wiki/List_of_Formula_One_seasons) page
is included a flag from the country of each driver. The f1db database model
only includes a nationality column for each driver, that we can see here:

~~~ sql
  select count(*) as victories,
         forename, surname, nationality
    from drivers
         left join results
                on drivers.driverid = results.driverid
               and results.position = 1
group by drivers.driverid
order by victories desc
   limit 10;
~~~

Which gives the following result:

~~~ psql
 victories | forename  |  surname   | nationality 
-----------+-----------+------------+-------------
        91 | Michael   | Schumacher | German
        56 | Lewis     | Hamilton   | British
        51 | Alain     | Prost      | French
        45 | Sebastian | Vettel     | German
        41 | Ayrton    | Senna      | Brazilian
        32 | Fernando  | Alonso     | Spanish
        31 | Nigel     | Mansell    | British
        27 | Jackie    | Stewart    | British
        25 | Jim       | Clark      | British
        25 | Niki      | Lauda      | Austrian
(10 rows)
~~~

Can we display directly the champion's country flag in the result set?

# The unicode trick

If we apply what we read before in
the
[Regional Indicator Symbol](https://en.wikipedia.org/wiki/Regional_Indicator_Symbol) page,
displaying a Unicode country flag is as easy as concatenating the country
codes together in the right Unicode range. Of course PostgreSQL knows how to
that, thanks to its advanced set
of
[String Functions and Operators](https://www.postgresql.org/docs/current/static/functions-string.html):

~~~ sql
select chr(127462+ascii('F')-ascii('A')) as a,
       chr(127462+ascii('R')-ascii('A')) as b,
          chr(127462+ascii('F')-ascii('A'))
       || chr(127462+ascii('R')-ascii('A')) as flag;
~~~

And yes that's a good reading of the Unicode specifications for *Regional
Indicator Symbol*, it seems:

~~~
 a | b | flag 
---+---+------
 🇫 | 🇷 | 🇫🇷
(1 row)
~~~

As it is quite cumbersome to write this expression each time, we can code a
PostgreSQL function that would do that for us:

~~~ sql
create or replace function flag
 (
   code text
 )
 returns text
language sql
as $$
 select    chr(  127462
               + ascii(substring(code from 1 for 1))
               - ascii('A'))

        || chr(  127462
               + ascii(substring(code from 2 for 1))
               - ascii('A'))
$$;
~~~

With that it's easier:

~~~ sql
select name, code, flag(code)
  from country
 where code is not null
 limit 10;
~~~

And here's a nice list of flags now:

~~~
       name        | code | flag 
-------------------+------+------
 Afghanistan       | AF   | 🇦🇫
 Albania           | AL   | 🇦🇱
 Algeria           | DZ   | 🇩🇿
 American Samoa    | AS   | 🇦🇸
 Andorra           | AD   | 🇦🇩
 Angola            | AO   | 🇦🇴
 Anguilla          | AI   | 🇦🇮
 Antarctica        | AQ   | 🇦🇶
 Antigua & Barbuda | AG   | 🇦🇬
 Argentina         | AR   | 🇦🇷
(10 rows)
~~~

# Country Codes

This *country* table comes
from <http://data.okfn.org/data/core/country-codes>. They have a GitHub
repository at <https://github.com/datasets/country-codes> from which you can
easily grab the CSV data file then integrate it into PostgreSQL:

~~~ sql
begin;

create table public.country
 (
   name                             text,
   official_name_en                 text,
   official_name_fr                 text,
   code                             text,
   trigram                          text,
   M49                              text,
   ITU                              text,
   MARC                             text,
   WMO                              text,
   DS                               text,
   Dial                             text,
   FIFA                             text,
   FIPS                             text,
   GAUL                             text,
   IOC                              text,
   currency_alphabetic_code         text,
   currency_country_name            text,
   currency_minor_unit              text,
   currency_name                    text,
   currency_numeric_code            text,
   is_independent                   text,
   Capital                          text,
   Continent                        text,
   TLD                              text,
   Languages                        text,
   Geoname                          text,
   EDGAR                            text
 );

\copy public.country from 'country-codes.csv' with delimiter ',' csv header quote '"'

commit;
~~~

# Nationality and Country Code Mapping

We said before that our Formula One database only has a *nationality* column
for our drivers. How can we map that into country codes, so that we can
diplay our nice little Unicode flag?

Well this time I failed to find a ready to use dataset, but I found an easy
to scrap web page with the needed information
at <http://www.ef.com/english-resources/english-grammar/nationalities/>.

Some lines of python later:

~~~ python
#! /usr/bin/env python3

import re

def parse_nationalities(filename):
    regexp = '\t\t\t\t(.*)</td>$'

    # Given that regexp we want to keep the first match as the country name
    # then the second match as the nationality, then skip the third line
    current = None
    skip = False

    with open(filename, 'r') as f:
        for line in f:
            match = re.search(regexp, line)
            if match:
                if skip:
                    skip = False
                elif current is None:
                    current = match.group(1)
                else:
                    print("%s;%s" % (current, match.group(1)))
                    current = None
                    skip = True

if __name__ == "__main__":
    parse_nationalities('nationalities.html')
~~~

It is now possible to load the data set thanks to the following SQL:

~~~ sql
begin;

create table public.nationalities
 (
   country     text,
   nationality text
 );


\copy public.nationalities from 'nationalities.csv' with delimiter ';' csv

insert into public.nationalities(nationality, country)
     values ('American', 'US'),
            ('American-Italian', 'US'),
            ('Argentine-Italian', 'Argentina'),
            ('Chilean', 'Chile'),
            ('East German', 'Germany'),
            ('Liechtensteiner', 'Liechtenstein'),
            ('Monegasque', 'Monaco'),
            ('New Zealander', 'New Zealand'),
            ('Rhodesian', 'Zimbabwe'),
            ('Venezuelan', 'Venezuela');

update public.nationalities
   set country = 'Netherlands'
 where country = 'Holland';

update public.nationalities
   set country = 'US'
 where country = 'The United States';

update public.nationalities
   set country = 'UK'
 where country = 'Britain';

update public.nationalities
   set country = 'Czech Republic'
 where country = 'the Czech Republic';

commit;
~~~

Note that to have matches for all our drivers, we did need to edit the list
provided so that it would match with the set of nationalities found in the
f1db database. You can list those with the following query:

~~~ sql
select distinct(nationality) from drivers;
~~~

And more to the point, you can find any missing nationality in your new
table set with the following *anti-join* query:

~~~ sql
select nationality, country.code
  from drivers
       left join nationalities using(nationality)
       left join country
              on country.name = nationalities.country
 where country.code is null;
~~~

Thanks to the modification we did previously after importing the dataset,
this query should return an empty result set.

# Formula One drivers and country flags

We now have enough data to finally play our unicode trick and display
driver's country flags directly in our query result set on the console:

~~~ sql
  select count(*) as victories,
         forename, surname,
         flag(country.code)
    from drivers
         left join results
                on drivers.driverid = results.driverid
               and results.position = 1
         join nationalities using(nationality)
         join country
           on country.name = nationalities.country
group by drivers.driverid, country.code
order by victories desc
   limit 10;
~~~

And here's our result with inline colored flags in the terminal:

~~~
 victories | forename  |  surname   | flag 
-----------+-----------+------------+------
        91 | Michael   | Schumacher | 🇩🇪
        56 | Lewis     | Hamilton   | 🇬🇧
        51 | Alain     | Prost      | 🇫🇷
        45 | Sebastian | Vettel     | 🇩🇪
        41 | Ayrton    | Senna      | 🇧🇷
        32 | Fernando  | Alonso     | 🇪🇸
        31 | Nigel     | Mansell    | 🇬🇧
        27 | Jackie    | Stewart    | 🇬🇧
        25 | Jim       | Clark      | 🇬🇧
        25 | Niki      | Lauda      | 🇦🇹
(10 rows)
~~~

Fun, right?

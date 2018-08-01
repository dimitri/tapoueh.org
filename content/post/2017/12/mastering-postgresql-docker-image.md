+++
title = "Mastering PostgreSQL: more about the docker image"
date = "2017-12-12T23:27:55+01:00"
tags = ["PostgreSQL","YeSQL","Mastering PostgreSQL","book","docker"]
categories = ["PostgreSQL","YeSQL"]
coverImage = "/img/container-ship.jpg"
coverMeta = "in"
coverSize = "partial"
thumbnailImage = "/img/docker-logo.svg"
thumbnailImagePosition = "left"

+++

The Enterprise Edition of [Mastering PostgreSQL in Application
Development](https://masteringpostgresql.com) ships with a docker image that
hosts both a PostgreSQL server instance with a pre-loaded database, the one
that's used throughout the book examples, and also with a Jupyter Network
notebook that hosts SQL queries thanks to the
[sql_magic](https://github.com/pivotal/sql_magic) plugin.

<!--more-->

{{< figure class="right"
             src="/img/MasteringPostgreSQLinAppDev-Cover-th.png"
            link="http://masteringpostgresql.com" >}}
           
This article is intended to users of the Enterprise Edition who wants to run
the *psql* application from within the docker container. When using a
Windows system for example, it might be easier to do that when compared to
having a local *psql* environment that connects to the PostgreSQL instance
in the docker container.

<!--toc-->

## Running the Docker Container

On the ubuntu machine where I start the docker image, I need to be using
*sudo* all the time. First, we start the docker image with the command given
in the README.ENTERPRISE.md with the following command:

~~~ bash
$ docker run --rm -it -p 8080:8080 dimitri:appdev
~~~

## Additionnal Commands in the Running Container

Then it's possible to also run the *psql* from within the container thanks
to the *docker exec* command. So keep open the shell window where the
container is running and serving the Jupiter Network note books, and from
another one, type in those commands.

First, we need to get the docker name of the running instance, with the
following command:

~~~ bash
$ sudo docker ps --filter ancestor=dimitri:appdev --format '{{.Names}}'
berserk_fermi
~~~

Now that we have the name, here *berserk_fermi*, we can use that name in the
following *docker exec* command. The name will be different at every launch
of the container, and from a machine to another one, so please replace it
with your local and current value.

Here's how to run our *psql* binary that ships within the docker container:

~~~ bash
$ sudo docker exec -it berserk_fermi             \
    psql -d postgresql://appdev@localhost/appdev \
         -c '\dt chinook.'
~~~

And we get the following result:

~~~
            List of relations
 Schema  |     Name      | Type  | Owner  
---------+---------------+-------+--------
 chinook | album         | table | appdev
 chinook | artist        | table | appdev
 chinook | customer      | table | appdev
 chinook | employee      | table | appdev
 chinook | genre         | table | appdev
 chinook | invoice       | table | appdev
 chinook | invoiceline   | table | appdev
 chinook | mediatype     | table | appdev
 chinook | playlist      | table | appdev
 chinook | playlisttrack | table | appdev
 chinook | track         | table | appdev
(11 rows)
~~~

It's also possible to have an interactive shell of course:

~~~ bash
$ sudo docker exec -it berserk_fermi psql -d postgresql://appdev@localhost/appdev
psql (9.6.4)
SSL connection (protocol: TLSv1.2, cipher: ECDHE-RSA-AES256-GCM-SHA384,
bits: 256, compression: off)
Type "help" for help.

appdev=> \q
~~~

## Using the SQL files from the Book

A more complex example, using the SQL files from outside the docker image
and setting a value to the variable start:

~~~ bash
$ sudo docker exec -i berserk_fermi              \
    psql --variable=start='2017-07-01'           \
         -d postgresql://appdev@localhost/appdev \
       < sql/02-intro/02-usecase/05_01.sql
    date    | day |     dollars      | WoW %  
------------+-----+------------------+--------
 2017-07-01 | Sat | $              0 |       
 2017-07-02 | Sun | $              0 |       
 2017-07-03 | Mon | $ 24,095,490,727 | -51.99
 2017-07-04 | Tue | $              0 |       
 2017-07-05 | Wed | $ 41,608,777,952 |   4.81
 2017-07-06 | Thu | $ 41,275,073,939 |  -8.49
 2017-07-07 | Fri | $ 33,808,366,512 | -33.81
 2017-07-08 | Sat | $              0 |       
 2017-07-09 | Sun | $              0 |       
 2017-07-10 | Mon | $ 36,854,420,581 |  34.62
 2017-07-11 | Tue | $ 35,948,998,801 |       
 2017-07-12 | Wed | $ 37,077,542,524 | -12.22
 2017-07-13 | Thu | $ 35,306,179,810 | -16.91
 2017-07-14 | Fri | $ 32,588,362,765 |  -3.74
 2017-07-15 | Sat | $              0 |       
 2017-07-16 | Sun | $              0 |       
 2017-07-17 | Mon | $ 33,760,987,656 |  -9.16
 2017-07-18 | Tue | $ 35,165,369,201 |  -2.23
 2017-07-19 | Wed | $ 35,297,166,209 |  -5.04
 2017-07-20 | Thu | $ 39,376,330,787 |  10.34
 2017-07-21 | Fri | $ 42,470,589,106 |  23.27
 2017-07-22 | Sat | $              0 |       
 2017-07-23 | Sun | $              0 |       
 2017-07-24 | Mon | $ 42,701,120,315 |  20.94
 2017-07-25 | Tue | $ 54,039,073,558 |  34.93
 2017-07-26 | Wed | $ 41,899,255,292 |  15.76
 2017-07-27 | Thu | $ 47,487,803,967 |  17.08
 2017-07-28 | Fri | $ 38,751,697,648 |  -9.60
 2017-07-29 | Sat | $              0 |       
 2017-07-30 | Sun | $              0 |       
 2017-07-31 | Mon | $ 49,022,828,254 |  12.90
(31 rows)
~~~

Ok it works, but it's not nice to use. It would be best to copy the SQL
files over to the docker container, so that we have them locally available
from within *psql*. Here's how to do that. From the directory in the
*MasteringPostgreSQLinAppDev* where you have the `sql/` directory, type:

~~~
$ sudo docker cp sql/  berserk_fermi:/opt/src/sql/
~~~

Where again *berserk_fermi* is the name of the running container we obtained
with *docker ps* earlier. Now we have the files in `/opt/src/sql` in the
container, so it's possible to be more interactive:

~~~
$ sudo docker exec -ti berserk_fermi psql -d postgresql://appdev@localhost/appdev
psql (9.6.4)
SSL connection (protocol: TLSv1.2, cipher: ECDHE-RSA-AES256-GCM-SHA384,
bits: 256, compression: off)
Type "help" for help.

appdev=> \set start '2017-07-01'
appdev=> \i 02-intro/02-usecase/05_01.sql 
~~~

And you should have the same result as in the previous example.

<hr />

## Conclusion

I hope you will enjoy reading [Mastering PostgreSQL in Application
Development](https://masteringpostgresql.com) as much as I did writing it!

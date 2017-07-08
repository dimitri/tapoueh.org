+++
title = "Exploring a Data Set in SQL"
date = "2017-06-13T13:47:08+02:00"
tags = ["PostgreSQL","YeSQL","SQL","exploration","window functions",
        "data","statistics","percentile","median","histogram"]
categories = ["PostgreSQL","YeSQL"]
coverImage = "/img/Plitvice-Cave-1.jpg"
coverMeta = "in"
coverSize = "partial"
thumbnailImage = "/img/explore-data.png"
thumbnailImagePosition = "left"

+++

Sometimes you need to dive in an existing data set that you know very little
about. Let's say we've been lucky to have had a high level description of
the business case covered by a database, and then access to it. Our next
step is figuring out data organisation, content and quality. Our tool box:
*the world's most advanced open source
database*, [PostgreSQL](https://www.postgresql.org), and its *Structured
Query Language*, SQL.

<!--more-->

In this article we are going to explore the music catalog parts of
the [Chinook](https://github.com/lerocha/chinook-database) sample database,
so that you can easily reproduce our queries locally should you want to.

Being the author of [pgloader](http://pgloader.io) I use
the
[Chinook SQLite](https://github.com/lerocha/chinook-database/blob/master/ChinookDatabase/DataSources/Chinook_Sqlite_AutoIncrementPKs.sqlite) distribution
and then load it with pgloader. It's a good test case for its migration
capabilities. Nowadays a PostgreSQL specific distribution is available tho.

<!--toc-->

# Artists, Albums, Tracks, Genre

The Chinook database model contains a music collection of tracks, artists,
albums and genre. Each track has a *genreid* which is a *foreign key* to the
*genre* reference table, and also an *albumid* which is a *foreing key* to
the album table. Then album has a *foreign key* to artist. To discover that
we use the `\d` facility of *psql*, here's an example for the track table:

~~~ psql
chinook# \d track
                               Table "public.track"
    Column    |  Type   |                        Modifiers                        
--------------+---------+---------------------------------------------------------
 trackid      | bigint  | not null default nextval('track_trackid_seq'::regclass)
 name         | text    | 
 albumid      | bigint  | 
 mediatypeid  | bigint  | 
 genreid      | bigint  | 
 composer     | text    | 
 milliseconds | bigint  | 
 bytes        | bigint  | 
 unitprice    | numeric | 
Indexes:
    "idx_51519_ipk_track" PRIMARY KEY, btree (trackid)
    "idx_51519_ifk_trackalbumid" btree (albumid)
    "idx_51519_ifk_trackgenreid" btree (genreid)
    "idx_51519_ifk_trackmediatypeid" btree (mediatypeid)
Foreign-key constraints:
    "track_albumid_fkey" FOREIGN KEY (albumid) REFERENCES album(albumid)
    "track_genreid_fkey" FOREIGN KEY (genreid) REFERENCES genre(genreid)
    "track_mediatypeid_fkey" FOREIGN KEY (mediatypeid) REFERENCES mediatype(mediatypeid)
Referenced by:
    TABLE "invoiceline" CONSTRAINT "invoiceline_trackid_fkey" FOREIGN KEY (trackid) REFERENCES track(trackid)
    TABLE "playlisttrack" CONSTRAINT "playlisttrack_trackid_fkey" FOREIGN KEY (trackid) REFERENCES track(trackid)
~~~

I could offer a diagram of the relations in between those tables but instead
let's write our first query against this model, showing some details about a
single album:

~~~ sql
> \set albumid 193
> select artist.name as artist,
         album.title as album,
         track.name as track
    from track
         join album using(albumid)
         join artist using(artistid)
   where albumid = :albumid;
~~~

{{< alert info >}}

Note: we saw the `\set` trick in our previous
entry [How to Write SQL](/blog/2017/06/how-to-write-sql/).

{{< /alert >}}

We see that the model has been done with *surrogate keys*: the primary keys
are all derived from a sequence. Also, those key names embed the table
names, which makes them unique in the whole schema, allowing us to use the
`JOIN ... USING(...)` syntax. This syntax has a nice side effect: in the
query output we find the joining column only once.

~~~ psql
        artist         |         album         |            track             
-----------------------+-----------------------+------------------------------
 Red Hot Chili Peppers | Blood Sugar Sex Magik | The Power Of Equality
 Red Hot Chili Peppers | Blood Sugar Sex Magik | If You Have To Ask
 Red Hot Chili Peppers | Blood Sugar Sex Magik | Breaking The Girl
 Red Hot Chili Peppers | Blood Sugar Sex Magik | Funky Monks
 Red Hot Chili Peppers | Blood Sugar Sex Magik | Suck My Kiss
 Red Hot Chili Peppers | Blood Sugar Sex Magik | I Could Have Lied
 Red Hot Chili Peppers | Blood Sugar Sex Magik | Mellowship Slinky In B Major
 Red Hot Chili Peppers | Blood Sugar Sex Magik | The Righteous & The Wicked
 Red Hot Chili Peppers | Blood Sugar Sex Magik | Give It Away
 Red Hot Chili Peppers | Blood Sugar Sex Magik | Blood Sugar Sex Magik
 Red Hot Chili Peppers | Blood Sugar Sex Magik | Under The Bridge
 Red Hot Chili Peppers | Blood Sugar Sex Magik | Naked In The Rain
 Red Hot Chili Peppers | Blood Sugar Sex Magik | Apache Rose Peacock
 Red Hot Chili Peppers | Blood Sugar Sex Magik | The Greeting Song
 Red Hot Chili Peppers | Blood Sugar Sex Magik | My Lovely Man
 Red Hot Chili Peppers | Blood Sugar Sex Magik | Sir Psycho Sexy
 Red Hot Chili Peppers | Blood Sugar Sex Magik | They're Red Hot
(17 rows)
~~~

In the *track* table we saw some more interesting columns, such as
*milliseconds* and *bytes*:

~~~ sql
select name,
       milliseconds * interval '1 millisecond' as duration,
       pg_size_pretty(bytes) as bytes
  from track
 where albumid = :albumid;
~~~

Which gives the following view of the album of choice here:

~~~ psql
             name             |       duration       |  bytes  
------------------------------+----------------------+---------
 The Power Of Equality        | @ 4 mins 3.591 secs  | 7957 kB
 If You Have To Ask           | @ 3 mins 36.79 secs  | 7030 kB
 Breaking The Girl            | @ 4 mins 55.497 secs | 9576 kB
 Funky Monks                  | @ 5 mins 23.395 secs | 10 MB
 Suck My Kiss                 | @ 3 mins 37.234 secs | 6962 kB
 I Could Have Lied            | @ 4 mins 4.506 secs  | 7899 kB
 Mellowship Slinky In B Major | @ 4 mins 0.091 secs  | 7785 kB
 The Righteous & The Wicked   | @ 4 mins 8.084 secs  | 7943 kB
 Give It Away                 | @ 4 mins 43.01 secs  | 9091 kB
 Blood Sugar Sex Magik        | @ 4 mins 31.229 secs | 8731 kB
 Under The Bridge             | @ 4 mins 24.359 secs | 8479 kB
 Naked In The Rain            | @ 4 mins 25.717 secs | 8520 kB
 Apache Rose Peacock          | @ 4 mins 42.226 secs | 9094 kB
 The Greeting Song            | @ 3 mins 13.593 secs | 6198 kB
 My Lovely Man                | @ 4 mins 39.118 secs | 9004 kB
 Sir Psycho Sexy              | @ 8 mins 16.692 secs | 16 MB
 They're Red Hot              | @ 1 min 11.941 secs  | 2326 kB
(17 rows)
~~~

If you're interested into compression at the album level, you might want to
compare *rolling sum* of both the album duration and its size in bytes,
track after track. The following query uses *window functions* to track
that:

~~~ sql
  select name,
         round(  sum(bytes::numeric) over(order by trackid)
               / sum(milliseconds) over(order by trackid), 2)
           as bps,
  
         pg_size_pretty(
            sum(bytes) over(order by trackid)
         ) as album_bytes,
         
         sum(milliseconds) over(order by trackid)
            * interval '1 millisecond' as progres

    from track
   where albumid = :albumid
order by trackid;
~~~

And here's our *rolling aggregate* result where we can see the *bps* column
for *bytes per second*, computed after the accumulated sum of milliseconds
and bytes throughout the album's track data:

~~~ psql
             name             |  bps  | album_bytes |           progres            
------------------------------+-------+-------------+------------------------------
 The Power Of Equality        | 33.45 | 7957 kB     | @ 4 mins 3.591 secs
 If You Have To Ask           | 33.34 | 15 MB       | @ 7 mins 40.381 secs
 Breaking The Girl            | 33.28 | 24 MB       | @ 12 mins 35.878 secs
 Funky Monks                  | 33.23 | 34 MB       | @ 17 mins 59.273 secs
 Suck My Kiss                 | 33.16 | 41 MB       | @ 21 mins 36.507 secs
 I Could Have Lied            | 33.15 | 49 MB       | @ 25 mins 41.013 secs
 Mellowship Slinky In B Major | 33.15 | 56 MB       | @ 29 mins 41.104 secs
 The Righteous & The Wicked   | 33.11 | 64 MB       | @ 33 mins 49.188 secs
 Give It Away                 | 33.08 | 73 MB       | @ 38 mins 32.198 secs
 Blood Sugar Sex Magik        | 33.07 | 81 MB       | @ 43 mins 3.427 secs
 Under The Bridge             | 33.05 | 90 MB       | @ 47 mins 27.786 secs
 Naked In The Rain            | 33.03 | 98 MB       | @ 51 mins 53.503 secs
 Apache Rose Peacock          | 33.03 | 107 MB      | @ 56 mins 35.729 secs
 The Greeting Song            | 33.01 | 113 MB      | @ 59 mins 49.322 secs
 My Lovely Man                | 33.02 | 122 MB      | @ 1 hour 4 mins 28.44 secs
 Sir Psycho Sexy              | 33.01 | 137 MB      | @ 1 hour 12 mins 45.132 secs
 They're Red Hot              | 33.01 | 140 MB      | @ 1 hour 13 mins 57.073 secs
(17 rows)
~~~

{{< alert success >}}

If that's your first encounter with a window function you might want to
read
[Understanding Window Functions](/blog/2013/08/understanding-window-functions/),
an all time favorite article of this website.

{{< /alert >}}

# Album genres

So, what kind of music do we have in this collection of Chinook data:

~~~ sql
  select genre.name, count(*),
         sum(count(*)) over () as tracks,
         round(100.0 * count(*) / sum(count(*)) over(), 2) as pct
    from           genre
         left join track using(genreid)
group by genre.name order by genre.name;
~~~

We have here the repartition of tracks per genre and the percentage (in the
*pct* column) of them with respect to the whole set of our 25 different
music genres here:

~~~ psql
        name        | count | tracks |  pct  
--------------------+-------+--------+-------
 Alternative        |    40 |   3503 |  1.14
 Alternative & Punk |   332 |   3503 |  9.48
 Blues              |    81 |   3503 |  2.31
 Bossa Nova         |    15 |   3503 |  0.43
 Classical          |    74 |   3503 |  2.11
 Comedy             |    17 |   3503 |  0.49
 Drama              |    64 |   3503 |  1.83
 Easy Listening     |    24 |   3503 |  0.69
 Electronica/Dance  |    30 |   3503 |  0.86
 Heavy Metal        |    28 |   3503 |  0.80
 Hip Hop/Rap        |    35 |   3503 |  1.00
 Jazz               |   130 |   3503 |  3.71
 Latin              |   579 |   3503 | 16.53
 Metal              |   374 |   3503 | 10.68
 Opera              |     1 |   3503 |  0.03
 Pop                |    48 |   3503 |  1.37
 R&B/Soul           |    61 |   3503 |  1.74
 Reggae             |    58 |   3503 |  1.66
 Rock               |  1297 |   3503 | 37.03
 Rock And Roll      |    12 |   3503 |  0.34
 Sci Fi & Fantasy   |    26 |   3503 |  0.74
 Science Fiction    |    13 |   3503 |  0.37
 Soundtrack         |    43 |   3503 |  1.23
 TV Shows           |    93 |   3503 |  2.65
 World              |    28 |   3503 |  0.80
(25 rows)
~~~

A more visual way to have a look at this data right from the console is with
the following query:

~~~ sql
  select genre.name, count(*),
         repeat('■', (  100.0
                      * count(*)
                      / sum(count(*)) over()
                     )::integer
               ) as pct
    from genre
         left join track using(genreid)
group by genre.name
order by genre.name;
~~~

Which gives this time:

~~~ psql
        name        | count |                  pct                  
--------------------+-------+---------------------------------------
 Alternative        |    40 | ■
 Alternative & Punk |   332 | ■■■■■■■■■
 Blues              |    81 | ■■
 Bossa Nova         |    15 | 
 Classical          |    74 | ■■
 Comedy             |    17 | 
 Drama              |    64 | ■■
 Easy Listening     |    24 | ■
 Electronica/Dance  |    30 | ■
 Heavy Metal        |    28 | ■
 Hip Hop/Rap        |    35 | ■
 Jazz               |   130 | ■■■■
 Latin              |   579 | ■■■■■■■■■■■■■■■■■
 Metal              |   374 | ■■■■■■■■■■■
 Opera              |     1 | 
 Pop                |    48 | ■
 R&B/Soul           |    61 | ■■
 Reggae             |    58 | ■■
 Rock               |  1297 | ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■
 Rock And Roll      |    12 | 
 Sci Fi & Fantasy   |    26 | ■
 Science Fiction    |    13 | 
 Soundtrack         |    43 | ■
 TV Shows           |    93 | ■■■
 World              |    28 | ■
(25 rows)
~~~

Now we easily spot that the music collection distribution by genre is skewed
over Rock, Latin, Metal and Punk music. Also there's a non trivial amount of
TV Shows in there.

# Multi-genres albums

In order to dive into the data, let's have a look at it from the genre point
of view. As we saw in the *Chinook* database model a given album is always
edited by a single artist, but might contain several different genres, one
per track. Do we have multi-genre albums?

~~~ sql
  select title as album,
         array_agg(distinct genre.name order by genre.name) as genres
    from      track
         join genre using(genreid)
         join album using (albumid)
group by title
  having count(distinct genre.name) > 1;
~~~

In this query, we use the *array_agg* PostgreSQL aggregates. As is name
suggests, *array_agg* build an *array* in which it *aggregates* all the data
it sees, in this case the data is *distinct* `genre.names` in a specific
ordering.

So apparently we have 11 albums with tracks from different genres:

~~~ psql
             album              |                      genres                       
--------------------------------+---------------------------------------------------
 Battlestar Galactica, Season 3 | {"Sci Fi & Fantasy","Science Fiction","TV Shows"}
 Greatest Hits                  | {Metal,Reggae,Rock}
 Heroes, Season 1               | {Drama,"TV Shows"}
 LOST, Season 4                 | {Drama,"TV Shows"}
 Live After Death               | {"Heavy Metal",Metal}
 Lost, Season 2                 | {Drama,"TV Shows"}
 Lost, Season 3                 | {Drama,"TV Shows"}
 Rock In Rio [CD2]              | {Metal,Rock}
 The Number of The Beast        | {Metal,Rock}
 The Office, Season 3           | {Comedy,"TV Shows"}
 Unplugged                      | {Blues,Latin}
(11 rows)
~~~

Many of those seem to not actually be music but rather video based content
such as TV Shows and Drama, maybe we can filter them out? What I would like
to do is to filter out entire albums based on a list of genres we're not
interested into (such as *TV Shows* and *Drama*, not that we don't like
them, but they won't probably compare well with the rest of the music
collection).

If we have a look at the *Unplugged* album, we can see that we have a data quality problem here:

~~~ sql
  select genre.name,
         array_agg(track.name order by trackid) as tracks
    from      track
         join album using(albumid)
         join genre using(genreid)
   where album.title = 'Unplugged'
group by genre.name;
~~~

~~~ psql
-[ RECORD 1 ]---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
name   | Blues
tracks | {Signe,"Before You Accuse Me","Hey Hey","Tears In Heaven","Lonely Stranger","Nobody Knows You When You're Down & Out",Layla,"Running On Faith","Walkin' Blues",Alberta,"San Francisco Bay Blues","Malted Milk","Old Love","Rollin' And Tumblin'"}
-[ RECORD 2 ]---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
name   | Latin
tracks | {"A Novidade","Tenho Sede",Refazenda,Realce,Esotérico,Drão,"A Paz","Beira Mar",Sampa,Parabolicamará,"Tempo Rei","Expresso 2222","Aquele Abraço",Palco,"Toda Menina Baiana","Sítio Do Pica-Pau Amarelo"}
~~~

My guess is that the *Latin* tracks associated with the album are actually
from another album and got there erroneously. That situation with a real
production data set might trigger some data quality assessment work.

The way to have the complete list of tracks per genre for each multi-genred
album is the following:

~~~ sql
with multi_genre_albums(albumid, artistid, title) as
 (
    select albumid, artistid, title
      from      track
           join genre using(genreid)
           join album using (albumid)
  group by albumid, artistid, title
    having count(distinct genre.name) > 1
 )
  select artist.name as artist,
         title as album,
         genre.name as genre,
         array_agg(track.name order by trackid) as tracks
    from multi_genre_albums
         join track using(albumid)
         join genre using(genreid)
         join artist using(artistid)
group by artist, album, genre;
~~~

We reuse the previous query in a *Common Table Expression* and this time
only output interesting columns: those we're going to reuse in the main
query, either for reference in later *joins*, or because we already have it
at this stage.

# Selecting a single genre per album

In order to be able to filter by genre at the album level, we need to first
be able to assign a single genre per album. Let's have some more statistics
about the multi-genred albums:

~~~ sql
select artist.name as artist,
       album.title as album,
       track.name as track,
       row_number()
         over(partition by albumid
                  order by trackid)
         as n,
       count(genreid)
         over(partition by albumid, genreid)
         as same

  from track
       join
       (
          select albumid,
                 array_agg(distinct genre.name
                           order by genre.name)
                   as genres
            from track
                 join genre using(genreid)
        group by albumid
          having count(distinct genre.name) > 1
        )
        as multi_genre_album(albumid, genres) using (albumid)

        join genre using(genreid)
        join album using(albumid)
        join artist using(artistid)
        
order by artist, album, trackid;
~~~

In this query *n* is the track order within its album and *same* is how many
tracks of the same genre do we have in said album. It's meant as an
intermediate query, the one you would cook just to have a manual look over
the data. Also you might notice that this time rather than using
*multi_genre_album* as a *Common Table Expression* we inlined it as a
*subquery*.

What we learn from the result of this query is that we can select the genre
having the highest number of tracks attached in an album as being the
album's genre. In particular we don't see equal scores when using that
simple rule, so we don't need a tie-breaker. Let's have a try at that:

~~~ sql
with album_genre_count(albumid, genreid, count, max)
 as (
      select albumid,
             genreid,
             count(trackid) as tracks,
             max(count(trackid)) over(partition by albumid)
        from track
    group by albumid, genreid
    order by albumid
 )
 select artist.name as artist,
        album.title as title,
        genre.name as genre
   from      album_genre_count
        join genre using(genreid)
        join album using(albumid)
        join artist using (artistid)
  where count = max
    and albumid in (select albumid
                      from track
                  group by albumid
                    having count(distinct genreid) > 1
                  order by albumid);
~~~

In the query we limit the output to those albums with more than one genre
attached to their tracks so that we can manually check our query. If you
remove this restriction by *albumid*, the query applies as-is to the whole
data set we have, so it could be made a view for later use. So let's have a
look at our problematic cases:

~~~ psql
        artist        |             title              |      genre      
----------------------+--------------------------------+-----------------
 Eric Clapton         | Unplugged                      | Latin
 Iron Maiden          | Live After Death               | Metal
 Iron Maiden          | Rock In Rio [CD2]              | Rock
 Iron Maiden          | The Number of The Beast        | Metal
 Lenny Kravitz        | Greatest Hits                  | Rock
 Battlestar Galactica | Battlestar Galactica, Season 3 | Science Fiction
 Heroes               | Heroes, Season 1               | Drama
 Lost                 | Lost, Season 3                 | Drama
 Lost                 | Lost, Season 2                 | TV Shows
 The Office           | The Office, Season 3           | Comedy
 Lost                 | LOST, Season 4                 | Drama
(11 rows)
~~~

All those albums are successfully assigned a single genre now. The genre we
chose is the most common value found in the album's track, and for our
interactive discovery of the data set, it's plenty good enough.

# Statistics per genre

Now that we are able to actually play with a single genre per album, what
can we learn about musical genre in that music collection? What about the
average track length? Well as we all know averages are not saying much (if
anything at all) about the data, let's have a look at the median, 90, 95,
and 99 percentiles instead:

~~~ sql
with album_genre_count(albumid, genreid, count, max)
 as (
      select albumid,
             genreid,
             count(trackid) as tracks,
             max(count(trackid)) over(partition by albumid)
        from track
    group by albumid, genreid
    order by albumid
 )
  select genre.name, 

         percentile_cont(array[0.5, 0.9, 0.95, 0.99])
           within group (order by ceil(milliseconds/60000))
         as distribution,

         repeat('■',
            ceil(
              percentile_cont(0.99)
                within group (order by ceil(milliseconds/60000))
            )::integer
         )
         as hist

    from genre
         left join track using(genreid)
         left join
         (
             select trackid, album_genre_count.genreid
               from track 
                    join album_genre_count using(albumid)
              where count = max
         )
         as normalized_track_genre using(genreid)
where genre.name not in ('Comedy',
                         'Drama',
                         'Sci Fi & Fantasy',
                         'Science Fiction',
                         'TV Shows' 
                         )
group by genre.name
order by genre.name;
~~~

And now we can see our genre stats applied to only the music selection:

~~~ psql
        name        |       distribution        |      hist      
--------------------+---------------------------+----------------
 Alternative        | {3.5,5,6,11}              | ■■■■■■■■■■■
 Alternative & Punk | {3,5,6,8}                 | ■■■■■■■■
 Blues              | {4,6,7,9}                 | ■■■■■■■■■
 Bossa Nova         | {3,6,6,6}                 | ■■■■■■
 Classical          | {4,8,9,9}                 | ■■■■■■■■■
 Easy Listening     | {3,4,4,4}                 | ■■■■
 Electronica/Dance  | {5,6,7,8}                 | ■■■■■■■■
 Heavy Metal        | {4.5,7,8,8}               | ■■■■■■■■
 Hip Hop/Rap        | {3,3,5,6}                 | ■■■■■■
 Jazz               | {4,6.10000000000036,9,14} | ■■■■■■■■■■■■■■
 Latin              | {3,5,5,7}                 | ■■■■■■■
 Metal              | {4,7,8,10}                | ■■■■■■■■■■
 Opera              | {2,2,2,2}                 | ■■
 Pop                | {3,4,5,11}                | ■■■■■■■■■■■
 R&B/Soul           | {3,5,5,6}                 | ■■■■■■
 Reggae             | {3,5,5,6}                 | ■■■■■■
 Rock               | {4,6,8,13}                | ■■■■■■■■■■■■■
 Rock And Roll      | {2,2,2,2}                 | ■■
 Soundtrack         | {4,4,5,6}                 | ■■■■■■
 World              | {3,4,4,5}                 | ■■■■■
(20 rows)
~~~

The array of numbers are the *median* and then the 0.90, 0.95 and 0.99
percentiles of the duration of tracks per genre, to the next minute. It
means that the *median* duration of a Rock track is under 4 minutes and up
to 5 minutes in Electronica/Dance genre. Also the longest tracks are to be
found in Jazz and Rock with up to respectively 14 and 13 minutes.

The quick histogram is based on the 0.99 percentile, so we have a visual of
the longest tracks per genre.

The genre we skipped in the query have duration distribution in between 21
and 88 minutes, so they would have ruined our quick analysis here.

# Conclusion

{{< image classes="fig25 right dim-margin"
              src="/img/old/sql-logo.png"
           title="PostgreSQL is YeSQL" >}}
           
When given a new database we know nothing about, it's good to do some
preliminary exploration of the data. Which angle we pick exactly is not that
important first, because we want to have grasp of what we are playing with.
It's all about having a taste of what's in there.

In this article we took the choice of not creating any SQL object. We could
have chosen to create *views* and maybe even to store their result for the
next query thanks to
a
[Materialized View](https://www.postgresql.org/docs/current/static/sql-creatematerializedview.html).

When the dataset is too big to explore that way, we can also use the
*TABLESAMPLE* clause, as per
the
[PostgreSQL SELECT](https://www.postgresql.org/docs/9.6/static/sql-select.html) documentation.

Mainly this exploration exercise is meant to teach some SQL, so I hope you
did discover several tricks you didn't know yet!

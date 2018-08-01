+++
title = "The Mode Ordered-Set Aggregate Function"
date = "2017-11-13T18:15:51+01:00"
tags = ["PostgreSQL","YeSQL","SQL","mode","Ordered-Set Aggregates"]
categories = ["PostgreSQL","YeSQL"]
coverImage = "/img/Plitvice-Cave-1.jpg"
coverMeta = "in"
coverSize = "partial"
thumbnailImage = "/img/explore-data.png"
thumbnailImagePosition = "left"

+++

In our article [Exploring a Data Set in
SQL](/blog/2017/06/exploring-a-data-set-in-sql/) we discovered a data set
related to music: the [Chinook](https://github.com/lerocha/chinook-database)
sample database.

Our discovery led us to find albums containing tracks of multiple genres,
and for the analytics we were then pursuing, we wanted to *clean* the data
set and assign a single genre per album. We did that in SQL of course, and
didn't actually edit the data.

Finding the most frequent input value in a group is a job for the `mode()
WITHIN GROUP (ORDER BY sort_expression)` Ordered-Set Aggregate Function, as
documented in the PostgreSQL page about [Aggregate
Functions](https://www.postgresql.org/docs/current/static/functions-aggregate.html#FUNCTIONS-ORDEREDSET-TABLE).

<!--more-->

For more about the *Mode* function name, read the Wikipedia article that
explain the Mathematics background behind it at [Mode
(statistics)](https://en.wikipedia.org/wiki/Mode_(statistics)).

## Multi-genres albums

In order to dive into the data, let's have a look at it from the genre point
of view. As we saw in the *Chinook* database model a given album is always
edited by a single artist, but might contain several different genres, one
per track. Do we have multi-genre albums?

~~~ sql
  select title as album,
         string_agg(
             distinct genre.name, ', '
             order by genre.name
         )
         as genres
    from      track
         join genre using(genreid)
         join album using (albumid)
group by title
  having count(distinct genre.name) > 1;
~~~

We have the following list of multi-genre albums in our collection:

~~~
             album              │                   genres                    
════════════════════════════════╪═════════════════════════════════════════════
 Battlestar Galactica, Season 3 │ Sci Fi & Fantasy, Science Fiction, TV Shows
 Greatest Hits                  │ Metal, Reggae, Rock
 Heroes, Season 1               │ Drama, TV Shows
 LOST, Season 4                 │ Drama, TV Shows
 Live After Death               │ Heavy Metal, Metal
 Lost, Season 2                 │ Drama, TV Shows
 Lost, Season 3                 │ Drama, TV Shows
 Rock In Rio [CD2]              │ Metal, Rock
 The Number of The Beast        │ Metal, Rock
 The Office, Season 3           │ Comedy, TV Shows
 Unplugged                      │ Blues, Latin
(11 rows)
~~~

## Selecting a single genre per album

In the previous article, we manually counted the number of tracks of each
genre per album and selected the most frequent genre: the one with the
greatest number of tracks.

SQL provides an Ordered-Set Aggregate Function that does just that:

~~~ sql
with multi_genre_albums
 as ( 
    select albumid
      from track
  group by albumid
    having count(distinct genreid) > 1
 )
 select artist.name as artist,
        album.title as title,
        genre.name as genre,
        mode() within group (order by genreid)
   from multi_genre_albums
        join genre using(genreid)
        join album using(albumid)
        join artist using (artistid);
~~~

In this query we limit our processing to albums containing tracks of several
genres, thanks to the `multi_genre_albums` CTE, where we use a simple `GROUP
BY` and `HAVING` clause. We then use the result of the CTE and fetch
supplementary information about the albums, and use the `mode()` Set-Ordered
Aggregate Function to retain only the most frequent `genreid`:

~~~
        artist        │             title              │      genre      
══════════════════════╪════════════════════════════════╪═════════════════
 Eric Clapton         │ Unplugged                      │ Latin
 Iron Maiden          │ The Number of The Beast        │ Metal
 Iron Maiden          │ Rock In Rio [CD2]              │ Rock
 Iron Maiden          │ Live After Death               │ Metal
 Lenny Kravitz        │ Greatest Hits                  │ Rock
 Battlestar Galactica │ Battlestar Galactica, Season 3 │ Science Fiction
 Heroes               │ Heroes, Season 1               │ Drama
 Lost                 │ LOST, Season 4                 │ Drama
 Lost                 │ Lost, Season 2                 │ TV Shows
 Lost                 │ Lost, Season 3                 │ Drama
 The Office           │ The Office, Season 3           │ Comedy
(11 rows)
~~~

Full documentation for the `mode()` function says:

> returns the most frequent input value (arbitrarily choosing the first one
> if there are multiple equally-frequent results)

Use it next time you want to select only the most-frequent element of a
group in your result-set.

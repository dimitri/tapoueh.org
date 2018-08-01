+++
title = "How to Write SQL"
date = "2017-06-08T13:23:26+02:00"
categories = ["PostgreSQL","YeSQL"]
tags = ["PostgreSQL","YeSQL","AnoSQL","Python","SQL"]
coverImage = "/img/matrix-code.jpg"
coverMeta = "in"
coverSize = "partial"
thumbnailImage = "/img/sql-filetype.svg"
thumbnailImagePosition = "left"

+++

[Kris Jenkins](https://twitter.com/krisajenkins) cooked up a very nice way
to embed SQL in your
code: [YeSQL for Clojure](https://github.com/krisajenkins/yesql). The main
idea is that you should be writing your SQL queries in `.sql` files in your
code repository and maintain them there.

The idea is very good and it is now possible to find alternative
implementations of the [Clojure](https://clojure.org) *yesql* library in
other languages. Today, we are going to have a look at one of them for
the [python](https://www.python.org) programming
language: [anosql](https://github.com/honza/anosql).

<!--more-->
<!--toc-->

## SQL is code

When you have to write SQL code, several options are available. One of them
is to consider that SQL is nothing more than a static literal string
embedded in your code, and use the tools that your programming language
provide for that,
e.g.
[PHP Heredoc](http://php.net/manual/en/language.types.string.php#language.types.string.syntax.heredoc) or
[python triple-quoted strings](https://docs.python.org/3.2/tutorial/introduction.html#strings).

Then, often enough you also want to integrate dynamic parts in your *query
string*... and now either you concatenate string parts and variables or you
need a templating facility of sorts. Or maybe you're thinking that your ORM
offers the perfect solution to that problem, allowing you to never resort to
writing *raw* SQL yourself...

In any case, a time will come when you need to debug a query that runs in
production, and now you need to find it again in your source code and edit
it. When you're lucky enough, you have a SQL savvy guy in the team, maybe
even a DBA, and so you're analyzing the query
using
[Explain](https://www.postgresql.org/docs/current/static/sql-explain.html)
and rewriting it to solve your problem, maybe an efficiency related one.

How easy it is for you to find the problematic query in your code, and then
to replace it with the new version you just came up with? In a lot of cases,
the new version will look nothing like the previous one. It might even use
constructs that your ORM knows nothing about.

Let's see an example query that works against
the [Chinook](https://github.com/lerocha/chinook-database) database. This
database models a digital media store, including tables for artists, albums,
media tracks, invoices, and customers. We have a table `track` with a
`milliseconds` column and we want to display both the time in a proper human
readable form and the percentage of each track duration with respect to the
total duration of the album:

~~~ sql
   select name as title,
          milliseconds * interval '1ms' as duration,
          round(  milliseconds
                / sum(milliseconds) over ()
                * 100, 2)
          as pct
    from track
   where albumid = :id
order by trackid;
~~~

And here's the output for `:id = 1`:

~~~ console
                  title                  │       duration       │  pct  
═════════════════════════════════════════╪══════════════════════╪═══════
 For Those About To Rock (We Salute You) │ @ 5 mins 43.719 secs │ 14.32
 Put The Finger On You                   │ @ 3 mins 25.662 secs │  8.57
 Let's Get It Up                         │ @ 3 mins 53.926 secs │  9.75
 Inject The Venom                        │ @ 3 mins 30.834 secs │  8.78
 Snowballed                              │ @ 3 mins 23.102 secs │  8.46
 Evil Walks                              │ @ 4 mins 23.497 secs │ 10.98
 C.O.D.                                  │ @ 3 mins 19.836 secs │  8.33
 Breaking The Rules                      │ @ 4 mins 23.288 secs │ 10.97
 Night Of The Long Knives                │ @ 3 mins 25.688 secs │  8.57
 Spellbound                              │ @ 4 mins 30.863 secs │ 11.28
(10 rows)
~~~

## psql variables

So, how did I run the previous query, may you ask?
Well [psql](https://www.postgresql.org/docs/current/static/app-psql.html) is
a wonderful PostgreSQL
interactive [REPL](https://en.wikipedia.org/wiki/Read–eval–print_loop) with
*variables* support.

Say I saved my query in a `album.sql` file and want to execute it against
the particular album I know the id of:

~~~ bash
psql --variable "id=1" -f album.sql chinook
~~~

Of course, it is also possible to create and edit the variables
interactively so instead I could have done the following from within a psql
session:

~~~ psql
> \cd path/to/my/sources
> \set id 1
> \i album.sql
~~~

The psql console being handy, you have *autocompletion* when entering the
file paths in both the `\cd` and `\i` commands.

So it is now quite easy to have your SQL query opened in your favorite
editor and a terminal window with the interactive psql console wherein you
can easily try your query with different values of your variables.

## Dynamically building SQL queries

In some cases, your code will use almost the same query in different places,
and it's easy to want to reduce duplication. Then what happens is that you
have conditional code to build the query string with entirely optional
clauses: a *where* clause might be omitted in some cases, or maybe even a
*join*. My advice is simple: don't do that.

SQL has support for
complex
[conditional expressions](https://www.postgresql.org/docs/current/static/functions-conditional.html) with
*CASE*, *COALESCE* and even [filter](http://modern-sql.com/feature/filter)
if you want to count it there.

In particular, it is possible to use the `CASE` clause within your `WHERE`
conditions if you need to, and the PostgreSQL documentation shows the
following example that also shows the evaluation rules of the clause:

~~~ sql
SELECT ... WHERE CASE WHEN x <> 0 THEN y/x > 1.5 ELSE false END;
~~~

So my advice is
to [Keep It Simple](https://en.wikipedia.org/wiki/KISS_principle) and have a
SQL file for each main set of conditions. Then in your application code, you
can pick the SQL file you need depending on the situation. You might have
some SQL code duplication when doing so, that's true and it is the main
*cons* argument against doing so. On the *pros* side though you have
achieved better modularity: it is now really easy to fix that query you
discover being problematic in your production logs. And it is even dead
simple to replay and *explain* the query interactively, either on your
developer environment or even in production if necessary (it is, sometimes).

## Integrating SQL code in python with anosql

Kris Jenkin's [yesql](https://github.com/krisajenkins/yesql) makes it easy
to implement your SQL in a `query.sql` file and then expose your queries as
functions. It is now possible to do so in python too thanks
to [anosql](https://github.com/honza/anosql), which is available
in [pip](http://pip-python3.readthedocs.io).

Here's an example of what the `album.sql` file would look like with AnoSQL
integration:

~~~ sql
-- name: list-tracks-by-albumid
-- List the tracks of an album, includes duration and position
   select name as title,
          milliseconds * interval '1ms' as duration,
          round(  milliseconds
                / sum(milliseconds) over ()
                * 100, 2)
          as pct
    from track
   where albumid = :id
order by trackid;
~~~

The main difference with the previous example is that we added a couple of
*documentation strings*. The first comment line gives a name to the
function, and the second one a python docstring. Here's how you would use
such a query file in your python code now:

~~~ python
import anosql
import psycopg2
import sqlite3
import argparse
import sys

class chinook(object):
    """Our database model and queries"""
    def __init__(self,
                 pgconnstring = "dbname=chinook application_name=cdstore"):
        self.pgconn = psycopg2.connect(pgconnstring)
        self.genre  = anosql.load_queries('postgres', 'genre.sql')
        self.artist = anosql.load_queries('postgres', 'artist.sql')
        self.album  = anosql.load_queries('postgres', 'album.sql')

    def genre_list(self):
        return self.genre.tracks_by_genre(self.pgconn)

    def genre_top_n(self, n):
        return self.genre.genre_top_n(self.pgconn, n=n)

    def artist_by_albums(self, n):
        return self.artist.top_artists_by_album(self.pgconn, n=n)

    def album_details(self, albumid):
        return self.album.list_tracks_by_albumid(self.pgconn, id=albumid)
        
def foo(albumid):
    db = chinook()
    for (title, duration, pct) in db.album_details(albumid):
        ... do something here ...
~~~

The *anosql* library automatically exposes python functions with *kwargs*
support for your queries, so that not only do you get to write proper SQL in
the good way™ — which means in SQL files that you can version control — but
also, all you have to do in your code is call functions or methods. And I
guess you already know how to do that!

You might have seen that I use *application_name=cdstore* in my connection
string. You can then see it in the PostgreSQL activity system view and in
the logs. This very simple thing helps tremendously when you want to relate
production activity with SQL embedded in your application's code. Do it now,
and be as granular as you can (module names, class names, package names,
etc).

## Tests

When implement SQL code as *.sql* files, you need to take care about test
coverage specifically.
My [SQL Regression Tests](/blog/2017/08/sql-regression-tests/) article gives
more details about how to test SQL with dedicated tools, introducing
both [pgTap](http://pgtap.org/)
and [RegreSQL](https://github.com/dimitri/regresql).

## Conclusion

{{< image classes="fig25 right dim-margin"
              src="/img/old/sql-logo.png"
           title="PostgreSQL is YeSQL" >}}
           
One aspect of using SQL to its full capacity is having a productive editing
environment with the same level of support you have when writing in another
programming language, and more. With *psql* you also get an interactive REPL
console to play with your code and adjust it.

Compared to using concatenated strings sprinkled all over your code, I don't
suppose I have to explain the benefits. Compared with an ORM, it means you
have control of the SQL queries you send to your production server and are
now actually able to have a productive talk and optimization session with
your friendly DBA. He might as well just send you *pull requests* with
better versions of your SQL, and if you already implement *peer review* you
will learn a lot very fast!

Finally, the same idea is available
in [other languages](https://github.com/krisajenkins/yesql#other-languages)
than Clojure and Python, I can see Javascript, Perl and Go over there, in
the middle of PHP and C#. Maybe you know of an alternative implementation
not yet listed?

This topic is an important one and you will find more about *How to Write
SQL* in the book I am currently
writing:
[Mastering PostgreSQL in Application Development](http://masteringpostgresql.com)!

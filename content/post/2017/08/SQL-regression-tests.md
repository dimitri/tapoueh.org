+++
title = "SQL Regression Tests"
date = "2017-08-08T17:55:51+02:00"
tags = ["PostgreSQL","YeSQL","SQL","testing", "regression"]
categories = ["PostgreSQL","YeSQL"]
coverImage = "/img/regressiontesting.png"
coverMeta = "in"
coverSize = "partial"
thumbnailImage = "/img/bug-512.png"
thumbnailImagePosition = "left"

+++

In a previous article here we
saw [How to Write SQL](/blog/2017/06/how-to-write-sql/) in your application
code. The main idea in that article is to maintain your queries in separate
SQL files, where it's easier to maintain them. In particular if you want to
be able to test them again in production, and when you have to work and
rewrite queries.

<!--more-->

Naturally, if you consider SQL as code (which I do), then you want all the
usual tooling around code. It's easy to benefit from *version control* when
your SQL files are stored in separate files, so that's a checked box. It
might be less easy to unit test and regression test your queries, you think…
In this article we see how to do that!

<!--toc-->

There are different ways to test your SQL queries, among them in good
position we can do *Unit Testing* and *Regression Testing*. The *Unit
Testing* is used to run your queries against static arguments and a static
data set, and allows to test very basic things. The assumptions that you
want to never break, because your code rely on them.

*Regression Testing* is about ensuring that when you change things, they
continue to implement the same contract. In term of SQL, you can either
change your data set (adding columns, altering data types, etc) or your
queries. If you run the same query against a different schema or a modified
data set that should be compatible it might be named *Integrity Testing*.

# Unit Testing SQL

For *Unit Testing* in PostgreSQL the perfect tool
is [pgTap](http://pgtap.org/): _pgTAP is a suite of database functions that
make it easy to write TAP-emitting unit tests in psql scripts or xUnit-style
test functions. The TAP output is suitable for harvesting, analysis, and
reporting by a TAP harness, such as those used in Perl applications._

When using pgTap, see
the
[relation-testing functions](http://pgtap.org/documentation.html#canyourelate) for
implementing result set based unit tests. From the documentation, let's pick
a couple example, testing against static result sets as *VALUES*:

~~~ sql
SELECT results_eq(
    'SELECT * FROM active_users()',
    $$
      VALUES (42, 'Anna'),
             (19, 'Strongrrl'),
             (39, 'Theory')
    $$,
    'active_users() should return active users'
);
~~~

and *ARRAYS*:

~~~ sql
SELECT results_eq(
    'SELECT * FROM active_user_ids()',
    ARRAY[ 2, 3, 4, 5]
);
~~~

As you can see your unit tests are coded in SQL too. Which means you have
all the SQL power at your hands to write tests, and also that you can check
your schema integrity directly in SQL, using PostgreSQL catalog functions.

So check out [pgTap](http://pgtap.org/) to implement PostgreSQL Unit Tests.

# RegreSQL

In my article [How to Write SQL](/blog/2017/06/how-to-write-sql/) the main
take out is that your SQL queries are to be managed in *.sql* files, just
like in the following example:

~~~ sql
-- name: list-albums-by-artist
-- List the album titles and duration of a given artist
  select album.title as album,
         sum(milliseconds) * interval '1 ms' as duration
    from album
         join artist using(artistid)
         left join track using(albumid)
   where artist.name = :'name'
group by album
order by album;
~~~

Such a query is expected to be processed either by `psql` directly thanks to
its
[psql variables](https://www.postgresql.org/docs/current/static/app-psql.html#APP-PSQL-VARIABLES) support,
or by a library like
Clojure's [YeSQL](https://github.com/krisajenkins/yesql) or
Python's [anosql](https://github.com/honza/anosql) for using in your
application's code base. Implementations exist in other programming
languages too of course, so check out if your favorite is listed already at
the main [YeSQL](https://github.com/krisajenkins/yesql) GitHub's page.

In the *put your money where your mouth is* department, allow me to unveil
the small and neat tool [RegreSQL](https://github.com/dimitri/regresql):

```bash
$ go get github.com/dimitri/regresql

$ regresql --help
Run regression tests for your SQL queries

Usage:
  regresql [command]

Available Commands:
  help        Help about any command
  init        Initialize regresql for use in your project
  list        list candidates SQL files
  plan        Creates missing plans for new queries
  test        Run regression tests for your SQL queries
  update      Creates or updates the expected output files
```

The idea of *RegreSQL* is to make it easy to run *Regression Tests* against
SQL queries that you manage with a *YeSQL* like library. In a local sample
application using the query shown above, we can use *RegreSQL* to run our
queries and check that their result is the expected one:

```
$ regresql test
Connecting to 'postgres:///chinook?sslmode=disable'… ✓
TAP version 13
ok 1 - src/sql/album-by-artist.1.out
ok 2 - src/sql/album-tracks.1.out
ok 3 - src/sql/artist.1.out
ok 4 - src/sql/genre-topn.top-3.out
ok 5 - src/sql/genre-topn.top-1.out
ok 6 - src/sql/genre-tracks.out
```

To be able to do that, we need parameters to associate with our queries.
*RegreSQL* calls that a test plan, and you can create your test plans with
the command `regresql init` or `regresql plan` if you're already initialized
a repository and just added some queries…

If we take the *regresql/plans/src/sql/genre-topn.yaml* plan file as an
example, it contains two sets of parameters for the *src/sql/genre-topn.sql*
query. The YAML file contains plans. Each plan has a name, then a list of
key and values. The keys are the SQL parameter names, and the values are the
actual values we want to test our queries with. Here's the *genre-topn.yaml*
content:

~~~ yaml
"top-3":
  "n": "3"
"top-1":
  "n": "1"
~~~

The *RegreSQL* tool creates empty plan files with a single plan named *"1"*,
and prefills it with empty string values for all the query parameters, so
that you only have to fill-in the actual values for testing.

Use `regresql update` to run the queries and register their output as the
*expected* ouput. In the following usage example, we did `regresql update`
with `"id": "1"` in our plan file, but then changed our mind and replaced it
with `"id": "2"` before running `regresql test`:

~~~ bash
$ regresql test
Connecting to 'postgres:///chinook?sslmode=disable'… ✓
TAP version 13
ok 1 - src/sql/album-by-artist.1.out
ok 2 - src/sql/album-tracks.1.out
# Query File: 'src/sql/artist.sql'
# Bindings File: 'regresql/plans/src/sql/artist.yaml'
# Bindings Name: '1'
# Query Parameters: 'map[n:2]'
# Expected Result File: 'regresql/expected/src/sql/artist.1.out'
# Actual Result File: 'regresql/out/src/sql/artist.1.out'
# 
# --- regresql/expected/src/sql/artist.1.out
# +++ regresql/out/src/sql/artist.1.out
# @@ -1,4 +1,5 @@
# -   name     | albums
# -------------+-------
# -Iron Maiden | 21
# +    name     | albums
# +-------------+-------
# +Iron Maiden  | 21
# +Led Zeppelin | 14
#  
not ok 3 - src/sql/artist.1.out
ok 4 - src/sql/genre-topn.top-3.out
ok 5 - src/sql/genre-topn.top-1.out
ok 6 - src/sql/genre-tracks.out
~~~

We can see that the query returns the 2 most prolific artists of our
collection rather than the expected single most prolific artist. If that's
your new requirements for the query, then *update* the expected result. If
not, then fix your query!

# Conclusion

{{< image classes="fig25 right dim-margin"
              src="/img/sql-filetype.svg"
           title="SQL is code!" >}}
           
SQL is code. We need a productive and complete tooling around it, and most
of this tooling is already available. It's quite easy to fill-in the gaps,
as soon as you start considering that SQL is code.


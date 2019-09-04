+++
title = "Postgres Connection Strings and psql"
date = "2019-09-04T11:38:02+02:00"
tags = ["PostgreSQL","YeSQL","psql","DSN"]
categories = ["PostgreSQL","YeSQL"]
coverImage = "/img/connection-rope.jpg"
coverMeta = "out"
coverSize = "partial"
thumbnailImage = "/img/knot-512.png"
thumbnailImagePosition = "left"

+++

[PostgreSQL connection
strings](https://www.postgresql.org/docs/current/libpq-connect.html#LIBPQ-CONNSTRING)
embedded in your application can take two different forms: the key-value
notation or the `postgresql://` URI scheme. When it comes to using `psql`
though, another form of connection string is introduced, with command line
options `-h -p -U` and environment variable support.

In this short article you will learn that you can use either of the three
different forms in `psql` and thus easily copy & paste you application
connection string right at the console to test it!

<!--more-->

When using `psql` to connect to your Postgres database, you might be used to
using the following options:

~~~ bash
$ psql --help
psql is the PostgreSQL interactive terminal.

Usage:
  psql [OPTION]... [DBNAME [USERNAME]]

General options:
  -c, --command=COMMAND    run only single command (SQL or internal) and exit
  -d, --dbname=DBNAME      database name to connect to (default: "dim")

...
Connection options:
  -h, --host=HOSTNAME      database server host or socket directory (default: "local socket")
  -p, --port=PORT          database server port (default: "5432")
  -U, --username=USERNAME  database user name (default: "dim")
  -w, --no-password        never prompt for password
  -W, --password           force password prompt (should happen automatically)
~~~

The following trick is not apparent in this help message, and maybe easy to
miss when reading the [manual page for
psql](https://www.postgresql.org/docs/current/app-psql.html). You can
actually use a whole connection string for the `dbname` parameter.

Here's three different ways to establish a connection to the same database,
the one I'm using in my book [The Art of
PostgreSQL](https://theartofpostgresql.com) of course:

~~~ bash
$ psql -Atx -U taop -d taop -h localhost -p 5432 -c 'select current_date'
2019-09-04

$ psql -Atx postgresql://taop@localhost:5432/taop -c 'select current_date'
2019-09-04

$ psql -Atx "host=localhost port=5432 dbname=taop user=taop" -c 'select current_date' 
2019-09-04
~~~

What happens is that the first comamnd line argument is used by `psql` as
the dbname here, and the connection string parsing provided by `libpq` is
done on that parameter. So that for the `-d dbname` parameter, you can
actually pass in a whole connection string:

~~~ bash
$ psql -d "host=localhost port=5432 dbname=taop user=taop"
psql (12devel, server 10.10)
Type "help" for help.

taop> select current_date;
┌──────────────┐
│ current_date │
├──────────────┤
│ 2019-09-04   │
└──────────────┘
(1 row)
~~~

So if your application code is using a driver based on `libpq` or a
compatible connection string scheme, re-using the application connection
string on your terminal with `psql` is a simple copy-paste away.

~~~ python
import sys
import psycopg2
import psycopg2.extras
from calendar import Calendar

CONNSTRING = "dbname=taop application_name=factbook"
~~~

I hope you'll find this simple trick useful in your daily usage of `psql`
and PostgreSQL! For more about `psql`, you can also read my article [Setting
up psql, the PostgreSQL
CLI](https://tapoueh.org/blog/2017/12/setting-up-psql-the-postgresql-cli/)
where we dive in my current (at the time) setup for it. I recently changed
my `~/.psqlrc` file to integrate [pspg](https://github.com/okbob/pspg) as
the pager, and will probably do write-up about that later.

Meanwhile, check-out my book [The Arf of
PostgreSQL](https://theartofpostgresql.com), you can register to get a
**free** sample with many other good SQL techniques to improve your
developer skills!

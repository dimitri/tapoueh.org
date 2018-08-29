+++
title = "Preventing SQL Injections"
date = "2018-08-29T16:17:00-07:00"
tags = ["PostgreSQL","YeSQL","Security","SQL Injection"]
categories = ["PostgreSQL","YeSQL"]
coverImage = "/img/sql-injection.jpg"
coverMeta = "in"
coverSize = "partial"
thumbnailImage = "/img/sql-injection-icon.png"
thumbnailImagePosition = "left"

+++

An *SQL Injection* is a security breach, one made famous by the [Exploits of
a Mom](https://xkcd.com/327/) `xkcd` comic episode in which we read about
*little Bobby Tables*:

{{< figure class="center"
             src="/img/exploits_of_a_mom.png"
            link="https://xkcd.com/327/" >}}

PostgreSQL implements a protocol level facility to send the static SQL query
text separately from its dynamic arguments. An SQL injection happens when
the database server is mistakenly led to consider a dynamic argument of a
query as part of the query text. Sending those parts as separate entities
over the protocol means that SQL injection is no longer possible.

<!--more-->

The PostgreSQL protocol is fully documented and you can read more about
*extended query* support on the [Message
Flow](https://www.postgresql.org/docs/current/static/protocol-flow.html)
documentation page. Also relevant is the `PQexecParams` driver API,
documented as part of the [command execution
functions](https://www.postgresql.org/docs/current/static/libpq-exec.html)
of the `libpq` PostgreSQL C driver.

A lot of PostgreSQL application drivers are based on the libpq C driver,
which implements the PostgreSQL protocol and is maintained alongside the
main server's code. Some drivers variants also exist that don't link to any
C runtime, in which case the PostgreSQL protocol has been implemented in
another programming language. That's the case for variants of the JDBC
driver, and the `pq` Go driver too, among others.

It is advisable that you read the documentation of your current driver and
understand how to send SQL query parameters separately from the main SQL
query text; this is a reliable way to never have to worry about *SQL
injection* problems ever again.

In particular, ***never*** build a query string by concatenating your query
arguments directly into your query strings, i.e. in the application client
code. Never use any library, ORM or another tooling that would do that. When
building SQL query strings that way, you open your application code to
serious security risk for no reason.

~~~ Python
def fetch_month_data(year, month):
    "Fetch a month of data from the database"
    date = "%d-%02d-01" % (year, month)
    sql = """
  select date, shares, trades, dollars
    from factbook
   where date >= date %s
     and date  < date %s + interval '1 month'
order by date;
"""
    pgconn = psycopg2.connect(CONNSTRING)
    curs = pgconn.cursor()
    curs.execute(sql, (date, date))

    res = {}
    for (date, shares, trades, dollars) in curs.fetchall():
        res[date] = (shares, trades, dollars)

    return res
~~~

We are using the [psycopg](http://initd.org/psycopg/) Python driver in the
example above, and Psycopg is based on `libpq`. The documentation of this
driver addresses [passing parameters to SQL
queries](http://initd.org/psycopg/docs/usage.html#passing-parameters-to-sql-queries)
right from the beginning. *Psycopg* is making good use of the functionality
we just described, and our `factbook-month.py` program above makes use of
the `%s` syntax for SQL query arguments, so we're safe.

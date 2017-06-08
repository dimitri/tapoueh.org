+++
comments = false
date = "2017-05-22T00:50:29+02:00"
keywords = ["Dimitri Fontaine", "PostgreSQL", "Open Source Software Engineer"]
showDate = false
showPagination = false
showSocial = false
showTags = false
title = "About Dimitri Fontaine"
aliases = ["/about.html","/about"]
+++

{{< image classes="fig50 right fancybox dim-margin"
              src="/img/dimface.320.png"
            title="Dimitri, picture by Oleg Bartunov" >}}

Hi! My name is Dimitri. I have been using and contributing to Open Source
Software for the best of the last twenty years. I took role as developer,
maintainer, packager, release manager, software architect, database
architect and administrator, sometimes *devops*. In the same time frame I
also have started several companies (they are still thriving) with a strong
Open Source business model, and I have been in *manager* positions at times
too, including at the Executive level in big companies.

If you've
read
[Charity Major's Engineer/Manager Pendulum](https://charity.wtf/2017/05/11/the-engineer-manager-pendulum/) piece,
there's something to that in my career up to now, definitely. From the
hacker guy to the CEO of a small company, from the PostgreSQL Major
Contributor to the CTO of a big company.

<hr/>

{{< image classes="fig50 left fancybox dim-margin" src="/images/book-320.jpg" >}}
            
In this place I share my [PostgreSQL](/tags/postgresql/) expertise. You will
find about my [projects](/projects/) of course, and the blog is full of
interesting articles: have a try
at
[Understanding Window Functions](/blog/2013/08/understanding-window-functions/) which
has been helpful to many readers! This article is even part of a data driven
series of post, the [YeSQL](/categories/yesql/) category, check it out!

Also, I'm writing a book
about
[Mastering PostgreSQL in Application Development](http://masteringpostgresql.com),
and you can subscribe to get sample content before anyone else and be the
first to get the book when it's available!



<hr>

{{< image classes="fig50 right fancybox dim-margin"
              src="/img/postgresql-elephant.png"
            title="PostgreSQL Major Contributor" >}}

The most notable project I have contributed to
is [PostgreSQL](https://www.postgresql.org). I wrote
the
[Extension Packaging](https://www.postgresql.org/docs/current/static/extend-extensions.html) feature
and
the
[Event Triggers](https://www.postgresql.org/docs/current/static/event-triggers.html) facility,
among other contributions.

As
a
[debian](https://www.debian.org) [maintainer](https://qa.debian.org/developer.php?login=dim@tapoueh.org) I
also participated into building a PostgreSQL repository for all currently
supported version of both PostgreSQL and debian, available
at [apt.postgresql.org](https://wiki.postgresql.org/wiki/Apt),
with [Christoph Berg](https://github.com/ChristophBerg)
and [Magnus Hagander](https://blog.hagander.net).

My main project related to PostgreSQL these days
is [pgloader](http://pgloader.io) which allows loading data into PostgreSQL
and include facilities to handle a full database migration from a live
database connection!

<hr>

{{< image classes="fig50 center fancybox dim-margin"
              src="/img/type-casting-machine.500.jpg"
            title="pgloader features user defined type casting" >}}

[pgloader](http://pgloader.io) is an awesome database migration tool that I
wrote, open source and available at <http://github.com/dimitri/pgloader>.
You can use it in a _Continuous Integration_ environment and keep migrating
your **schema** and **data** until your application tests all pass, then
decide to go live with PostgreSQL!

In the general case to migrate from
a [MySQL](https://www.youtube.com/watch?v=emgJtr9tIME) to PostgreSQL
with [pgloader](http://pgloader.io) all you need to do is run a single
command:

```
$ pgloader mysql://user@host/dbname pgsql:///dbname
```

Also supported are SQLite and Microsoft® SQL Server. Email me if you want to
add Oracle™ to the list!

<hr>

{{< image classes="fig50 left fancybox dim-margin"
              src="/img/open-source-software-320.jpg"
            title="Open Source Software Engineering" >}}


I can help you use PostgreSQL in your development environment, be
it [Go](https://golang.org)
or [Java](https://jdbc.postgresql.org), [Erlang](http://www.erlang.org)
or
[Common Lisp](https://common-lisp.net),
[Python](https://www.python.org), [Ruby](https://www.ruby-lang.org/en/)
or [PHP](http://php.net), or something else entirely. It's
still [PostgreSQL](https://www.postgresql.org) after all...

The first step here is realizing that your database engine actually is part
of your application logic. Any SQL statement you write, even the simplest
possible, embeds some logic already: you are *projecting* a particular set
of columns, *filtering* the result to only a part of the available data set
(thanks to the `where` clause), and you want to receive the result in a
known *ordering*. That already is business logic. Application code written
in SQL.

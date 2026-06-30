+++
comments = false
date = "2026-06-29T00:00:00+02:00"
keywords = ["Dimitri Fontaine", "PostgreSQL", "Open Source Software Engineer"]
showDate = false
showPagination = false
showSocial = false
showTags = false
title = "About Dimitri Fontaine"
aliases = ["/about.html","/about"]
+++

{{< image classes="fig25 right fancybox dim-margin" src="/img/dimface.320.png" title="Dimitri, picture by Oleg Bartunov" >}}

I am a PostgreSQL developer, author, and open-source builder based near Paris.
Most of what I have done professionally for the past twenty-plus years has
orbited around PostgreSQL — writing code for the database itself, building tools
on top of it, teaching it, and occasionally starting or running companies
around it.

## PostgreSQL Core

I am a **PostgreSQL Major Contributor**. Two features I wrote ship in every
PostgreSQL installation:

**[Extensions](https://www.postgresql.org/docs/current/extend-extensions.html)**
— the packaging system that lets you distribute and install add-ons for
PostgreSQL. `CREATE EXTENSION` is mine. Before extensions, third-party modules
required patching the source tree; now they are first-class citizens of the
ecosystem.

**[Event Triggers](https://www.postgresql.org/docs/current/event-triggers.html)**
— DDL-level triggers that fire on schema changes, giving you programmatic
control over `CREATE TABLE`, `DROP INDEX`, and the rest of the DDL vocabulary.

As a [Debian](https://www.debian.org)
[maintainer](https://qa.debian.org/developer.php?login=dim@tapoueh.org),
I helped build [apt.postgresql.org](https://wiki.postgresql.org/wiki/Apt)
together with [Christoph Berg](https://github.com/ChristophBerg) and
[Magnus Hagander](https://blog.hagander.net) — the repository that has kept
every supported PostgreSQL version installable on every supported Debian and
Ubuntu release for well over a decade.

## Open Source Tools

The [Projects](/projects/) page has the full list, but the main ones:

[**pgloader**](https://pgloader.io) has been around since 2005. It loads data
into PostgreSQL — and migrates whole databases from MySQL, SQLite, or MS SQL
with a single command. It was originally Python, then rewritten in Common Lisp
for real concurrency, and is now moving to Clojure.

[**pgcopydb**](https://pgcopydb.readthedocs.io) copies an entire PostgreSQL
database to another server in parallel, with change-data-capture support for
near-zero-downtime migrations. The kind of tool you reach for when `pg_dump |
pg_restore` is too slow or too blunt.

[**pg_auto_failover**](https://pg-auto-failover.readthedocs.io) automates
failover and high availability for PostgreSQL around a simple monitor node —
no external consensus cluster, no zoo of processes to manage.

[**el-get**](https://github.com/dimitri/el-get) predates MELPA as an Emacs
package manager. It installs packages from anywhere — GitHub, EmacsWiki, tarballs
— with full dependency resolution.

## Career

If you have read
[Charity Majors on the Engineer/Manager Pendulum](https://charity.wtf/2017/05/11/the-engineer-manager-pendulum/),
there is something to that in how things have gone for me — from PostgreSQL
hacking to running small companies and back to engineering.

I have started several companies with open-source at the core, spent time as
CTO and CEO, then moved back into a principal engineering role. I spent a
number of years at [2ndQuadrant](https://www.2ndquadrant.com) doing
PostgreSQL consulting and development, then joined
[Citus Data](https://www.citusdata.com) to work on distributed PostgreSQL and
build pg_auto_failover — Citus was acquired by Microsoft in 2019.

## The Book

I wrote [The Art of PostgreSQL](https://theartofpostgresql.com) for
application developers who want to get serious about SQL. The premise is that
most developers are leaving enormous query power on the floor — not because
PostgreSQL is hard, but because nobody showed them what it can actually do.
The book, the self-paced courses, the workshops, and The Lab all come from
the same place: twenty years of watching people underuse a database that could
do most of their heavy lifting.

A good place to start on this blog is
[Understanding Window Functions](/blog/2013/08/understanding-window-functions/),
part of the data-driven [YeSQL](/categories/yesql/) series.

{{< ecosystem >}}

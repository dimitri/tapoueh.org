+++
comments = false
date = "2017-05-22T00:50:29+02:00"
keywords = ["Dimitri Fontaine", "PostgreSQL", "Open Source Software Engineer"]
showDate = false
showPagination = false
showSocial = false
showTags = false
title = "Projects"
coverImage = "/img/postgresql-emacs.jpg"
coverSize = "partial"
coverMeta = "out"
aliases = ["/projects.html","/projects","/emacs/el-get.html",
           "/pgsql", "/pgsql/index.html", "/emacs", "/emacs/index.html"]
+++

Along the years I have been contributing to many projects, and I started
some of my own too. If you want to have an idea of the graveyard, have a
look at the full repositories page on my GitHub
profile: <https://github.com/dimitri?tab=repositories>. I used to still list
some projects like `M-x mailq` that I've not been using in a long while, or
`M-x rcirc-groups-mode` that I still use daily, which allows the project to
have at least one user...

Here's a short list of some projects that are maintained and have users!

<!--toc-->

{{< image classes="fig33 right fancybox dim-margin"
              src="/img/toy-loader.320.jpg"
            title="pgloader is not a toy!" >}}

# pgloader

[pgloader](http://pgloader.io) is an awesome database migration tool that I
wrote, open source, at <http://github.com/dimitri/pgloader>. You can use it
in a _Continuous Integration_ environment and keep migrating your **schema**
and **data** until your application tests all pass, then go live with
PostgreSQL!

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
              src="/img/prefix-trie.jpg"
            title="A Prefix Trie data structure" >}}

# prefix

The [prefix](https://github.com/dimitri/prefix) PostgreSQL Extension is an
data type and an indexing strategy allowing to speed-up SQL queries
searching for matching prefixes: a routing table for example.

```sql
explain (costs off) select * from ranges where prefix @> '0146640123';
                         QUERY PLAN                         
------------------------------------------------------------
 Bitmap Heap Scan on ranges
   Recheck Cond: (prefix @> '0146640123'::prefix_range)
   ->  Bitmap Index Scan on idx_prefix
         Index Cond: (prefix @> '0146640123'::prefix_range)
(4 rows)
```

The [prefix debian package](https://packages.debian.org/source/sid/prefix)
is available both in the official distribution and
in <https://apt.postgresql.org>.

<hr>

{{< image classes="fig50 right fancybox dim-margin"
              src="/img/el-get.big.png"
            title="El-Get logo" >}}

# el-get

Of course, my emacs setup is managed in a private `git` repository. Some
people would
use
[git submodules](http://www.kernel.org/pub/software/scm/git/docs/git-submodule.html) for
managing external repositories in there, but all I can say is that I frown
on this idea. I want an easy canonical list of packages I depend on to run
emacs, and I want this documentation to be usable as-is.
Enters [El-Get](https://github.com/dimitri/el-get)!


<hr>

# Emacs switch-window


{{< image classes="fig50 center fancybox dim-margin"
              src="/img/emacs-switch-window.png"
            title="Emacs Switch Window is visual!" >}}

This is a ***visual*** replacement for `C-x o`, so here's what it looks like
when you use it! Go check out the Github page of the project
at <https://github.com/dimitri/switch-window>, there's more for you!

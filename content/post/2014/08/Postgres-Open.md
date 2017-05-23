+++
date = "2014-08-29T14:26:00.000000+02:00"
title = "Going to Chicago, Postgres Open"
tags = ["PostgreSQL", "Conferences", "YeSQL"]
categories = ["Conferences","PostgreSQL Confs"]
thumbnailImage = "/confs/pgopen.jpg"
thumbnailImagePosition = "left"
coverImage = "/confs/pgopen.jpg"
coverSize = "partial"
coverMeta = "out"
aliases = ["/blog/2014/08/29-Postgres-Open",
           "/blog/2014/08/29-Postgres-Open.html"]
+++

Next month, 
[Postgres Open 2014](https://postgresopen.org/2014/) is happening in Chicago, and I'll have the
pleasure to host a tutorial about PostgreSQL Extensions
[Writing & Using Postgres Extensions](https://postgresopen.org/events/schedule/pgopen2014/session/77-4-writing-using-postgres-extensions/index.html/), and a talk aimed at developers wanting
to make the best out of PostgreSQL, 
[PostgreSQL for developers](https://postgresopen.org/events/schedule/pgopen2014/session/56-postgresql-for-developers/):


<div class="figure center dim-margin">
  <a href="https://postgresopen.org/2014/">
    <img src="/img/old/postgresopen.png">
  </a>
</div>

The tutorial is based on first hand experience on the
[PostgreSQL Extension Packaging System](http://www.postgresql.org/docs/current/interactive/extend-extensions.html) both as a user and a developer. It's a
series of practical use cases where using extensions will simplify your life
a lot, and each of those practical use case is using real world data (thanks
to 
[pgloader](http://pgloader.io/)).

Most of the examples covered in the tutorial have a blog entry here that
present the idea and the solution, so the tutorial is all about putting it
all together. You can already read the blog posts under the 
[YeSQL](/tags/yesql) and
[Extensions](/tags/extensions) for a preview.

The developer talk itself is based on the 
[Reset Counter](/blog/2012/10/05-reset-counter) use case where we
learn a lot about 
[Common Table Expressions, or WITH queries](http://www.postgresql.org/docs/9.3/interactive/queries-with.html) and
[Window Functions](http://tapoueh.org/blog/2013/08/20-Window-Functions), my favourite SQL clauses.

If you want to learn how to implement a modern search user interface for
your own product, something that your users know how to use already, then
the tutorial is for you, as we will cover PostgreSQL based approximate
searches with suggestions (
*did you mean ...?*) and autocompletion.


{{< image classes="fig50 center fancybox dim-margin" src="/img/old/simplicity.png" >}}


The tutorial gives you the toolset you will use to avoid the situation
depicted here.

See you all in Chicago!

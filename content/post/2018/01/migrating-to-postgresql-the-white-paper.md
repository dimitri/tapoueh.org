+++
title = "Migrating to PostgreSQL, the White Paper"
date = "2018-01-11T10:01:45+01:00"
tags = ["PostgreSQL","pgloader","book"]
categories = ["PostgreSQL","pgloader"]
coverImage = "/img/crumpled_paper_texture_by_pkgam-d73k5mj.jpg"
coverMeta = "in"
coverSize = "partial"
thumbnailImage = "/img/MigratingToPostgreSQL-Cover.png"
thumbnailImagePosition = "left"

+++

After having been involved in many migration projects over the last 10
years, I decided to publish the following [White
Paper](https://pgloader.io/white-paper) in order to share my learnings.

The paper is titled [Migrating to PostgreSQL, Tools and
Methodology](https://pgloader.io/white-paper) and details the [Continuous
Migration](https://pgloader.io/blog/continuous-migration/) approach. It
describes how to migrate from another relational database server technology
to PostgreSQL. The reasons to do so are many, and first among them is often
the licensing model.

<!--more-->

## From MySQL to PostgreSQL over the Week-End!

On February the 18th, 2015 I received a pretty interesting mention on
Twitter:

<center>
<blockquote class="twitter-tweet" data-lang="en"><p lang="en" dir="ltr"><a href="https://twitter.com/tapoueh?ref_src=twsrc%5Etfw">@tapoueh</a> thank you for pgloader, it made our migration from mysql to postgresql really easy (~1Tb)</p>&mdash; CommaFeed (@CommaFeed) <a href="https://twitter.com/CommaFeed/status/568053907370450944?ref_src=twsrc%5Etfw">February 18, 2015</a></blockquote> <script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script> 
</center>

Their story follows:

<center>
<blockquote class="twitter-tweet" data-lang="en"><p lang="en" dir="ltr"><a href="https://twitter.com/tapoueh?ref_src=twsrc%5Etfw">@tapoueh</a> it was almost too easy, I just ran the one-liner and waited for 48 hours. Nothing to change in the app, thanks to hibernate.</p>&mdash; CommaFeed (@CommaFeed) <a href="https://twitter.com/CommaFeed/status/568055592704716800?ref_src=twsrc%5Etfw">February 18, 2015</a></blockquote> <script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script> 
</center>

While this is awesome news for this particular project, it is still pretty
rare that having to change your connection string is all you need to do to
handle a migration!

<figure style="float: right; clear: left; display: block; width: 200px; margin-right: 0em;">
    <a href="https://pgloader.io/white-paper/">
        <img style="width:200px; height: 229px; border: 1px solid lightblue; box-shadow: 15px 0 20px -20px lightblue, -15px 0 20px -20px lightblue;"
               src="/img/MigratingToPostgreSQL-Cover.png">
    </a>
</figure>

If you're less lucky than *CommaFeed*, you might want to prepare for a long
running project and activate the necessary resources, both servers and
people availability. Even if you're using an ORM and never wrote a SQL query
yourself, your application is still sending SQL queries to the database, and
maybe not all of them can be written in a PostgreSQL compatible way.

Get the White Paper now and learn about the **Continuous Migration**
methodology in order to make your migration to PostgreSQL a success. The
White Paper is free to download, just register your email for updates!

<script async id="_ck_322615" src="https://forms.convertkit.com/322615?v=6">
</script>

## Continuous Migration

Migrating from one database technology to PostgreSQL requires solid project
methodology. In this document we show a simple and effective database
migration method, named ***Continuous Migration***:

  1. Setup your target PostgreSQL architecture
  2. Fork a continuous integration environment that uses PostgreSQL
  3. Migrate the data over and over again every night, from your current
     production RDBMS
  4. As soon as the CI is all green using PostgreSQL, schedule the D-day
  5. Migrate without any unpleasant suprises… and enjoy! 

This method makes it possible to break down a huge migration effort into
smaller chunks, and also to pause and resume the project if need be. It also
ensures that your migration process is well understood and handled by your
team, drastically limiting the number of surprises you may otherwise
encounter on migration D-day.

## Conclusion

The the third step above is the migration of the data, and this isn't always
as easy to implement as it should be, and that's why the
[pgloader](https://pgloader.io) open source project exists: it implements
fully automated database migrations!

For your developers to be ready when it comes to making the most out of
PostgreSQL, consider reading [The Art of
PostgreSQL](https://theartofpostgresql.com) which comes with an Enterprise
Edition that allows a team of up to 50 developers to share this great
resource.

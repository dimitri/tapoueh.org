+++
date = "2011-10-06T18:23:00.000000+02:00"
title = "Scaling Stored Procedures"
tags = ["PostgreSQL", "Extensions", "plproxy"]
categories = ["PostgreSQL","Extensions"]
thumbnailImage = "/img/postgresql-512.jpg"
thumbnailImagePosition = "left"
coverImage = "/img/postgresql-512.jpg"
coverSize = "partial"
coverMeta = "out"
aliases = ["/blog/2011/10/06-scaling-with-stored-procedures",
           "/blog/2011/10/06-scaling-with-stored-procedures.html"]
+++

In the news recently 
*stored procedures* where used as an excuse for moving
away logic from the database layer to application layer, and to migrate away
from a powerful technology to a simpler one, now that there's no logic
anymore in the database.

It's not the way I would typically approach scaling problems, and apparently
I'm not alone on the 
*Stored Procedures* camp.  Did you read this nice blog
post 
[Mythbusters: Stored Procedures Edition](http://ora-00001.blogspot.com/2011/07/mythbusters-stored-procedures-edition.html) already?  Well it happens in
another land that where my comfort zone is, but still has some interesting
things to say.

I won't try and address all of the myths they attack in a single article.
Let's pick the scalability problems, the two of them I think about are code
management and performances.  We are quite well equiped for that in
PostgreSQL, really.

For code maintainance we now have 
[PostgreSQL Extensions](http://www.postgresql.org/docs/9.1/static/extend-extensions.html), which allows you to
pack all your procedures into separate 
*extensions*, and to maintain a version
number and upgrade procedures for each of them.  You can handle separate
rollouts in development for going from 
`1.12` to 
`1.13` then 
`1.14` and after the
developers tested it more completely and changed their mind again on the
best API they want to work with, 
`1.15` which is stamped ok for production.
At this point, 
`ALTER EXTENSION UPGRADE` will happily apply all the rollouts
in sequence to upgrade from 
`1.12` straight to 
`1.15` in one go.  And if you
prefer to bake a special careful script to handle that big jump, you also
can provide a specific 
`extension--1.12--1.15.sql` script.

Of course you're managing all those files with your favorite 
*SCM*, to answer
to some other myth from the blog reference we are loosely following.


<div class="figure center dim-margin">
  <a href="http://postgresqlrussia.org/articles/view/131">
    <img src="/img/old/Moskva_DB_Tools.v3.png">
  </a>
</div>

I wanted to talk about the other side of the scalability problem, which is
the operations side of it.  What happens when you need to scale the database
in terms of its size and level of concurrent activity?  PostgreSQL earned a
very good reputation at being able to scale-up, what about scaling-out?
Certainly, now that you're all down into 
*Stored Procedure*, it's going to be
a very bad situation?

Well, in fact, you're then in a very good position here, thanks to 
[PLproxy](http://wiki.postgresql.org/wiki/PL/Proxy).
This 
*extension* is a custom procedural language whose job is to handle a
cluster of database shards that all expose the same PL API, and it's very
good at doing that.

*Stored Procedures* are a very good tool to have, be sure to get comfortable
enough with them so that you can choose exactly when to use them.  If you're
not sure about that, we at 
[2ndQuadrant](http://www.2ndquadrant.com/) will be happy to help you there!

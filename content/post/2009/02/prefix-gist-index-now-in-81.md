+++
date = "2009-02-10T00:00:00.000000+01:00"
title = "Prefix GiST index now in 8.1"
tags = ["PostgreSQL", "prefix"]
categories = ["Projects","prefix"]
thumbnailImage = "/img/postgresql-512.jpg"
thumbnailImagePosition = "left"
coverImage = "/img/postgresql-512.jpg"
coverSize = "partial"
coverMeta = "out"
aliases = ["/blog/2009/02/10-prefix-gist-index-now-in-81",
           "/blog/2009/02/10-prefix-gist-index-now-in-81.html"]
+++

The 
[prefix](http://blog.tapoueh.org/prefix.html) project is about matching a 
*literal* against 
*prefixes* in your
table, the typical example being a telecom routing table. Thanks to the
excellent work around 
*generic* indexes in PostgreSQL with 
[GiST](http://www.postgresql.org/docs/current/static/gist-intro.html), indexing
prefix matches is easy to support in an external module. Which is what
the 
[prefix](http://prefix.projects.postgresql.org/) extension is all about.

Maybe you didn't come across this project before, so here's the typical
query you want to run to benefit from the special indexing, where the 
`@>`
operator is read 
*contains* or 
*is a prefix of*:

~~~
SELECT * FROM prefixes WHERE prefix @> '0123456789';
~~~


Now, a user asked about an 
`8.1` version of the module, as it's what some
distributions ship (here, Red Hat Enterprise Linux 5.2). It turned out it
was easy to support 
`8.1` when you already support 
`8.2`, so the 
`CVS` now hosts
[8.1 support code](http://cvs.pgfoundry.org/cgi-bin/cvsweb.cgi/prefix/prefix/). And here's what the user asking about the feature has to
say:

> It's works like a charm now with 3ms queries over 200,000+ rows.  The speed
> also stays less than 4ms when doing complex queries designed for fallback,
> priority shuffling, and having multiple carriers.


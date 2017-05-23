+++
date = "2009-07-09T12:48:00.000000+02:00"
title = "prefix 1.0~rc2-1"
tags = ["PostgreSQL", "debian", "prefix"]
categories = ["Projects","prefix"]
thumbnailImage = "/img/old/debian-logo.png"
thumbnailImagePosition = "left"
coverImage = "/img/old/debian-logo.png"
coverSize = "partial"
coverMeta = "out"
aliases = ["/blog/2009/07/09-prefix-10rc2-1",
           "/blog/2009/07/09-prefix-10rc2-1.html"]
+++

I've been having problem with building both 
`postgresql-8.3-prefix` and
`postgresql-8.4-prefix` debian packages from the same source package, and
fixing the packaging issue forced me into modifying the main 
`prefix`
`Makefile`. So while reaching 
`rc2`, I tried to think about missing pieces easy
to add this late in the game: and there's one, that's a function
`length(prefix_range)`, so that you don't have to cast to text no more in the
following wildspread query:

~~~
SELECT foo, bar
    FROM prefixes
   WHERE prefix @> '012345678'
ORDER BY length(prefix) DESC
   LIMIT 1;
~~~


And here's a simple stupid benchmark of the new function, here in
[prefix-1.0~rc2.tar.gz](http://prefix.projects.postgresql.org/prefix-1.0~rc2.tar.gz). And it'll soon reach debian, if my QA dept agrees (my
[sponsor](http://julien.danjou.info/blog/) is a QA dept all by himself!).

First some preparation:

~~~
dim=#   create table prefixes (
dim(#          prefix    prefix_range primary key,
dim(#          name      text not null,
dim(#          shortname text,
dim(#          status    char default 'S',
dim(# 
dim(#          check( status in ('S', 'R') )
dim(#   );
NOTICE:  CREATE TABLE / PRIMARY KEY will create implicit index "prefixes_pkey" for
 table "prefixes"                                                                
CREATE TABLE
Time: 74,357 ms
dim=#   \copy prefixes from 'prefixes.fr.csv' with delimiter ; csv quote '"'
Time: 200,982 ms
dim=# select count(*) from prefixes ;
 count 
-------
 11966
(1 row)
Time: 3,047 ms
~~~


And now for the micro-benchmark:

~~~
dim=# \o /dev/null
dim=# select length(prefix) from prefixes;
Time: 16,040 ms
dim=# select length(prefix::text) from prefixes;
Time: 23,364 ms
dim=# \o
~~~


Hope you enjoy!

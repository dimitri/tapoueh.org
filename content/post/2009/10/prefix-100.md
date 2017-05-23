+++
date = "2009-10-06T15:56:00.000000+02:00"
title = "prefix 1.0.0"
tags = ["PostgreSQL", "debian", "release", "backports", "pg_staging", "prefix", "pgloader"]
categories = ["Projects","pgloader"]
thumbnailImage = "/img/old/debian-logo.png"
thumbnailImagePosition = "left"
coverImage = "/img/old/debian-logo.png"
coverSize = "partial"
coverMeta = "out"
aliases = ["/blog/2009/10/06-prefix-100",
           "/blog/2009/10/06-prefix-100.html"]
+++

So there it is, at long last, the final 
`1.0.0` release of prefix! It's on its
way into the debian repository (targetting sid, in testing in 10 days) and
available on 
[pgfoundry](http://pgfoundry.org/frs/?group_id=1000352) to.

In order to make it clear that I intend to maintain this version, the number
has 3 digits rather than 2... which is also what 
[PostgreSQL](http://www.postgresql.org/support/versioning) users will
expect.

The only last minute change is that you can now use the first version of the
two following rather than the second one:

~~~
-  create index idx_prefix on prefixes using gist(prefix gist_prefix_range_ops);
+  create index idx_prefix on prefixes using gist(prefix);
~~~


For you information, I'm thinking about leaving 
`pgfoundry` as far as the
source code management goes, because I'd like to be done with 
`CVS`. I'd still
use the release file hosting though at least for now. It's a burden but it's
easier for the users to find them, when they are not using plain 
`apt-get
install`. That move would lead to host 
[prefix](http://pgfoundry.org/projects/prefix/) and 
[pgloader](http://pgfoundry.org/projects/pgloader) and the 
[backports](http://cvs.pgfoundry.org/cgi-bin/cvsweb.cgi/backports/)
over there at 
[github](http://github.com/dimitri), where my next pet project, 
`pg_staging`, will be hosted
too.

The way to see this 
*pgfoundry* leaving is that if everybody does the same,
then migrating the facility to some better or more recent hosting software
will be easier. Maybe some other parts of the system are harder than the
sources to migrate, though. If that's the case I'll consider moving them out
too, maybe getting listed on the 
[PostgreSQL Software Catalogue](http://www.postgresql.org/download/product-categories) will prove
enough as far as web presence goes?

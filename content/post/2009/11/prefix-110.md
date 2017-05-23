+++
date = "2009-11-30T12:10:00.000000+01:00"
title = "prefix 1.1.0"
tags = ["PostgreSQL", "Extensions", "prefix", "9.1"]
categories = ["Projects","prefix"]
thumbnailImage = "/img/postgresql-512.jpg"
thumbnailImagePosition = "left"
coverImage = "/img/postgresql-512.jpg"
coverSize = "partial"
coverMeta = "out"
aliases = ["/blog/2009/11/30-prefix-110",
           "/blog/2009/11/30-prefix-110.html"]
+++

So I had two 
[bug](http://archives.postgresql.org/pgsql-general/2009-11/msg01042.php) 
[reports](http://lists.pgfoundry.org/pipermail/prefix-users/2009-November/000005.html) about 
[prefix](prefix.html) in less than a week. It means several
things, one of them is that my code is getting used in the wild, which is
nice. The other side of the coin is that people do find bugs in there. This
one is about the behavior of the 
`btree opclass` of the type 
`prefix range`. We
cheat a lot there by simply having written one, because a range does not
have a strict ordering: is 
`[1-3]` before of after 
`[2-4]`? But when you know
you have no overlapping intervals in your 
`prefix_range` column, being able to
have it part of a 
*primary key* is damn useful.

Note: in 
`8.5` we should have a way to express 
*contraint exclusion* and have
PostgreSQL forbids overlapping entries for us. Not being there yet, you
could write a 
*constraint trigger* and use the 
*GiST index* to have nice speed
there, which is exactly what this 
*constraint exclusion* support is about.

It turns out the code change required is pretty simple:

~~~
-    return (a->first == b->first) ? (a->last - b->last) : (a->first - b->first);
+    /*
+     * we are comparing e.g. '1' and '12' (the shorter contains the
+     * smaller), so let's pretend '12' < '1' as it contains less elements.
+     */
+    return (alen == mlen) ? 1 : -1;
~~~


This happens in the 
*compare support function* (see
[Interfacing Extensions To Indexes](http://www.postgresql.org/docs/8.4/interactive/xindex.html)) so that means you now have to rebuild
your 
`prefix_range` btree indexes, hence the version number bump.

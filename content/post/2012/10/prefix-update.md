+++
date = "2012-10-16T10:47:00.000000+02:00"
title = "Prefixes and Ranges"
tags = ["PostgreSQL"]
categories = ["PostgreSQL"]
thumbnailImage = "/img/old/Prefix-Pro-Blend.jpg"
thumbnailImagePosition = "left"
coverImage = "/img/old/Prefix-Pro-Blend.jpg"
coverSize = "partial"
coverMeta = "out"
aliases = ["/blog/2012/10/16-prefix-update",
           "/blog/2012/10/16-prefix-update.html"]
+++

It's been a long time since I last had some time to spend on the 
[prefix](http://tapoueh.org/pgsql/prefix.html)
PostgreSQL extension and its 
`prefix_range` data type. With PostgreSQL 9.2
out, some users wanted me to update the extension for that release, and
hinted me that it was high time that I fix that old bug for which I already
had a patch.


## `prefix_range` release 1.2.0

I'm sorry it took that long. It's now done, you can have 
`prefix 1.2.0` from
[https://github.com/dimitri/prefix](https://github.com/dimitri/prefix) or if you want a 
*tagged* tarball then you
can use this link: 
[https://github.com/dimitri/prefix/tarball/v1.2.0](https://github.com/dimitri/prefix/tarball/v1.2.0).

The 
*changelog* is all about fixing an index search bug and updating the
package to primarily be an extension for PostgreSQL 9.1 and 9.2. Of course
older Major Versions are still supported (all of them since 
`8.1`, but please
first consider upgrading PostgreSQL) if you want to install it 
*manually*,
using the 
`prefix--1.2.0.sql` file.


## debian package

And thanks to 
[Christoph Berg](http://www.df7cb.de/) the debian package is already validated and has
reached 
*debian experimental*. We don't target 
*sid* these days because debian
is preparing a new stable release, so there's a freeze. I think. Anyway,
take your prefix package from here if you need it:
[http://packages.debian.org/experimental/postgresql-9.1-prefix](http://packages.debian.org/experimental/postgresql-9.1-prefix).


## Range Types

If you step back a little there's an interesting question to answer here.
Why isn't 
`prefix_range` and 
[PostgreSQL Range Type](http://www.postgresql.org/docs/9.2/static/rangetypes.html)? Given the names it seems
like a pretty good candidate.

Well the thing is that to make a generic range type you need to have a total
ordering on the range elements, and a distance function that tells you how
far any two elements of a range are one from each other.

When talking about prefixes, I don't see how to do that. The prefix range
`['abcd', 'abce')` contains an infinity of elements, all the 
*strings* that
begin with the letters 
`abcd`. I guess that coming with an ordering on text is
possible, but what if any text element represents a prefix?

I mean that in our case, the elements would be of type 
`prefix`, and 
`'abcd'` is
a prefix of 
`'abcdefg'`. The question I want to answer is that given a table
with prefixes 
`'abcd'`, 
`'abce'` and 
`'abcde'` which row in there has the longest
prefix matching the literal 
`'abcdef'`.

I'm not seeing how to abuse the 
*Range Types* mechanism to implement that, so
if you have some ideas please share them!

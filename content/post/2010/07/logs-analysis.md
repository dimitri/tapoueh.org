+++
date = "2010-07-13T14:15:00.000000+02:00"
title = "Logs analysis"
tags = ["PostgreSQL"]
categories = ["PostgreSQL"]
thumbnailImage = "/img/postgresql-512.jpg"
thumbnailImagePosition = "left"
coverImage = "/img/postgresql-512.jpg"
coverSize = "partial"
coverMeta = "out"
aliases = ["/blog/2010/07/13-logs-analysis",
           "/blog/2010/07/13-logs-analysis.html"]
+++

Nowadays to analyze logs and provide insights, the more common tool to use
is 
[pgfouine](http://pgfouine.projects.postgresql.org/), which does an excellent job. But there has been some
improvements in logs capabilities that we're not benefiting from yet, and
I'm thinking about the 
`CSV` log format.

So the idea would be to turn 
*pgfouine* into a set of 
`SQL` queries against the
logs themselves once imported into the database. Wait. What about having our
next PostgreSQL version, which is meant (I believe) to include CSV support
in 
*SQL/MED*, to directly expose its logs as a system view?

A good thing would be to expose that as a ddl-partitioned table following
the log rotation scheme as setup in 
`postgresql.conf`, or maybe given in some
sort of a setup, in order to support 
`logrotate` users. At least some
facilities to do that would be welcome, and I'm not sure plain 
*SQL/MED* is
that when it comes to 
*source* partitioning.

Then all that remains to be done is a set of 
`SQL` queries and some static or
dynamic application to derive reports from there.

This is yet again an idea I have in mind but don't have currently time to
explore myself, so I talk about it here in the hope that others will share
the interest. Of course, now that I work at 
[2ndQuadrant](http://2ndQuadrant.com), you can make it so
that we consider the idea in more details, up to implementing and
contributing it!

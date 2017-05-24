+++
date = "2011-10-12T22:22:00.000000+02:00"
title = "Implementing backups"
tags = ["PostgreSQL", "Backups", "pg_staging", "pgfouine"]
categories = ["Projects","pg_staging"]
thumbnailImage = "/img/old/online-backup.jpg"
thumbnailImagePosition = "left"
coverImage = "/img/old/online-backup.jpg"
coverSize = "partial"
coverMeta = "out"
aliases = ["/blog/2011/10/12-backup-strategy",
           "/blog/2011/10/12-backup-strategy.html"]
+++

I've been asked about my opinion on backup strategy and best practices, and
it so happens that I have some kind of an opinion on the matter.

I tend to think best practice here begins with defining properly the 
*backup
plan* you want to implement. It's quite a complex matter, so be sure to ask
yourself about your needs: what do you want to be protected from?

The two main things to want to protect from are hardware loss (crash
disaster, plane in the data center, fire, water flood, etc) and human error
(
`UPDATE` without a where clause). Replication is an answer to the former,
archiving and dumps to the latter. You generally need both.

Often enough “backups” include 
`WAL` 
*archiving* and 
*shipping* and nightly or
weekly 
*base backups*, with some retention and some scripts or procedures
ready to setup 
[Point In Time Recovery](http://www.postgresql.org/docs/9.1/static/continuous-archiving.html) and recover some data without
interfering with the WAL archiving and shipping. Of course with PostgreSQL
9.0 and 9.1, the 
*WAL Shipping* can be implemented with 
*streaming replication*
and you can even have a 
*Hot Standby*. But for backups you still want
archiving.

Mostly I still implement 
`pg_dump -Fc` nightly backups with a custom retention
(for example, 1 backup a month kept 2 years, 1 backup a week kept 6 or 12
months, 1 backup a night kept 1 to 2 weeks), when the database size allow
the 
`pg_dump` run to remain constrained in the 
*maintenance window*, if any.

Don't forget that while 
`pg_dump` runs, you can't roll out 
*DDL changes* to the
production system any more, so you want to be careful about this
*maintenance window* thing. When you have one.

*Physical backups* are not locking 
*rollouts* away, but they often suck a good
deal of the 
*IO bandwidth* so you need to pick up a right timing to do them.
That's how you can get to once a week base backup and WAL 
*archiving*.

If you can't 
`pg_dump` production, maybe you can have 
*automated restore jobs*
from the 
*physical backups* that you then 
`pg_dump -Fc`, so that you still have
that. That can come up handy, really: you can't test your 
*major upgrade* out
of a 
*physical backup*.

Also, 
***obviously***, never consider your backup strategy implemented until you
have either 
*automated restores* in place or a regular schedule to exercise
them (
*staging instances*, devel instances).

Then as far as the practical tools go, I tend to think that 
[pg_staging](http://tapoueh.org/pgsql/pgstaging.html) is
worth its installation complexity, and for WAL archiving and base backup I
recommend 
[walmgr](http://skytools.projects.postgresql.org/doc/walmgr.html) from 
[Skytools](http://wiki.postgresql.org/wiki/SkyTools), that's a very handy tool. When using
PostgreSQL 
`9.0` or 
`9.1`, consider using 
[walmgr3](http://packages.debian.org/experimental/skytools3-walmgr) so that it's behaving nice
alongside 
*streaming replication*.

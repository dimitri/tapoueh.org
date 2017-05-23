+++
date = "2011-11-16T14:00:00.000000+01:00"
title = "pgbouncer munin plugin"
tags = ["PostgreSQL", "pgbouncer", "munin", "monitoring"]
categories = ["PostgreSQL","pgbouncer"]
thumbnailImage = "/img/old/bouncing_elephant.gif"
thumbnailImagePosition = "left"
coverImage = "/img/old/bouncing_elephant.gif"
coverSize = "partial"
coverMeta = "out"
aliases = ["/blog/2011/11/16-pgbouncer-munin",
           "/blog/2011/11/16-pgbouncer-munin.html"]
+++

It seems that if you search for a 
[munin](http://munin-monitoring.org/) plugin for 
[pgbouncer](http://wiki.postgresql.org/wiki/PgBouncer) it's easy
enough to reach an old page of mine with an old version of my plugin, and a
broken link. Let's remedy that by publishing here the newer version of the
plugin. To be honest, I though it already made its way into the official
munin 
`1.4` set of plugins, but I've not been following closely enough.


As the plugin is 300 lines of python code, it's not a good idea to just
inline it here, so please grab it at 
[pgbouncer_](../../../resources/pgbouncer_).

You might need to know that the script name once installed should follow the
form 
`pgbouncer_dbname_stats_requests` or 
`pgbouncer_dbname_pools`, where of
course 
`dbname` can contain any number of 
`_` characters. This script supports
quite old versions of 
*pgbouncer* that didn't accept the normal 
`pq` protocol,
you did have to use 
`psql` to have any chance of getting the data from a
script, you couldn't then just use a PostgreSQL driver such as 
[psycopg2](http://initd.org/psycopg/).

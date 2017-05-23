+++
date = "2010-11-07T13:45:00.000000+01:00"
title = "pg_basebackup"
tags = ["PostgreSQL", "skytools", "backup"]
categories = ["PostgreSQL","Skytools"]
thumbnailImage = "/img/old/londiste_logo.gif"
thumbnailImagePosition = "left"
coverImage = "/img/old/londiste_logo.gif"
coverSize = "partial"
coverMeta = "out"
aliases = ["/blog/2010/11/07-pg_basebackup",
           "/blog/2010/11/07-pg_basebackup.html"]
+++

[Hannu](http://2ndquadrant.com/about/#krosing) just gave me a good idea in 
[this email](http://archives.postgresql.org/pgsql-hackers/2010-11/msg00236.php) on 
[-hackers](http://archives.postgresql.org/pgsql-hackers/), proposing that
[pg_basebackup](https://github.com/dimitri/pg_basebackup) should get the 
`xlog` files again and again in a loop for the
whole duration of the 
*base backup*. That's now done in the aforementioned
tool, whose options got a little more useful now:

~~~
Usage: pg_basebackup.py [-v] [-f] [-j jobs] "dsn" dest

Options:
  -h, --help            show this help message and exit
  --version             show version and quit
  -x, --pg_xlog         backup the pg_xlog files
  -v, --verbose         be verbose and about processing progress
  -d, --debug           show debug information, including SQL queries
  -f, --force           remove destination directory if it exists
  -j JOBS, --jobs=JOBS  how many helper jobs to launch
  -D DELAY, --delay=DELAY
                        pg_xlog subprocess loop delay, see -x
  -S, --slave           auxilliary process
  --stdin               get list of files to backup from stdin
~~~


Yeah, as implementing the 
`xlog` idea required having some kind of
parallelism, I built on it and the script now has a 
`--jobs` option for you to
setup how many processes to launch in parallel, all fetching some 
`base
backup` files in its own standard (
`libpq`) 
[PostgreSQL](http://www.postgresql.org/) connection, in
compressed chunks of 
`8 MB` (so that's not 
`8 MB` chunks sent over).

The 
`xlog` loop will fetch any 
`WAL` file whose 
`ctime` changed again,
wholesale. It's easier this way, and tools to get optimized behavior already
do exist, either 
[walmgr](http://skytools.projects.postgresql.org/doc/walmgr.html) or 
[walreceiver](http://www.postgresql.org/docs/9.0/interactive/warm-standby.html#STREAMING-REPLICATION).

The script is still a little 
[python](http://python.org/) self-contained short file, it just went
from about 
`100` lines of code to about 
`400` lines. There's no external
dependency, all it needs is provided by a standard python installation. The
problem with that is that it's using 
`select.poll()` that I think is not
available on windows. Supporting every system or adding to the dependencies,
I've been choosing what's easier for me.

~~~
import select
    p = select.poll()
    p.register(sys.stdin, select.POLLIN)
~~~


If you get to try it, please report about it, you should know or easily
discover my 
*email*!

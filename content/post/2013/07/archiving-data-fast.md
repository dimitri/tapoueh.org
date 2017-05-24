+++
date = "2013-07-05T15:30:00.000000+02:00"
title = "Archiving data as fast as possible"
tags = ["PostgreSQL", "Replication", "Catalogs", "YeSQL"]
categories = ["PostgreSQL","YeSQL"]
thumbnailImage = "/img/old/pipeline.png"
thumbnailImagePosition = "left"
coverImage = "/img/old/pipeline.png"
coverSize = "partial"
coverMeta = "out"
aliases = ["/blog/2013/07/05-archiving-data-fast",
           "/blog/2013/07/05-archiving-data-fast.html"]
+++

In a recent article here we've been talking about how do do 
[Batch Updates](/blog/2013/03/15-batch-update) in
a very efficient way, using the 
*Writable CTE* features available in
PostgreSQL 9.1. I sometime read how 
[Common Table Expressions](http://www.postgresql.org/docs/current/interactive/queries-with.html) changed the
life of fellow DBAs and developers, and would say that 
*Writable CTE* are at
least the same boost again.

<center>*Writable CTEs allow to easily implement data processing pipelines*</center>

In the case of archiving data into side tables the pipeline we're talking
about aims to move data out of a table (that's a 
`DELETE`) then store it on
the destination (
*archiving*) table, and that's an 
`INSERT`:

~~~
WITH archive AS (
    DELETE FROM source WHERE ...
 RETURNING s.id
)
INSERT INTO target(id, counter)
     SELECT * FROM archive;
~~~


That's probably the most efficient way to move data around in PostgreSQL
when the source and target tables are within the same database. If not, then
I can't wait to play with the 
[Postgres Foreign Data Wrapper](http://www.postgresql.org/docs/9.3/static/postgres-fdw.html) in 9.3, that
should allow to send the data directly with the same query.

Now, if you have foreign keys to deal with, the trick is to embed all the
related statements in the right ordering into a single transaction... and
that's about it. For archiving data remotely though, it's a little more
complex as we need to control two transactions in the right way™, and that
is a topic for another article later. The transactional model to follow
though is the one implemented in 
[PGQ](/tags/pgq) already, so at least we know how to do
things.

About performances of that simple method, let me say that it's fast enough
that you can actually find yourself pusing loads of 
`WAL` data down to your
[Hot Standby server](http://www.postgresql.org/docs/current/static/hot-standby.html). If the system is already very highly loaded, then a way
to 
*throttle* the impact is needed.

The way to fix that problem that I've been using is to check the 
*lag* in
between the Hot Standby you're interested into and the primary server by
running that query periodically:

~~~
select pg_current_xlog_location() as current, replay_location as replay
  from pg_stat_replication
 where application_name = 'standby-name';
~~~


Be aware that any replication client that you use will show up in the
`pg_stat_replication` view, and that includes 
[pg_basebackup](http://www.postgresql.org/docs/current/static/app-pgbasebackup.html) and
[pg_receivexlog](http://www.postgresql.org/docs/current/static/app-pgreceivexlog.html):

~~~
# select application_name, pg_current_xlog_location(),
         sent_location, replay_location
    from pg_stat_replication;
 application_name | pg_current_xlog_location | sent_location  | replay_location 
------------------+--------------------------+----------------+-----------------
 pg_receivexlog   | 18C85/55DCA900           | 18C85/55DAEC20 | {NULL}
 standby-name     | 18C85/55DCA900           | 18C85/55DCA900 | 18C76/4B327D0
(2 lignes)
~~~


Then in between loops of running the 
`WITH archive AS (DELETE ...) INSERT`
query, when the lag is higher than your arbitrary threshold, just pause
until it's back under control. That part I've implemented with a very simple
buzy loop around the previous query and a 1 second wait.

Now, to make sense of the returned data you can use the function
[pg_xlog_location_diff](http://www.postgresql.org/docs/9.2/static/functions-admin.html#FUNCTIONS-ADMIN-BACKUP-TABLE) as of 9.2. If you're still using 9.1, then you can
replicate its implementation in your client application code, it's simple
enough to do so. Here's a 
[Common Lisp](/tags/common-lisp) version of it:

~~~
(defun pg-xlog-location-diff (loc1 loc2)
  "Compute the difference between WAL locations as WAL bytes.

   Locations are expected with the XLOG position format 163A8/210598E8.
   Computation from PostgreSQL sources of pg_xlog_location_diff as in
   src/backend/access/transam/xlogfuncs.c "
  (flet ((parse-location (pos)
	   (let ((shift #.(expt 2 32)))
	     (destructuring-bind (hi lo)
		 (split-sequence:split-sequence #\/ pos)
	       (+ (* shift (parse-integer hi :radix 16))
		  (parse-integer lo :radix 16))))))
    (- (parse-location loc1) (parse-location loc2))))
~~~


Exercice for the reader: write a PL version of it with your PL of choice.


## Update

Reading that article, 
[Bernd Helmle](http://psoos.blogspot.fr/) tells me that he's already done the
backporting of the 
`pg_xlog_location_diff` function to previous versions of
PostgreSQL, and you can find it at
[https://github.com/credativ/pg_xlog_location_diff](https://github.com/credativ/pg_xlog_location_diff).

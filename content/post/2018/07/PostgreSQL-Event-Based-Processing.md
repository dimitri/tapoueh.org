+++
title = "PostgreSQL Event Based Processing"
date = "2018-07-16T09:27:54+02:00"
tags = ["PostgreSQL","YeSQL","Concurrency","Triggers"]
categories = ["PostgreSQL","YeSQL"]
coverImage = "/img/event_processing.png"
coverMeta = "out"
coverSize = "partial"
thumbnailImage = "/img/c09-iis-event-processing.png"
thumbnailImagePosition = "left"

+++

In the previous article of the series [Modeling for
Concurrency](/blog/2018/07/modeling-for-concurrency/), we saw how to model
your application for highly concurrent activity. It was a follow-up to the
article entitled [PostgreSQL Concurrency: Isolation and
Locking](/blog/2018/07/postgresql-concurrency-isolation-and-locking/), which
was a primer on PostgreSQL isolation and locking properties and behaviors.

Today's article takes us a step further and builds on what we did in the
previous articles in our series. After having had all the characters from
Shakespeare's *A Midsummer Night's Dream* tweet their own lines in our
database in [PostgreSQL Concurrency: Data Modification
Language](/blog/2018/06/PostgreSQL-DML.md), and having had them like and
retweet a lot in [PostgreSQL Concurrency: Isolation and
Locking](/blog/2018/07/postgresql-concurrency-isolation-and-locking/), we
saw how to manage concurrent retweets in an efficient way in [Computing and
Caching](/blog/2018/07/computing-and-caching/).

What we did implement in the previous article is a *cache* system, all with
its necessary **cache invalidation policy**. Sometimes though, the
processing of an *event* needs to happen within the same transaction where
the event is registered in your system. PostgreSQL makes it possible to
maintain a summary table transactionally thanks to its
[trigger](https://www.postgresql.org/docs/current/static/sql-createtrigger.html)
support. Today, we're going to dive in how to maintain a summary table with
triggers, and its impact on concurrency.

<!--more-->
<!--toc-->


## Triggers

When a cache refresh policy of minutes isn't advisable, a common approach is
to implement event-based processing. Most SQL systems, including PostgreSQL,
implement an event-based facility called a *trigger*.

A *trigger* allows registering a procedure to be executed at a specified
timing when an event is produced. The timing can be *before*, *after* or
*instead of*, and the event can be *insert*, *update*, *delete* or
*truncate*. As usual, the PostgreSQL documentation covers the topic in full
details and is available online, in our case now at the manual page for the
command [CREATE
TRIGGER](https://www.postgresql.org/docs/current/static/sql-createtrigger.html).

Many triggers in PostgreSQL are written in the [PL/pgSQL — SQL Procedural
Language](https://www.postgresql.org/docs/current/static/plpgsql.html), so
we also need to read the [PLpgSQL trigger
procedures](https://www.postgresql.org/docs/current/static/plpgsql-trigger.html)
documentation for completeness.

Note that with PostgreSQL, it is possible to write procedures and triggers
in other programming languages. Default PostgreSQL builds include support
for [PL/Tcl](https://www.postgresql.org/docs/current/static/pltcl.html),
[PL/Perl](https://www.postgresql.org/docs/current/static/plperl.html),
[PL/Python](https://www.postgresql.org/docs/current/static/plpython.html)
and of course [C-language
functions](https://www.postgresql.org/docs/current/static/xfunc-c.html).

PostgreSQL extensions for other programming languages are available too,
maintained separately from the PostgreSQL core. You can find
[PL/Java](https://github.com/tada/pljava/wiki),
[PL/v8](https://github.com/plv8/plv8) for Javascript powered by the V8
engine, or [PL/XSLT](https://github.com/petere/plxslt) as we saw in
[PostgreSQL Data Types: XML](/blog/2018/04/postgresql-data-types-xml/). For
even more programming language support, see the [PL
Matrix](https://wiki.postgresql.org/wiki/PL_Matrix) in the PostgreSQL wiki.

Unfortunately, it is not possible to write triggers in plain SQL language,
so we have to write stored procedures to benefit from the PostgreSQL trigger
capabilities.


<hr />

{{< figure class="right"
             src="/img/TAOP_Book_Cover_200x260.png"
            link="https://theartofpostgresql.com" >}}
            
This article is extracted from my book [The Art of
PostgreSQL](https://theartofpostgresql.com), which teaches SQL to developers
so that they may replace thousands of lines of code with very simple
queries. The book has a full chapter about *Data Manipulation and
Concurrency Control* in PostgreSQL, including caching with materialized
views, check it out!

<hr />

## Transactional Event Driven Processing

PostgreSQL triggers call a registered procedure each time one of the
supported events is committed. The execution of the procedure is always
taken as a part of the transaction, so if your procedure fails at runtime
then the transaction is aborted.

A classic example of an event driven processing with a trigger in our
context is to update the counters of *rts* and *favs* each time there's a
related insert in the *tweet.activity* table.

~~~ sql
begin;

create table twcache.daily_counters
 (
   day     date not null primary key,
   rts     bigint,
   de_rts  bigint,
   favs    bigint,
   de_favs bigint
 );

create or replace function twcache.tg_update_daily_counters ()
 returns trigger
 language plpgsql
as $$
declare
begin
      update twcache.daily_counters
         set rts = case when NEW.action = 'rt'
                        then rts + 1
                        else rts
                    end,
             de_rts = case when NEW.action = 'de-rt'
                        then de_rts + 1
                        else de_rts
                    end,
             favs = case when NEW.action = 'fav'
                         then favs + 1
                         else favs
                     end,
             de_favs = case when NEW.action = 'de-fav'
                         then de_favs + 1
                         else de_favs
                     end
       where daily_counters.day = current_date;

  if NOT FOUND
  then
      insert into twcache.daily_counters(day, rts, de_rts, favs, de_favs)
           select current_date,
                  case when NEW.action = 'rt'
                       then 1 else 0
                    end,
                  case when NEW.action = 'de-rt'
                       then 1 else 0
                   end,
                  case when NEW.action = 'fav'
                       then 1 else 0
                   end,
                  case when NEW.action = 'de-fav'
                       then 1 else 0
                   end;
  end if;

  RETURN NULL;
end;
$$;

CREATE TRIGGER update_daily_counters
         AFTER INSERT
            ON tweet.activity
      FOR EACH ROW
       EXECUTE PROCEDURE twcache.tg_update_daily_counters();

insert into tweet.activity(messageid, action)
     values (7, 'rt'),
            (7, 'fav'),
            (7, 'de-fav'),
            (8, 'rt'),
            (8, 'rt'),
            (8, 'rt'),
            (8, 'de-rt'),
            (8, 'rt');

select day, rts, de_rts, favs, de_favs
  from twcache.daily_counters;

rollback;
~~~

Again, we don't really want to have that trigger in our setup, so the
transaction ends with a *ROLLBACK*. It's also a good way to try in-progress
development in *psql* in an interactive fashion, and fix all the bugs and
syntax errors until it all works.

Without this trick, then parts of the script pass and others fail, and you
then have to copy and paste your way around until it's all okay, but then
you're never sure that the whole script will pass from the start again,
because the conditions in which you want to apply have been altered on the
partially successful runs.

Here's the result of running our trigger test script:

~~~ psql
BEGIN
CREATE TABLE
CREATE FUNCTION
CREATE TRIGGER
INSERT 0 8
    day     │ rts │ de_rts │ favs │ de_favs 
════════════╪═════╪════════╪══════╪═════════
 2017-09-21 │   5 │      1 │    1 │       1
(1 row)

ROLLBACK
~~~

The thing is, each time there's a *tweet.activity* inserted this trigger
will transform the *insert* into an *update* against a single row, and the
same target row for a whole day.

This implementation is totally killing any ambitions we might have had about
concurrency and scalability properties of our model, in a single trigger.
Yet it's easy to write such a trigger, so it's seen a lot in the wild.

## Trigger and Counters Anti-Pattern

You might also notice that this triggers is very wrong in its behavior, as
coded. The implementation of the *insert or update* — a.k.a. *upsert* — is
coded in a way to leave the door open to concurrency issues. To understand
those issues, we need to consider what happens when we start a new day:

  1. The first transaction of the day attempts to *update* the daily
     counters table for this day, but finds no records because it's the
     first one.
     
  2. The first transaction of the day then *inserts* the first value for the
     day with ones and zeroes for the counters.
     
  3. The second transaction of the day then executes the *update* to the
     daily counter, finds the existing row, and skips the *insert* part of
     the trigger.

That's the happy scenario where no problem occurs. Now, in the real life,
here's what will sometimes happen. It's not always, mind you, but not never
either. Concurrency bugs — they like to hide in plain sight.

  1. The first transaction of the day attempts to *update* the daily
     counters table for this day but finds no records because it's the first
     one.
     
  2. The second transaction of the day attempts to *update* the daily
     counters table for this day, but finds no records, because the first
     one isn't there yet.
     
  3. The second transaction of the day now proceeds to *insert* the first
     value for the day, because the job wasn't done yet.
     
  4. The first transaction of the day then *inserts* the first value... and
     fails with a *primary key* conflict error because that *insert* has
     already been done. Sorry about that!

There are several ways to address this issue, and the classic one is
documented at [A PL/pgSQL Trigger Procedure For Maintaining A Summary
Table](https://www.postgresql.org/docs/current/static/plpgsql-trigger.html#PLPGSQL-TRIGGER-SUMMARY-EXAMPLE)
example in the PostgreSQL documentation.

The solution there is to *loop* over attempts at *update* then *insert*
until one of those works, ignoring the `UNIQUE_VIOLATION` exceptions in the
process. That allows implementing a fall back when another transaction did
insert a value concurrently, i.e. in the middle of the `NOT FOUND` test and
the consequent *insert*.

Starting in PostgreSQL 9.5 with support for the *on conflict* clause of the
*insert into* command, there's a much better way to address this problem.

## Fixing the Behavior

While it's easy to maintain a *cache* in an event driven fashion thanks to
PostgreSQL and its trigger support, turning an *insert* into an *update*
with contention on a single row is never a good idea. It's even a classic
anti-pattern.

Here's a modern way to fix the problem with the previous trigger
implementation, this time applied to a per-message counter of *retweet* and
*favorite* actions:

~~~ sql
begin;

create table twcache.counters
 (
   messageid  bigint not null references tweet.message(messageid),
   rts        bigint,
   favs       bigint,

   unique(messageid)
 );

create or replace function twcache.tg_update_counters ()
 returns trigger
 language plpgsql
as $$
declare
begin
   insert into twcache.counters(messageid, rts, favs)
        select NEW.messageid,
               case when NEW.action = 'rt' then 1 else 0 end,
               case when NEW.action = 'fav' then 1 else 0 end
   on conflict (messageid)
     do update
           set rts = case when NEW.action = 'rt'
                          then counters.rts + 1

                          when NEW.action = 'de-rt'
                          then counters.rts - 1

                          else counters.rts
                      end,

               favs = case when NEW.action = 'fav'
                           then counters.favs + 1

                           when NEW.action = 'de-fav'
                           then counters.favs - 1

                           else counters.favs
                       end
         where counters.messageid = NEW.messageid;

  RETURN NULL;
end;
$$;

CREATE TRIGGER update_counters
         AFTER INSERT
            ON tweet.activity
      FOR EACH ROW
       EXECUTE PROCEDURE twcache.tg_update_counters();

insert into tweet.activity(messageid, action)
     values (7, 'rt'),
            (7, 'fav'),
            (7, 'de-fav'),
            (8, 'rt'),
            (8, 'rt'),
            (8, 'rt'),
            (8, 'de-rt'),
            (8, 'rt');

select messageid, rts, favs
  from twcache.counters;

rollback;
~~~

And here's the result of running that file in *psql*, either from the
command line with *psql -f* or with the interactive `\i <path/to/file.sql`
command:

~~~ psql
BEGIN
CREATE TABLE
CREATE FUNCTION
CREATE TRIGGER
INSERT 0 8
 messageid │ rts │ favs 
═══════════╪═════╪══════
         7 │   1 │    0
         8 │   3 │    0
(2 rows)

ROLLBACK
~~~

You might have noticed that the file ends with a *ROLLBACK* statement.
That's because we don't really want to install such a trigger, it's meant as
an example only.

The reason why we don't actually want to install it is that it would cancel
all our previous efforts to model for tweet activity scalability by
transforming every *insert into tweet.activity* into an *update
twcache.counters* on the same *messageid*. We looked into that exact thing
in the previous article [Computing and
Caching](/blog/2018/07/computing-and-caching/) and we saw that it would
never scale to our requirements.

## Conclusion

PostgreSQL triggers are a powerful tool that allows extra business logic to
happen right in the transaction that registers events in your system. Deeper
thoughts about this complex topic are available in my article [SQL and
Business Logic](https://tapoueh.org/blog/2017/06/sql-and-business-logic/).

When using PostgreSQL triggers, concurrency behavior of your implementation
must be analyzed. It's quite easy to deploy a trigger to your production
system and implement simple and complex processing in a transactional way,
and it's incredibly easy to kill your concurrency scaling while doing that,
as seen here.

In our next article in the series, we will learn how to implement Event
Processing in PostgreSQL with
[LISTEN](https://www.postgresql.org/docs/current/static/sql-listen.html) and
[NOTIFY](https://www.postgresql.org/docs/current/static/sql-notify.html) and
a complete client example, in Go.


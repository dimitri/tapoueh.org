+++
title = "PostgreSQL Concurrency: Isolation and Locking"
date = "2018-07-03T13:30:13+02:00"
tags = ["PostgreSQL","YeSQL","Concurrency","Isolation","Locking"]
categories = ["PostgreSQL","YeSQL"]
coverImage = "/img/lockout_tagout_device1.jpg"
coverMeta = "in"
coverSize = "partial"
thumbnailImage = "/img/lock-logo.png"
thumbnailImagePosition = "left"

+++

[PostgreSQL](https://www.postgresql.org) is a relational database management
system. It's even the world's most advanced open source one of them. As
such, as its core, Postgres solves concurrent access to a set of data and
maintains consistency while allowing concurrent operations.

This article is a primer on PostgreSQL Isolation and Locking properties and
behaviors. You might be interested into the previous article in the series:
[PostgreSQL Concurrency: Data Modification
Language](/blog/2018/06/postgresql-concurrency-data-modification-language/).

<!--more-->
<!--toc-->

## Isolation and Locking

The main feature of any database system is its implementation of concurrency
and full respect of the system's constraints and properties when multiple
transactions are modifying the state of the system at the same time.

PostgreSQL is fully *ACID* compliant and implements *transactions isolation*
so that your application's concurrency can be dealt with gracefully.
Concurrency is a tricky and complex problem, and concurrency issues are
often hard to reproduce. That's why it's best to rely on existing solutions
for handling concurrency rather than rolling your own.

Dealing with concurrency issues in programming languages usually involves
proper declaration and use of *lock*, *mutex*, and *semaphore* facilities
which make a clever use of *atomic* operations as supported by your CPU, and
sometimes provided by the operating system. Some programming languages such
as Java offer *synchronized* blocks that in turn make use of previously
listed low-level features. Other programming languages such as Erlang only
implement *message passing* facilities, and handle concurrency internally
(in a *mailbox* system) so that you don't have to.

SQL is a declarative programming language, where our role as developers is
to declare our intention: the result we want to achieve. The implementation
is then tasked with implementing our command and making it right in every
detail, including concurrency behavior.

PostgreSQL implementation of the concurrency behavior is dependable and
allows some user control in terms of locking aspects of your queries.

<hr />

{{< figure class="right"
             src="/img/MasteringPostgreSQLinAppDev-Cover-th.png"
            link="https://masteringpostgresql.com" >}}
            
This article is extracted from my book [Mastering PostgreSQL in Application
Development](https://masteringpostgresql.com), which teaches SQL to
developers so that they may replace thousands of lines of code with very
simple queries. The book has a full chapter about *Data Manipulation and
Concurrency Control* in PostgreSQL, including caching with materialized
views, check it out!

<hr />

## Transactions and Isolation

Given the *ACID* properties, a transaction must be *Isolated* from other
concurrent transactions running in the system. It is possible to choose the
level of isolation from the concurrent activity, depending on your use case.

A simple use case for isolation is *online backups*. The backup application
for PostgreSQL is *pg_dump*, and the role of this application is to take a
snapshot of your whole database and export it to a backup file. This
requires that *pg_dump* reads are completely isolated from any concurrent
write activity in the system, and this is obtained with the isolation level
*repeatable read* or *serializable* as described next.

From PostgreSQL version 9.1 onward, *pg_dump* uses the isolation level
*serializable*. It used to be *repeatable read* until SSI implementation…
more on that later.

[Transaction
isolation](https://www.postgresql.org/docs/current/static/transaction-iso.html)
is defined by the SQL standard and implemented in PostgreSQL:

> The SQL standard defines four levels of transaction isolation. The most
> strict is Serializable, which is defined by the standard in a paragraph
> which says that any concurrent execution of a set of Serializable
> transactions is guaranteed to produce the same effect as running them one
> at a time in some order. The other three levels are defined in terms of
> phenomena, resulting from interaction between concurrent transactions,
> which must not occur at each level. The standard notes that due to the
> definition of Serializable, none of these phenomena are possible at that
> level. (This is hardly surprising -- if the effect of the transactions
> must be consistent with having been run one at a time, how could you see
> any phenomena caused by interactions?)

Still quoting the PostgreSQL documentation, here are the phenomena which are
prohibited at various levels are:

  - Dirty read

    A transaction reads data written by a concurrent uncommitted
    transaction.

  - Nonrepeatable read

    A transaction re-reads data it has previously read and finds that data
    has been modified by another transaction (that committed since the
    initial read).

  - Phantom read

    A transaction re-executes a query returning a set of rows that satisfy a
    search condition and finds that the set of rows satisfying the condition
    has changed due to another recently committed transaction.

  - Serialization anomaly

    The result of successfully committing a group of transactions is
    inconsistent with all possible orderings of running those transactions
    one at a time.

There are four isolation levels defined by the standard: *read uncommitted*,
*read committed*, *repeatable read*, and *serializable*. PostgreSQL doesn't
implement *read uncommitted*, which allows *dirty reads*, and instead
defaults to *read committed*.

The definition of those isolation levels says that *read committed*
disallows *dirty read* anomalies, *repeatable read* disallows *dirty read*
and *nonrepeatable read*, and *serializable* disallows all anomalies.

PostgreSQL also disallows *phantom read* from *repeatable read* isolation
level.

## About SSI

PostgreSQL's implementation of *serializable* is an amazing work. It is
described in details at the PostgreSQL wiki page entitled
[Serializable](https://wiki.postgresql.org/wiki/Serializable), and the wiki
page [SSI](https://wiki.postgresql.org/wiki/SSI) contains more details about
how to use it.

It took about 20 years for the research community to come up with a
satisfying mathematical model for implementing *serializable snapshot
isolation* in an efficient way, and then a single year for that major
progress to be included in PostgreSQL!

## Concurrent Updates and Isolation

In our *tweet* model of an application, we can have a look at handling
*retweets*, which is a *counter* field in the *tweet.message* table. 

{{< alert success >}}

The application model for Tweeting is introduced in the first article of
this series:[PostgreSQL Concurrency: Data Modification
Language](/blog/2018/06/postgresql-concurrency-data-modification-language/).

{{< /alert  >}}

Here's how to make a *retweet* in our model:

~~~ sql
update tweet.message
   set rts = rts + 1
 where messageid = 1;
~~~

Now, what happens if two users are doing that at the same time?

To better understand what *at the same time* means here, we can write the
query extended with manual transaction control, as PostgreSQL will do when
sent a single command without an explicit transaction:

~~~ sql
begin;

   update tweet.message
      set rts = rts + 1
    where messageid = 1;
returning messageid, rts;

commit;
~~~

Now, rather than doing this query, we open a *psql* prompt and send in:

~~~ sql
begin;

   update tweet.message
      set rts = rts + 1
    where messageid = 1
returning messageid, rts;
~~~

We get the following result now:

~~~ psql
 messageid │ rts 
═══════════╪═════
         1 │   2
(1 row)
~~~

The transaction remains open (it's *idle in transaction*) and waits for us
to do something else, or maybe *commit* or *rollback* the transaction.

Now, open a second *psql* prompt and send in the exact same query. This time
the *update* doesn't return. There's no way it could: the first transaction
is not done yet and is working on the row where *messageid = 1*. Until the
first transaction is done, no concurrent activity can take place on this
specific row.

So we go back to the first prompt and *commit*.

Then what happens depends on the *isolation level* required. Here we have
the default isolation level *read committed*, and at the second prompt the
*update* command is unlocked and proceeds to immediately return:

~~~ psql
 messageid │ rts 
═══════════╪═════
         1 │   3
(1 row)
~~~

Now for the following examples, we need to review our *psql* setting for the
*ON_ERROR_ROLLBACK* feature. When set to *true* or *interactive*, then
*psql* issues
[savepoints](https://www.postgresql.org/docs/current/static/sql-rollback-to.html)
to protect each outer transaction state, and that will hide what we're
showing next. Type the following command to momentarily disable this helpful
setting, so that we can see what really happens:

~~~
\set ON_ERROR_ROLLBACK off
~~~

If we pick the isolation level *repeatable read*, with the following syntax:

~~~ sql
start transaction isolation level repeatable read;

   update tweet.message
      set rts = rts + 1
    where messageid = 1
returning messageid, rts;
~~~

Again, we leave the transaction open, switch to a second prompt and do the
same thing, and only then — while the second update is waiting for the first
transaction to finish — commit the first transactions. What we get this time
is this:

~~~ psql
ERROR:  could not serialize access due to concurrent update

yesql!# commit;
ROLLBACK
~~~

Also notice that even if we ask for a *COMMIT*, what we get is a *ROLLBACK*.
Once an error occurs in a transaction, in PostgreSQL, the transaction can't
commit anymore.

When using the isolation level *serializable*, the same behavior as for
*repeatable read* is observed, with exactly the same error message exactly.

## Conclusion

PostgreSQL is an advanced RDBMS, with a solid foundation. The main service
PostgreSQL implements to its users is a correct handling of concurrent
operations. In the SQL model, that means handling both transactions and
isolation levels.

The next article in this series is going to show how to model your database
schema to make it a better fit for high concurrent updates, stay tuned!

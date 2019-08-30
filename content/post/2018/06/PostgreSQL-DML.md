+++
title = "PostgreSQL Concurrency: Data Modification Language"
date = "2018-06-25T09:58:53+02:00"
tags = ["PostgreSQL","YeSQL","Concurrency","INSERT","UPDATE","DELETE","CRUD"]
categories = ["PostgreSQL","YeSQL"]
coverImage = "/img/crud.png"
coverMeta = "out"
coverSize = "partial"
thumbnailImage = "/img/sql-dml.png"
thumbnailImagePosition = "left"

+++

[PostgreSQL](https://www.postgresql.org) is a relational database management
system. It's even the world's most advanced open source one of them. As
such, as its core, Postgres solves concurrent access to a set of data and
maintains consistency while allowing concurrent operations.

Postgres exposes its concurrency APIs in the SQL language, in particular in
the [DML](https://en.wikipedia.org/wiki/Data_manipulation_language) parts of
it: you can read the [Data Manipulation
Language](https://www.postgresql.org/docs/current/static/dml.html) chapter
of the PostgreSQL docs for all the details.

In this article we're going to create a set of data and use several of the
basic DML forms available in PostgreSQL to do so. This data will then be
used through a series of follow-up articles about concurrency in PostgreSQL.
In this first article, we approach the basics: SQL commands and how to make
them safe to concurrent behavior in your application.

## A database model

The application we're going to model is a Twitter like application, where we
have users and they can send short messages to the void. They can also
follow other users, and then will see what those other users sent to the
void.

We'll begin with something simple:

~~~ sql
create schema if not exists tweet;

create table tweet.users
 (
   userid     bigserial primary key,
   uname      text not null,
   nickname   text,
   bio        text,
   picture    text,

   unique(uname)
 );

create table tweet.follower
 (
   follower   bigint not null references tweet.users(userid),
   following  bigint not null references tweet.users(userid),

   primary key(follower, following)
 );

create table tweet.message
 (
   messageid  bigserial primary key,
   userid     bigint not null references tweet.users(userid),
   datetime   timestamptz not null default now(),
   message    text not null,
   favs       bigint,
   rts        bigint,
   location   point,
   lang       text,
   url        text
 );
~~~

## Insert Into

Given our model of tweets, the first thing we need are users. Here's how to
create our first users:

~~~ sql
insert into tweet.users (userid, uname, nickname, bio)
     values (default, 'Theseus', 'Duke Theseus', 'Duke of Athens.');
~~~

The SQL standard *values* clause is usable anywhere *select* is expected, as
we saw already in our truth tables in the [introduction to PostgreSQL Data
Types](/blog/2018/04/postgresql-data-types-an-intro/) article. Also,
[values](https://www.postgresql.org/docs/current/static/sql-values.html)
accepts several rows at a time.

~~~ sql
insert into tweet.users (uname, bio)
     values ('Egeus', 'father to #Hermia.'),
            ('Lysander', 'in love with #Hermia.'),
            ('Demetrius', 'in love with #Hermia.'),
            ('Philostrate', 'master of the revels to Theseus.'),
            ('Peter Quince', 'a carpenter.'),
            ('Snug', 'a joiner.'),
            ('Nick Bottom', 'a weaver.'),
            ('Francis Flute', 'a bellows-mender.'),
            ('Tom Snout', 'a tinker.'),
            ('Robin Starveling', 'a tailor.'),
            ('Hippolyta', 'queen of the Amazons, betrothed to Theseus.'),
            ('Hermia', 'daughter to Egeus, in love with Lysander.'),
            ('Helena', 'in love with Demetrius.'),
            ('Oberon', 'king of the fairies.'),
            ('Titania', 'queen of the fairies.'),
            ('Puck', 'or Robin Goodfellow.'),
            ('Peaseblossom', 'Team #Fairies'),
            ('Cobweb', 'Team #Fairies'),
            ('Moth', 'Team #Fairies'),
            ('Mustardseed', 'Team #Fairies'),
            ('All', 'Everyone speaking at the same time'),
            ('Fairy', 'One of them #Fairies'),
            ('Prologue', 'a play within a play'),
            ('Wall', 'a play within a play'),
            ('Pyramus', 'a play within a play'),
            ('Thisbe', 'a play within a play'),
            ('Lion', 'a play within a play'),
            ('Moonshine', 'a play within a play');
~~~

If you have lots of rows to insert into your database, consider using the
[copy](https://www.postgresql.org/docs/current/static/sql-copy.html) command
instead of doing a series of *inserts*. If for some reason you can't use
*copy*, for performance reasons, consider using a single transaction doing
several *insert* statements each with many *values*.

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

## Insert Into ... Select

The *insert* statement can also use a query as a data source. We could, for
instance, fill in our *tweet.follower* table with people that are known to
love each other from their *bio* field; and also we should have the fairies
follow their queen and king, maybe.

First, we need to take this data apart from the previously inserted fields,
which is our data source here.

~~~ sql
select users.userid as follower,
       users.uname,
       f.userid as following,
       f.uname
  from      tweet.users
       join tweet.users f
         on f.uname = substring(users.bio from 'in love with #?(.*).')
 where users.bio ~ 'in love with';
~~~

The *substring* expression here returns only the regular expression matching
group, which happens to be the name of who our user loves. The query then
gives us the following result, which looks about right:

~~~ psql
 follower │   uname   │ following │   uname   
══════════╪═══════════╪═══════════╪═══════════
        3 │ Lysander  │        13 │ Hermia
        4 │ Demetrius │        13 │ Hermia
       13 │ Hermia    │         3 │ Lysander
       14 │ Helena    │         4 │ Demetrius
(4 rows)
~~~

Now, we want to insert the *follower* and *following* data into the
*tweet.follower* table of course. As the *insert into* command knows how to
read its input from the result of a *select* statement, it's pretty easy to
do:

~~~ sql
insert into tweet.follower
     select users.userid as follower,
            f.userid as following
       from      tweet.users
            join tweet.users f
              on f.uname = substring(users.bio from 'in love with #?(.*).')
      where users.bio ~ 'in love with';
~~~

Now about those fairies following their queen and king:

~~~ sql
with fairies as
(
  select userid
    from tweet.users
   where bio ~ '#Fairies'
)
insert into tweet.follower(follower, following)
     select fairies.userid as follower,
            users.userid as following
       from fairies cross join tweet.users
      where users.bio ~ 'of the fairies';
~~~

This time we even have the opportunity to use a *cross join* as we want to
produce all the different combinations of a *fairy* with their royal
subjects.

Here's what we have set-up in terms of followers now:

~~~ sql
select follower.uname as follower,
       follower.bio as "follower's bio",
       following.uname as following
       
  from tweet.follower as follows
  
       join tweet.users as follower
         on follows.follower = follower.userid
         
       join tweet.users as following
         on follows.following = following.userid;
~~~

And here's what we've setup:

~~~ psql
   follower   │              follower's bio               │ following 
══════════════╪═══════════════════════════════════════════╪═══════════
 Hermia       │ daughter to Egeus, in love with Lysander. │ Lysander
 Helena       │ in love with Demetrius.                   │ Demetrius
 Demetrius    │ in love with #Hermia.                     │ Hermia
 Lysander     │ in love with #Hermia.                     │ Hermia
 Peaseblossom │ Team #Fairies                             │ Oberon
 Cobweb       │ Team #Fairies                             │ Oberon
 Moth         │ Team #Fairies                             │ Oberon
 Mustardseed  │ Team #Fairies                             │ Oberon
 Peaseblossom │ Team #Fairies                             │ Titania
 Cobweb       │ Team #Fairies                             │ Titania
 Moth         │ Team #Fairies                             │ Titania
 Mustardseed  │ Team #Fairies                             │ Titania
(12 rows)
~~~

The support for *select* as a source of data for the *insert* statement is
the way to implement *joins* for this command.

The *insert into* clause also accepts a conflict resolution clause with the
*on conflict* syntax, which is very powerful, and that we address later in
our [PostgreSQL Concurrency](/tags/concurrency/) series of articles.

## Update

The SQL *update* statement is used to replace existing values in the
database. Its most important aspect lies in its concurrency behavior, as it
allows replacing existing values while other users are concurrently working
with the database.

In PostgreSQL, all the concurrency feature are based on
[MVCC](https://www.postgresql.org/docs/current/static/mvcc.html), and in the
case of the *update* statement it means that internally PostgreSQL is doing
both an *insert* of the new data and a *delete* of the old one. PostgreSQL
system columns *xmin* and *xmax* allow visibility tracking of the rows so
that concurrent statement have a consistent snapshot of the server's data
set at all times.

As row locking is done per-tuple in PostgreSQL, an *update* statement only
ever blocks another *update*, *delete* or *select for update* statement that
targets the same row(s).

We created some users without a *nickname* before, and maybe it's time to
remedy that, by assigning them their *uname* as a *nickname* for now.

~~~ sql
begin;

   update tweet.users
      set nickname = 'Robin Goodfellow'
    where userid = 17 and uname = 'Puck'
returning users.*;

commit;
~~~

Here we pick the id 17 from the table after a manual lookup. The idea is to
show how to update fields in a single tuple from a *primary key* lookup. In
a lot of cases, our application's code has fetched the *id* previously and
injects it in the update query in much the same way as this.

And thanks to the *returning* clause, we get to see what we've done:

~~~
 userid │ uname │     nickname     │         bio          │ picture 
════════╪═══════╪══════════════════╪══════════════════════╪═════════
     17 │ Puck  │ Robin Goodfellow │ or Robin Goodfellow. │ ¤
(1 row)
~~~

As you can see in the previous query not only we used the *primary key*
field, but as it is a synthetic key, we also added the real value we are
interested into. Should we have pasted the information wrong, the *update*
would find no matching rows and affect zero tuples.

Now there's another use case for that double check: concurrency. We know
that the *Robin Goodfellow* nickname applies to *Puck*. What if someone did
*update* the *uname* of *Puck* while we were running our update statement?
With that double check, we know exactly one of the following is true:

  - Either the other statement came in first and the name is no longear
    *Puck* and we updated no rows.
    
  - The other statement will come later and we did update a row that we know
    is userid 17 and named *Puck*.

Think about that trick when dealing with concurrency in your application's
code, and even more when you're fixing up some data from the console for a
one-off fix. Then always use an explicit transaction block so that you can
check what happened and issue a *rollback;* when it's not what you thought.

We can also *update* several rows at the same time. Say we want to add a
default nickname to all those characters:

~~~ sql
   update tweet.users
      set nickname = case when uname ~ ' '
                          then substring(uname from '[^ ]* (.*)')
                          else uname
                      end
    where nickname is null
returning users.*;
~~~

And now everyone is assigned a proper nickname, computed from their username
with the easy and practical trick you can see in the query. The main thing
to remember in that query is that you can use existing data in your *UPDATE*
statement.

Now, who are our Twitter users?

~~~ sql
  select uname, nickname, bio
    from tweet.users
order by userid;
~~~

It's a bunch of folks you might have heard about before. I've taken the
names and biographies from the [A Midsummer Night's
Dream](https://en.wikipedia.org/wiki/A_Midsummer_Night%27s_Dream#Characters)
play from Shakespeare, for which there's a full XML transcript available at
[Shakespeare
2.00](http://research.cs.wisc.edu/niagara/data/shakes/shaksper.htm) thanks
to ***Jon Bosak***.

~~~ psql
      uname       │     nickname     │                     bio                     
══════════════════╪══════════════════╪═════════════════════════════════════════════
 Theseus          │ Duke Theseus     │ Duke of Athens.
 Egeus            │ Egeus            │ father to #Hermia.
 Lysander         │ Lysander         │ in love with #Hermia.
 Demetrius        │ Demetrius        │ in love with #Hermia.
 Philostrate      │ Philostrate      │ master of the revels to Theseus.
 Peter Quince     │ Quince           │ a carpenter.
 Snug             │ Snug             │ a joiner.
 Nick Bottom      │ Bottom           │ a weaver.
 Francis Flute    │ Flute            │ a bellows-mender.
 Tom Snout        │ Snout            │ a tinker.
 Robin Starveling │ Starveling       │ a tailor.
 Hippolyta        │ Hippolyta        │ queen of the Amazons, betrothed to Theseus.
 Hermia           │ Hermia           │ daughter to Egeus, in love with Lysander.
 Helena           │ Helena           │ in love with Demetrius.
 Oberon           │ Oberon           │ king of the fairies.
 Titania          │ Titania          │ queen of the fairies.
 Puck             │ Robin Goodfellow │ or Robin Goodfellow.
 Peaseblossom     │ Peaseblossom     │ Team #Fairies
 Cobweb           │ Cobweb           │ Team #Fairies
 Moth             │ Moth             │ Team #Fairies
 Mustardseed      │ Mustardseed      │ Team #Fairies
 All              │ All              │ Everyone speaking at the same time
 Fairy            │ Fairy            │ One of them #Fairies
 Prologue         │ Prologue         │ a play within a play
 Wall             │ Wall             │ a play within a play
 Pyramus          │ Pyramus          │ a play within a play
 Thisbe           │ Thisbe           │ a play within a play
 Lion             │ Lion             │ a play within a play
 Moonshine        │ Moonshine        │ a play within a play
(29 rows)
~~~

## Inserting Some Tweets

Now that we have created a bunch of users from *A Midsummer Night's Dream*,
it is time to have them tweet. The full XML transcript available at
[Shakespeare
2.00](http://research.cs.wisc.edu/niagara/data/shakes/shaksper.htm) contains
not only the list of persona but also the full text of the play. They are
all speakers and they all have lines. That's a good content for tweets!

Here's what the transcript looks like:

~~~ xml
<PLAYSUBT>A MIDSUMMER NIGHT'S DREAM</PLAYSUBT>

<ACT><TITLE>ACT I</TITLE>

<SCENE><TITLE>SCENE I.  Athens. The palace of THESEUS.</TITLE>
<STAGEDIR>Enter THESEUS, HIPPOLYTA, PHILOSTRATE, and
Attendants</STAGEDIR>

<SPEECH>
<SPEAKER>THESEUS</SPEAKER>
<LINE>Now, fair Hippolyta, our nuptial hour</LINE>
<LINE>Draws on apace; four happy days bring in</LINE>
<LINE>Another moon: but, O, methinks, how slow</LINE>
<LINE>This old moon wanes! she lingers my desires,</LINE>
<LINE>Like to a step-dame or a dowager</LINE>
<LINE>Long withering out a young man revenue.</LINE>
</SPEECH>

<SPEECH>
<SPEAKER>HIPPOLYTA</SPEAKER>
<LINE>Four days will quickly steep themselves in night;</LINE>
<LINE>Four nights will quickly dream away the time;</LINE>
<LINE>And then the moon, like to a silver bow</LINE>
<LINE>New-bent in heaven, shall behold the night</LINE>
<LINE>Of our solemnities.</LINE>
</SPEECH>
~~~

To have the characters of the play tweet their lines, we write a simple XML
parser for the format and use the *insert* SQL command. Extracted from the
code used to insert the data, here's the *insert* query:

~~~ sql
insert into tweet.message(userid, message)
     select userid, $2
       from tweet.users
      where users.uname = $1 or users.nickname = $1
~~~

As the play's text uses names such as `<SPEAKER>QUINCE</SPEAKER>` and we
inserted the real name into our database, we match the play's XML content
against either the *uname* or the *nickname* field.

Now that the data is loaded, we can have a look at the beginning of the play
in SQL.

~~~ sql
  select uname, message
    from tweet.message
         left join tweet.users using(userid)
order by messageid limit 4;
~~~

And yes, we can now see Shakespeare tweeting:

~~~
   uname   │                      message                      
═══════════╪═══════════════════════════════════════════════════
 Theseus   │ Now, fair Hippolyta, our nuptial hour            ↵
           │ Draws on apace; four happy days bring in         ↵
           │ Another moon: but, O, methinks, how slow         ↵
           │ This old moon wanes! she lingers my desires,     ↵
           │ Like to a step-dame or a dowager                 ↵
           │ Long withering out a young man revenue.
 Hippolyta │ Four days will quickly steep themselves in night;↵
           │ Four nights will quickly dream away the time;    ↵
           │ And then the moon, like to a silver bow          ↵
           │ New-bent in heaven, shall behold the night       ↵
           │ Of our solemnities.
 Theseus   │ Go, Philostrate,                                 ↵
           │ Stir up the Athenian youth to merriments;        ↵
           │ Awake the pert and nimble spirit of mirth;       ↵
           │ Turn melancholy forth to funerals;               ↵
           │ The pale companion is not for our pomp.          ↵
           │ Hippolyta, I woo'd thee with my sword,           ↵
           │ And won thy love, doing thee injuries;           ↵
           │ But I will wed thee in another key,              ↵
           │ With pomp, with triumph and with revelling.
 Egeus     │ Happy be Theseus, our renowned duke!
(4 rows)
~~~

## Delete

The *delete* statement allows marking tuples for removal. Given PostgreSQL's
implementation of
[MVCC](https://www.postgresql.org/docs/current/static/mvcc.html), it would
not be wise to remove the tuple from disk at the time of the *delete*:

  - First, the transaction might *rollback* and we don't know that yet.

  - Second, other concurrent transactions only get to see the *delete* after
    *commit*, not as soon as the statement is done.

As with the *update* statement the most important part of the *delete*
statement has to do with concurrency. Again, the main reason why we use a
RDBMS is so that we don't have to solve the concurrency problems in our
application's code, where instead we can focus on delivering an improved
user experience.

The actual removal of on-disk tuples happens with *vacuum*, which the system
runs in the background for you automatically thanks to its [autovacuum
daemon](https://www.postgresql.org/docs/current/static/routine-vacuuming.html#AUTOVACUUM).
PostgreSQL might also re-use the on-disk space for an *insert* statement as
soon as the tuple isn't visible for any transaction anymore.

Say we mistakenly added characters from another play, and we don't want to
have to deal with them. First, inserting them:

~~~ sql
insert into tweet.users (uname, bio)
     values ('CLAUDIUS', 'king of Denmark.'),
            ('HAMLET', 'son to the late, and nephew to the present king'),
            ('POLONIUS', 'lord chamberlain.'),
            ('HORATIO', 'friend to Hamlet'),
            ('LAERTES', 'son to Polonius'),
            ('LUCIANUS', 'nephew to the king');
~~~

The *delete* syntax is quite simple:

~~~ sql
begin;

   delete
     from tweet.users
    where userid = 22 and uname = 'CLAUDIUS'
returning *;

commit;
~~~

And as usual thanks to the *returning* clause, we know exactly what we just
marked for deletion:

~~~ psql
 userid │  uname   │ nickname │       bio        │ picture 
════════╪══════════╪══════════╪══════════════════╪═════════
     22 │ CLAUDIUS │ ¤        │ king of Denmark. │ ¤
(1 row)
~~~

Now we can also *delete* more than one row with the same command — it all
depends on what we match. As the new characters inserted by mistake didn't
have a part in the play we inserted our messages from, then we can use an
*anti-join* to delete them based on that information:

~~~ sql
begin;

with deleted_rows as
(
    delete
      from tweet.users
     where not exists
           (
             select 1
               from tweet.message
              where userid = users.userid
           )
 returning *
)
select min(userid), max(userid),
       count(*),
       array_agg(uname)
  from deleted_rows;

commit;
~~~

And as expected we get a nice summary output of exactly what we did. This
should now be your default syntax for any *delete* you have to run
interactively in any database, right?

~~~ psql
 min │ max │ count │                 array_agg                  
═════╪═════╪═══════╪════════════════════════════════════════════
  41 │  45 │     5 │ {HAMLET,POLONIUS,HORATIO,LAERTES,LUCIANUS}
(1 row)
~~~

It is also possible to use a *join condition* when deleting rows. It is
written *using* and covered in the PostgreSQL documentation about the
[delete](https://www.postgresql.org/docs/9.6/static/sql-delete.html)
command.

## Tuples and Rows

In this chapter, we've been mentioning *tuples* and *rows* at different
times. There's a difference between the two: a single *row* might exist
on-disk as more than one *tuple* at any time, with only one of them visible
to any single transaction.

The transaction doing an *update* now sees the new version of the *row*, the
new *tuple* just inserted on-disk. As long as this transaction has yet to
*commit* then the rest of the world still sees the previous version of the
*row*, which is another *tuple* on-disk.

While in some contexts *tuples* and *rows* are equivalent, in this chapter
about DML we must be careful to use them in the right context.

## Deleting All the Rows: Truncate

PostgreSQL adds to the *DML* statements the *truncate* command. Internally,
it is considered to be a *DDL* rather than a *DML*. It is a very efficient
way to purge a table of all of its content at once, as it doesn't follow the
per-tuple MVCC system and will simply remove the data files on disk.

Note that the *truncate* command is still MVCC compliant:

~~~ sql
select count(*) from foo;

begin;
truncate foo;
rollback;

select count(*) from foo;
~~~

Assuming there's no concurrent activity on your system when running the
commands, both the counting queries naturally return the same number.

## Delete but Keep a Few Rows

When cleaning up a data set, it may happen that you want to remove most of
the content of a table. It could be a logs table, an audit trail that has
expired or something like that. As we saw earlier when using PostgreSQL,
*delete* marks the tuples as not being visible anymore and then *vacuum*
does the heavy lifting in the background. It is then more efficient to
create a table containing only the new rows and swap it with the old table:

~~~ sql
begin;

create table new_name (like name including all);

insert into new_name
     select <column list>
       from name
      where <restrictions>;
      
 drop table name;
alter table new_name rename to name;

commit;
~~~

In the general case, as soon as you remove *most* entries from your table,
this method is going to be more efficient. The trouble with that method is
the level of locking required to run the *drop table* and the *alter table*
statements.

Those *DDL* require an *access exclusive lock* and will block any read and
write traffic to both tables while they run. If you don't have slow hours or
even off-hours, then it might not be feasible for you to use this trick.

The good thing about *delete* and *vacuum* is that they can run in the
middle of about any concurrent traffic of course.

## Conclusion

PostgreSQL is and advanced RDBMS which provides plenty fancy data processing
options. Its core service is handling concurrent editing of the data, and as
a user the way to edit data is by using [Data Manipulation
Language](https://www.postgresql.org/docs/current/static/dml.html). In
follow-up articles we'll get into more details about how to deal with
concurrency.

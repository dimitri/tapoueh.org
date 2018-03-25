+++
title = "Database Modelization Anti-Patterns"
date = "2018-03-08T18:00:43+01:00"
tags = ["PostgreSQL","YeSQL","Modelisation","Anti-Patterns"]
categories = ["PostgreSQL","YeSQL"]
coverImage = "/img/dbmodel.jpg"
coverMeta = "in"
coverSize = "partial"
thumbnailImage = "/img/dbschema.svg"
thumbnailImagePosition = "left"

+++

Next week we see two awesome PostgreSQL conferences in Europe, back to back,
with a day in between just so that people may attend both! In chronological
order we have first [Nordic pgDay](https://2018.nordicpgday.org) in Oslo
where I will have the pleasure to talk about [Data Modeling, Normalization
and
Denormalization](https://www.postgresql.eu/events/nordicpgday2018/schedule/session/1896-data-modeling-normalization-and-denormalization/).
Then we have [pgday.paris](https://2018.pgday.paris) with an awesome
schedule and a strong focus on the needs of application developers!

<!--more-->

{{< figure class="right"
             src="/img/MasteringPostgreSQLinAppDev-Cover-th.png"
            link="https://masteringpostgresql.com" >}}

So in today's article I wanted to share some of the bits I'm going to talk
about next week at the Nordic pgDay conference: database modeling. To get
your interest into database normalization, we see what could happen when you
neglect to normalize properly, with a selection of three classic
anti-patterns: the infamous EAV, using multiple values in a single column,
and how using UUIDs might be an anti-pattern too.

The content of this article is extracted from my book [Mastering PostgreSQL
in Application Development](https://masteringpostgresql.com), check it out!

<!--toc-->

# Entity Attribute Values

The [entity attribute
values](https://en.wikipedia.org/wiki/Entity%E2%80%93attribute%E2%80%93value_model)
or *EAV* is a design that tries to accommodate with a lack of
specifications. In our application, we have to deal with parameters and new
parameters may be added at each release. It's not clear which parameters we
need, we just want a place to manage them easily, and we are already using a
database server after all. So there we go:

~~~ sql
begin;

create schema if not exists eav;

create table eav.params
 (
   entity    text not null,
   parameter text not null,
   value     text not null,
   
   primary key(entity, parameter)
 );

commit;
~~~

You might have already seen this model or a variation of it in the field.
The model makes it very easy to add *things* to it, and very difficult to
make sense of the accumulated data, or to use them effectively in SQL,
making it an anti-pattern.

~~~ sql
insert into eav.params(entity, parameter, value)
     values ('backend', 'log_level', 'notice'),
            ('backend', 'loglevel', 'info'),
            ('api', 'timeout', '30'),
            ('api', 'timout', '40'),
            ('gold', 'response time', '60'),
            ('gold', 'escalation time', '90'),
            ('platinum', 'response time', '15'),
            ('platinum', 'escalation time', '30');
~~~

In this example we made some typos on purpose, to show the limits of the
*EAV* model. It's impossible to catch those errors, and you might have parts
of your code that query one spelling or a different one.

Main problems of this *EAV* anti-pattern are:

  - The *value* attribute is of type *text* so as to be able to host about
    anything, where some parameters are going to be *integer*, *interval*,
    *inet* or *boolean* values.
    
  - The *entity* and *parameter* fields are likewise free-text, meaning that
    any typo will actually create new entries, which might not even be used
    anywhere in the application.
    
  - When fetching all the parameters of an entity to set up your
    application's object, the parameter names are a value in each row rather
    than the name of the column where to find them, meaning extra work and
    loops.
    
  - When you need to process parameter in SQL queries, you need to add a
    join to the *params* table for each parameter you are interested in.

As an example of the last point, here's a query that fetches the *response
time* and the *escalated time* for support customers when using the previous
*params* setup. First, we need a quick design for a customer and a support
contract table:

~~~ sql
begin;

create table eav.support_contract_type
 (
   id   serial primary key,
   name text not null
 );
 
insert into eav.support_contract_type(name)
     values ('gold'), ('platinum');

create table eav.support_contract
 (
   id       serial primary key,
   type     integer not null references eav.support_contract_type(id),
   validity daterange not null,
   contract text,
   
   exclude using gist(type with =, validity with &&)
 );

create table eav.customer
 (
   id       serial primary key,
   name     text not null,
   address  text
 );

create table eav.support
 (
   customer  integer not null,
   contract  integer not null references eav.support_contract(id),
   instances integer not null,

   primary key(customer, contract),
   check(instances > 0)
 );

commit;
~~~

And now it's possible to get customer support contract parameters such as
*response time* and *escalation time*, each with its own join:

~~~ sql
select customer.id,
       customer.name,
       ctype.name,
       rtime.value::interval as "resp. time",
       etime.value::interval as "esc. time"
  from eav.customer
       join eav.support
         on support.customer = customer.id

       join eav.support_contract as contract
         on support.contract = contract.id

       join eav.support_contract_type as ctype
         on ctype.id = contract.type

       join eav.params as rtime
         on rtime.entity = ctype.name
        and rtime.parameter = 'response time'
       
       join eav.params as etime
         on etime.entity = ctype.name
        and etime.parameter = 'escalation time';
~~~

Each parameter you add has to be added as an extra *join* operation in the
previous query. Also, if someone enters a value for *response time* that
isn't compatible with the *interval* data type representation, then the
query fails.

Never implement an *EAV* model, this anti-pattern makes everything more
complex than it should for a very small gain at modeling time.

It might be that the business case your application is solving actually has
an *attribute volatility* problem to solve. In that case, consider having as
solid a model as possible and use *jsonb* columns as extension points.

# Multiple Values per Column

In database normalization, we say that a table (or a relation) is in *first
normal form* (*1NF*) if:

  1. There are no duplicated rows in the table.
  
  2. Each cell is single-valued (no repeating groups or arrays).
  
  3. Entries in a column (field) are of the same kind.

An anti-pattern that fails to comply with those rules means having a
multi-valued field in a database schema:

~~~ sql
create table tweet
 (
   id      bigint primary key,
   date    timestamptz,
   message text,
   tags    text
 );
~~~

Data would then be added with a semicolon separator, for instance, or maybe
a pipe `|` char, or in some cases with a fancy Unicode separator char such
as `§`, `¶` or `¦`. Here we find a classic semicolon:

~~~ psql
         id         │ date │ message │          tags          
════════════════════╪══════╪═════════╪════════════════════════
 720553530088669185 │ ...  │ ...     │ #NY17
 720553531665682434 │ ...  │ ...     │ #Endomondo;#endorphins
(2 rows)
~~~

Using PostgreSQL makes it possible to use the *regexp_split_to_array()* and
*regexp_split_to_table()* functions in order to process the data in a
relatively sane way. The problem with going against *1NF* is that it's
nearly impossible to maintain the data set as the model offers all the
[database anomalies](https://en.wikipedia.org/wiki/Database_normalization)
at once.

Several things are very hard to do when you have several tags hidden in a
*text* column using a separator:

  - Tag Search
  
    To implement searching for a list of messages containing a single given
    tag, this model forces a *substring* search which is much less efficient
    than direct search.
    
    A normalized model would have a separate *tags* table and an association
    table in between the *tweet* and the *tags* reference table that we
    could name *tweet_tags*. Then search for tweets using a given tag is
    easy, as it's a simple join operation with a restriction that can be
    expressed either as a *where* clause or in the *join condition*
    directly.
    
    It is even possible to implement more complex searches of tweets
    containing several tags, or at least one tag in a list. Doing that on
    top of the *CSV* inspired anti-pattern is much more complex, if even
    possible at all.
    
    Rather than trying, we would fix the model!
  
  - Usage Statistics per Tag

    For the same reasons that implementing search is difficult, this *CSV*
    model anti-pattern makes it hard to compute per-tag statistics, because
    the *tags* column is considered as a whole.

  - Normalization of Tags
  
    People make typos or use different spellings for the tags, so we might
    want to normalize them in our database. As we keep the message unaltered
    in a different column, we would not lose any data doing so.
    
    While normalizing the tags at input time is trivial when using a tags
    reference table, it is now an intense computation, as it requires
    looping over all messages and splitting the tags each time.

This example looks a lot like a case of *premature optimization*, which per
[Donald Knuth](https://en.wikipedia.org/wiki/Donald_Knuth) is the root of
all evil… in most cases. The exact quote reads:

> _Programmers waste enormous amounts of time thinking about, or worrying
> about, the speed of noncritical parts of their programs, and these
> attempts at efficiency actually have a strong negative impact when
> debugging and maintenance are considered. We should forget about small
> efficiencies, say about 97% of the time: premature optimization is the
> root of all evil. Yet we should not pass up our opportunities in that
> critical 3%._

> **"Structured Programming with Goto Statements". Computing Surveys 6:4
> (December 1974), pp. 261–301, §1.**

Database modeling has a non-trivial impact on query performance and as such
is part of making attempts at upping efficiency. Using a *CSV* formatted
attribute rather than two additional tables looks like optimization, but
actually it will make just about everything worse: debugging, maintenance,
search, statistics, normalization, and other use cases.

# UUIDs

The PostgreSQL data type UUID allows for 128 bits synthetic keys rather than
32 bits with *serial* or 64 bits with *bigserial*.

The *serial* family of data types is built on a *sequence* with a standard
defined behavior for collision. A *sequence* is non-transactional to allow
several concurrent transactions to each get their own number, and each
transaction might then *commit* or fail to commit with a *rollback*. It
means that sequence numbers are delivered in a monotonous way, always
incrementally, and will be assigned and used without any ordering known in
advance, and with holes in between delivered values.

Still, *sequences* and their usage as a default value for synthetic keys
offer a guarantee against collisions.

*UUIDs* on the other hand rely on a way to produce random numbers in a 128
bits space that offers a strong theoretical guarantee against collision. You
might have to retry producing a number, though very rarely.

*UUIDs* are useful in distributed computing where you can't synchronize
every concurrent and distributed transaction against a common centralized
*sequence*, which would then act as a *Single Point Of Failure*, or *SPOF*.

That said, neither sequences nor *UUID* provides a natural primary key for
your data, as seen in the [Primary
Keys](/blog/2018/03/database-normalization-and-primary-keys/) section.

# Conclusion

Good database modeling is always a trade-off between normalization theories
and denormalization techniques. PostgreSQL offers many denormalization
techniques and some of them are quite advanced, so it's quite tempting to
put them all in good use.

My advice is to always normalize your database model first, and then only
fix the problems you have with that when you actually have them. Well except
in those 3% of cases where really, really, it should be done in the design
phase of the project. It's quite hard to recognize those 3% though, and that
ability is hard gained with experience.

The best way to gain experience and grow your _gut feeling_ and know
instinctively when to denormalize before it gets to be a production problem,
well, as usual, get lots of [deliberate
practice](https://en.wikipedia.org/wiki/Practice_(learning_method)). Which
means, learn to normalize, and force you to doing it always. Then create a
data set as in my article [Simple Data Modeling with a Test Data
Set](/blog/2017/11/simple-data-modeling-with-a-test-data-set/) and write
some queries using your normalized database model.

And only when you find problems then see about denormalizing. Maybe before
the problems happen in production, of course.


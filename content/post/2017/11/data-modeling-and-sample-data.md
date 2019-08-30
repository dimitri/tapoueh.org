+++
title = "Simple Data Modeling with a Test Data Set"
date = "2017-11-27T16:23:44+01:00"
tags = ["PostgreSQL","YeSQL","SQL","Model","Sample","Random","Lorem"]
categories = ["PostgreSQL","YeSQL"]
coverImage = "/img/lorem-ipsum-wallpaper.jpg"
coverMeta = "in"
coverSize = "partial"
thumbnailImage = "/img/lorem-ipsum-logo.png"
thumbnailImagePosition = "left"

+++

In [How to Write SQL](/blog/2017/06/how-to-write-sql/) we saw how to write
SQL queries as separate `.sql` files, and we learnt about using query
parameters with the *psql* syntax for that (`:variable`, `:'variable'`, and
`:"identifier"`). 

For writing our database model, the same tooling is all we need. An
important aspect of using *psql* is its capacity to provide immediate
feedback, and we can also have that with modeling too.

<!--more-->

{{< figure class="right"
           src="/img/TAOP_Book_Cover_200x260.png"
          link="https://theartofpostgresql.com" >}}

This article is extracted from my book [The Art of
PostgreSQL](https://theartofpostgresql.com). If you like what you read here,
you might enjoy a full book of SQL learning material!

<!--toc-->

## How to Write a Database Model

~~~ sql
create database sandbox;
~~~

Now you have a place where to try things out without disturbing existing
application code. If you need to interact with existing SQL objects, it
might be better to use a *schema* rather than a full-blown separate
database:

~~~ sql
create schema sandbox;
set search_path to sandbox;
~~~

In PostgreSQL, each database is an isolated environment. A connection string
must pick a target database, and it's not possible for one database to
interact with objects from another one, because catalogs are kept separated.
This is great for isolation purposes. If you want to be able to *join* data
in between your *sandbox* and your application models, use a *schema*
instead.

When trying a new schema, it's nice to be able to refine it as you go,
trying things out. Here's a simple and effective trick to enable that: write
your schema as a SQL script with explicit transaction control, and finish it
with your testing queries and a *rollback*.

In the following example, we iterate over the definition of a schema for a
kind of forum application about the news. Articles are written and tagged
with a single category, which is selected from a curated list that is
maintained by the editors. Users can read the articles, of course, and
comment on them. In this *MVP*, it's not possible to comment on a comment.

We would like to have a schema and a data set to play with, with some
categories, an interesting number of articles and a random number of
comments for each article.

Here's a SQL script that creates the first version of our schema and
populates it with random data following the specifications above, which are
intentionally pretty loose. Notice how the script is contained within a
single transaction and ends with a *rollback* statement: PostgreSQL even
implements transaction for DDL statements.

~~~ sql
begin;

create schema if not exists sandbox;

create table sandbox.category
 (
   id    serial primary key,
   name  text not null
 );

insert into sandbox.category(name)
     values ('sport'),('news'),('box office'),('music');

create table sandbox.article
 (
   id         bigserial primary key,
   category   integer references sandbox.category(id),
   title      text not null,
   content    text
 );

create table sandbox.comment
 (
   id         bigserial primary key,
   article    integer references sandbox.article(id),
   content    text
 );

insert into sandbox.article(category, title, content)
     select random(1, 4) as category,
            initcap(sandbox.lorem(5)) as title,
            sandbox.lorem(100) as content
       from generate_series(1, 1000) as t(x);

insert into sandbox.comment(article, content)
     select random(1, 1000) as article,
            sandbox.lorem(150) as content
       from generate_series(1, 50000) as t(x);
            
select article.id, category.name, title
  from      sandbox.article
       join sandbox.category
         on category.id = article.category
 limit 3;

select count(*),
       avg(length(title))::int as avg_title_length,
       avg(length(content))::int as avg_content_length
  from sandbox.article;

   select article.id, article.title, count(*)
     from      sandbox.article
          join sandbox.comment
            on article.id = comment.article
group by article.id
order by count desc
   limit 5;

select category.name,
       count(distinct article.id) as articles,
       count(*) as comments
  from      sandbox.category
       left join sandbox.article on article.category = category.id
       left join sandbox.comment on comment.article = article.id
group by category.name
order by category.name;

rollback;
~~~

This SQL script references ad-hoc functions creating a random data set. This
time for the book I've been using a source of *Lorem Ipsum* texts and some
variations on the *random()* function. Typical usage of the script would be
at the *psql* prompt thanks to the `\i` command:

~~~
yesql# \i .../path/to/schema.sql
BEGIN
...
CREATE TABLE
INSERT 0 4
CREATE TABLE
CREATE TABLE
INSERT 0 1000
INSERT 0 50000
 id │    name    │                  title                  
════╪════════════╪═════════════════════════════════════════
  1 │ sport      │ Debitis Sed Aperiam Id Ea
  2 │ sport      │ Aspernatur Elit Cumque Sapiente Eiusmod
  3 │ box office │ Tempor Accusamus Quo Molestiae Adipisci
(3 rows)

 count │ avg_title_length │ avg_content_length 
═══════╪══════════════════╪════════════════════
  1000 │               35 │                738
(1 row)

 id  │                    title                    │ count 
═════╪═════════════════════════════════════════════╪═══════
 187 │ Quos Quaerat Ducimus Pariatur Consequatur   │    73
 494 │ Inventore Eligendi Natus Iusto Suscipit     │    73
 746 │ Harum Saepe Hic Tempor Alias                │    70
 223 │ Fugiat Sed Dolorum Expedita Sapiente        │    69
 353 │ Dignissimos Tenetur Magnam Quaerat Suscipit │    69
(5 rows)

    name    │ articles │ comments 
════════════╪══════════╪══════════
 box office │      322 │    16113
 music      │      169 │     8370
 news       │      340 │    17049
 sport      │      169 │     8468
(4 rows)

ROLLBACK
~~~

As the script ends with a *ROLLBACK* command, you can now edit your schema
and do it again, at will, without having to first clean up the previous run.

## Generating Random Data

In the previous script, you might have noticed calls to functions that don't
exist in the distribution of PostgreSQL, such as *random(int, int)* or
*sandbox.lorem(int)*. Here's a complete ad-hoc definition for them:

~~~ sql
begin;

create schema if not exists sandbox;

drop table if exists sandbox.lorem;

create table sandbox.lorem
 (
   word text
 );

with w(word) as
(
     select regexp_split_to_table('Lorem ipsum dolor sit amet, consectetur
        adipiscing elit, sed do eiusmod tempor incididunt ut labore et
        dolore magna aliqua. Ut enim ad minim veniam, quis nostrud
        exercitation ullamco laboris nisi ut aliquip ex ea commodo
        consequat. Duis aute irure dolor in reprehenderit in voluptate velit
        esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat
        cupidatat non proident, sunt in culpa qui officia deserunt mollit
        anim id est laborum.'
            , '[\s., ]')
      union
     select regexp_split_to_table('Sed ut perspiciatis unde omnis iste natus
        error sit voluptatem accusantium doloremque laudantium, totam rem
        aperiam, eaque ipsa quae ab illo inventore veritatis et quasi
        architecto beatae vitae dicta sunt explicabo. Nemo enim ipsam
        voluptatem quia voluptas sit aspernatur aut odit aut fugit, sed quia
        consequuntur magni dolores eos qui ratione voluptatem sequi
        nesciunt. Neque porro quisquam est, qui dolorem ipsum quia dolor sit
        amet, consectetur, adipisci velit, sed quia non numquam eius modi
        tempora incidunt ut labore et dolore magnam aliquam quaerat
        voluptatem. Ut enim ad minima veniam, quis nostrum exercitationem
        ullam corporis suscipit laboriosam, nisi ut aliquid ex ea commodi
        consequatur? Quis autem vel eum iure reprehenderit qui in ea
        voluptate velit esse quam nihil molestiae consequatur, vel illum qui
        dolorem eum fugiat quo voluptas nulla pariatur?'
            , '[\s., ]')
      union
     select regexp_split_to_table('At vero eos et accusamus et iusto odio
        dignissimos ducimus qui blanditiis praesentium voluptatum deleniti
        atque corrupti quos dolores et quas molestias excepturi sint
        occaecati cupiditate non provident, similique sunt in culpa qui
        officia deserunt mollitia animi, id est laborum et dolorum fuga. Et
        harum quidem rerum facilis est et expedita distinctio. Nam libero
        tempore, cum soluta nobis est eligendi optio cumque nihil impedit
        quo minus id quod maxime placeat facere possimus, omnis voluptas
        assumenda est, omnis dolor repellendus. Temporibus autem quibusdam
        et aut officiis debitis aut rerum necessitatibus saepe eveniet ut et
        voluptates repudiandae sint et molestiae non recusandae. Itaque
        earum rerum hic tenetur a sapiente delectus, ut aut reiciendis
        voluptatibus maiores alias consequatur aut perferendis doloribus
        asperiores repellat.'
            , '[\s., ]')
)
  insert into sandbox.lorem(word)
       select word
         from w
        where word is not null
          and word <> '';

create or replace function random(a int, b int)
  returns int
  volatile
  language sql
as $$
  select a + ((b-a) * random())::int;
$$;

create or replace function sandbox.lorem(len int)
  returns text
  volatile
  language sql
as $$
  with words(w) as (
      select word
       from sandbox.lorem
   order by random()
      limit len
  )
  select string_agg(w, ' ')
    from words;
$$;

commit;
~~~

The not-so-random Latin text comes from [Lorem Ipsum](http://lipsum.com/)
and is a pretty good base for generating random content. We go even further
by separating words from their context and then aggregating them together
completely at random in the *sandbox.lorem(int)* function.

The method we use to get N words at random is known to be rather inefficient
given large data sources. If you have this use case to solve with a big
enough table, then have a look at [selecting random rows from a
table](http://blog.rhodiumtoad.org.uk/2009/03/08/selecting-random-rows-from-a-table/)
article from [Andrew Gierth](http://blog.rhodiumtoad.org.uk/), now a
PostgreSQL committer.

## Modeling Example

Now that we have some data to play with, we can test some application
queries for known user stories in the *MVP*, like maybe listing the most
recent articles per category with the first three comments on each article.

That's when we realize our previous schema design misses publication
timestamps for articles and comments. We need to add this information to our
draft model. As it is all a draft with random data, the easiest way around
this you already *committed* the data previously (by editing the script) is
to simply *drop schema cascade* as shown here:

~~~
yesql# drop schema sandbox cascade;

NOTICE:  drop cascades to 5 other objects
DETAIL:  drop cascades to table sandbox.lorem
drop cascades to function sandbox.lorem(integer)
drop cascades to table sandbox.category
drop cascades to table sandbox.article
drop cascades to table sandbox.comment
DROP SCHEMA
~~~

The next version of our schema then looks like this:

~~~ sql
begin;

create schema if not exists sandbox;

create table sandbox.category
 (
   id    serial primary key,
   name  text not null
 );

insert into sandbox.category(name)
     values ('sport'),('news'),('box office'),('music');

create table sandbox.article
 (
   id         bigserial primary key,
   category   integer references sandbox.category(id),
   pubdate    timestamptz,
   title      text not null,
   content    text
 );

create table sandbox.comment
 (
   id         bigserial primary key,
   article    integer references sandbox.article(id),
   pubdate    timestamptz,
   content    text
 );

insert into sandbox.article(category, title, pubdate, content)
     select random(1, 4) as category,
            initcap(sandbox.lorem(5)) as title,
            random( now() - interval '3 months',
                    now() + interval '1 months') as pubdate,
            sandbox.lorem(100) as content
       from generate_series(1, 1000) as t(x);

insert into sandbox.comment(article, pubdate, content)
     select random(1, 1000) as article,
            random( now() - interval '3 months',
                    now() + interval '1 months') as pubdate,
            sandbox.lorem(150) as content
       from generate_series(1, 50000) as t(x);
            
select article.id, category.name, title
  from      sandbox.article
       join sandbox.category
         on category.id = article.category
 limit 3;

select count(*),
       avg(length(title))::int as avg_title_length,
       avg(length(content))::int as avg_content_length
  from sandbox.article;

   select article.id, article.title, count(*)
     from      sandbox.article
          join sandbox.comment
            on article.id = comment.article
group by article.id
order by count desc
   limit 5;

select category.name,
       count(distinct article.id) as articles,
       count(*) as comments
  from      sandbox.category
       left join sandbox.article on article.category = category.id
       left join sandbox.comment on comment.article = article.id
group by category.name
order by category.name;

commit;
~~~

To be able to generate random timestamp entries, the script uses another
function that's not provided by default in PostgreSQL, and here's its
definition:

~~~ sql
create or replace function random
 (
  a timestamptz,
  b timestamptz
 )
 returns timestamptz
 volatile
 language sql
as $$
  select a
         + random(0, extract(epoch from (b-a))::int)
           * interval '1 sec';
$$;
~~~

## Querying the Schema: our Minimal Viable Product

Now we can have a go at solving the first query of the product's *MVP*, as
specified before, on this schema draft version. That should provide a taste
of the schema and how well it implements the business rules.

The following query lists the most recent articles per category with the
first three comments on each article:

~~~ sql
\set comments 3
\set articles 1

  select category.name as category,
         article.pubdate,
         title,
         jsonb_pretty(comments) as comments

    from sandbox.category
         /*
          * Classic implementation of a Top-N query
          * to fetch 3 most recent articles per category
          */
         left join lateral
         (
            select id,
                   title,
                   article.pubdate,
                   jsonb_agg(comment) as comments
              from sandbox.article
                  /*
                   * Classic implementation of a Top-N query
                   * to fetch 3 most recent comments per article
                   */
                  left join lateral
                  (
                      select comment.pubdate,
                             substring(comment.content from 1 for 25) || '…'
                             as content
                        from sandbox.comment
                       where comment.article = article.id
                    order by comment.pubdate desc
                       limit :comments
                  )
                  as comment
                  on true   -- required with a lateral join
              
             where category = category.id

          group by article.id
          order by article.pubdate desc
             limit :articles
         )
         as article
         on true -- required with a lateral join

order by category.name, article.pubdate desc;
~~~

The first thing we notice when running this query is the lack of indexing
for it. Let's fix this:

~~~ sql
create index on sandbox.article(pubdate);
create index on sandbox.comment(article);
create index on sandbox.comment(pubdate);
~~~

Here's the query result set, with some content removed. The query has been
edited for a nice result text which fits in the book pages, using
*jsonb_pretty()* and *substring()*. When embedding it in application's code,
this extra processing ougth to be removed from the query. Here's the result,
with a single article per category and the three most recent comments per
article, as a *JSONB* document:

~~~
─[ RECORD 1 ]───────────────────────────────────────────────────
category │ box office
pubdate  │ 2017-09-30 07:06:49.681844+02
title    │ Tenetur Quis Consectetur Anim Voluptatem
comments │ [                                                    ↵
         │     {                                                ↵
         │         "content": "adipisci minima ducimus r…",     ↵
         │         "pubdate": "2017-09-27T09:43:24.681844+02:00"↵
         │     },                                               ↵
         │     {                                                ↵
         │         "content": "maxime autem modi ex even…",     ↵
         │         "pubdate": "2017-09-26T00:34:51.681844+02:00"↵
         │     },                                               ↵
         │     {                                                ↵
         │         "content": "ullam dolorem velit quasi…",     ↵
         │         "pubdate": "2017-09-25T00:34:57.681844+02:00"↵
         │     }                                                ↵
         │ ]
═[ RECORD 2 ]═══════════════════════════════════════════════════
category │ music
pubdate  │ 2017-09-28 14:51:13.681844+02
title    │ Aliqua Suscipit Beatae A Dolor
...
═[ RECORD 3 ]═══════════════════════════════════════════════════
category │ news
pubdate  │ 2017-09-30 05:05:51.681844+02
title    │ Mollit Omnis Quaerat Do Odit
...
═[ RECORD 4 ]═══════════════════════════════════════════════════
category │ sport
pubdate  │ 2017-09-29 17:08:13.681844+02
title    │ Placeat Eu At Consequuntur Explicabo
...
~~~

We get this result in about 500ms to 600ms on a laptop, and the timing is
down to about 150ms when the *substring(comment.content from 1 for 25) ||
'…'* part is replaced with just *comment.content*. It's fair to use it in
production, with the proper caching strategy in place, i.e. we expect more
article reads than writes.

## Conclusion

Our schema is a good first version for answering the *MVP*:

  - It follows normalization rules.
  
  - It allows writing the main use case as a single query, and even if the
    query is on the complex side it runs fast enough with a sample of tens
    of thousands of articles and fifty thousands of comments.

  - The schema allows an easy implementation of workflows for editing
    categories, articles, and comments.

This draft schema is a SQL file, so it's easy to check it into your
versioning system, share it with your colleagues and deploy it to
development, integration and continuous testing environments.

For visual schema needs, tools are available that connect to a PostgreSQL
database and help in designing a proper set of diagrams from the live
schema.

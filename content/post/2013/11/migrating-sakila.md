+++
date = "2013-11-12T11:37:00.000000+01:00"
title = "Migrating Sakila from MySQL to PostgreSQL"
tags = ["PostgreSQL", "pgloader", "MySQL"]
categories = ["Projects","pgloader"]
thumbnailImage = "/img/old/dolphin-toy.320.jpg"
thumbnailImagePosition = "left"
coverImage = "/img/old/dolphin-toy.320.jpg"
coverSize = "partial"
coverMeta = "out"
aliases = ["/blog/2013/11/12-migrating-sakila",
           "/blog/2013/11/12-migrating-sakila.html"]
+++

As presented at the 
[PostgreSQL Conference Europe](http://2013.pgconf.eu/) the new version of 
[pgloader](https://github.com/dimitri/pgloader)
is now able to fully migrate a MySQL database, including discovering the
schema, casting data types, transforming data and default values. 
[Sakila](http://dev.mysql.com/doc/sakila/en/) is
the traditional MySQL example database, in this article we're going to fully
migrate it over to PostgreSQL.

<!--more-->

<center>*What about switching to PostgreSQL, it's easier than ever.*</center>

Without further ado, here's what happens when you ask 
*pgloader* to please
migrate the whole thing over to 
[PostgreSQL](http://www.postgresql.org/):

~~~
2013-11-12T11:34:37.000000+01:00 LOG Starting pgloader, log system is ready.
2013-11-12T11:34:37.001000+01:00 LOG Parsing commands from file "/Users/dim/dev/pgloader/test/sakila.load"
                    table name       read   imported     errors            time
------------------------------  ---------  ---------  ---------  --------------
                   before load          0          0          0          0.006s
                  create, drop          0          0          0          0.149s
------------------------------  ---------  ---------  ---------  --------------
                         store          2          2          0          0.013s
                         staff          2          2          0          0.082s
                        rental      16044      16044          0          0.614s
                       payment      16049      16049          0          0.529s
                      language          6          6          0          0.071s
                     inventory       4581       4581          0          0.107s
                     film_text       1000       1000          0          0.077s
                 film_category       1000       1000          0          0.031s
                    film_actor       5462       5462          0          0.079s
                          film       1000       1000          0          0.094s
                      customer        599        599          0          0.069s
                       country        109        109          0          0.029s
                          city        600        600          0          0.038s
                      category         16         16          0          0.021s
                       address        603        603          0          0.041s
                         actor        200        200          0          0.024s
        Index Build Completion          0          0          0          0.000s
------------------------------  ---------  ---------  ---------  --------------
                Create Indexes          0         41          0          1.014s
               Reset Sequences          0          1          0          0.033s
                  Foreign Keys          0         22          0          0.303s
------------------------------  ---------  ---------  ---------  --------------
             Total import time      47273      47273          0          2.410s
~~~


In those 
***2 and a half seconds***, the whole dataset has been converted. Note
that the indexes are being built in parallel with the data loading, and all
indexes against the same relation are built in parallel to each other,
too.

Here's the 
*pgloader command* that we used:

~~~
LOAD DATABASE
     FROM      mysql://root@localhost/sakila
     INTO postgresql://localhost:54393/sakila

 WITH include drop, create tables, no truncate,
      create indexes, reset sequences, foreign keys

  SET maintenance_work_mem to '128MB', work_mem to '12MB',
      search_path to 'sakila' /* migrate to a specific schema */

 CAST type datetime to timestamptz
           drop default drop not null using zero-dates-to-null,
      type date drop not null drop default using zero-dates-to-null,
      type year to integer

 BEFORE LOAD DO
      $$ create schema if not exists sakila; $$;
~~~


Here's an example of how the casting rules work in that very case, where
we've been using mostly default rules:

~~~
sakila# \d sakila.film
                                   Table "sakila.film"
        Column        |              Type              |            Modifiers            
----------------------+--------------------------------+---------------------------------
 film_id              | smallint                       | not null
 title                | text                           | not null
 description          | text                           | 
 release_year         | integer                        | 
 language_id          | smallint                       | not null
 original_language_id | smallint                       | 
 rental_duration      | smallint                       | not null default 3::smallint
 rental_rate          | numeric(4,2)                   | not null default 4.99
 length               | smallint                       | 
 replacement_cost     | numeric(5,2)                   | not null default 19.99
 rating               | sakila.film_rating             | default 'G'::sakila.film_rating
 special_features     | sakila.film_special_features[] | 
 last_update          | timestamp with time zone       | not null default now()
Indexes:
    "film_pkey" PRIMARY KEY, btree (film_id)
    "idx_17589_idx_fk_language_id" btree (language_id)
    "idx_17589_idx_fk_original_language_id" btree (original_language_id)
    "idx_17589_idx_title" btree (title)
Foreign-key constraints:
    "fk_film_language" FOREIGN KEY (language_id) REFERENCES sakila.language(language_id)
    "fk_film_language_original" FOREIGN KEY (original_language_id) REFERENCES sakila.language(language_id)
Referenced by:
    TABLE "sakila.film_actor" CONSTRAINT "fk_film_actor_film" FOREIGN KEY (film_id) REFERENCES sakila.film(film_id)
    TABLE "sakila.film_category" CONSTRAINT "fk_film_category_film" FOREIGN KEY (film_id) REFERENCES sakila.film(film_id)
    TABLE "sakila.inventory" CONSTRAINT "fk_inventory_film" FOREIGN KEY (film_id) REFERENCES sakila.film(film_id)
~~~


We can see that we're using a couple for 
*custom data types* in PostgreSQL,
those are the conversion from the 
`ENUM` and 
`SET` datatypes that MySQL database
is using here. The 
`SET` datatype is simply converted to an array of ENUM
values in PostgreSQL.

At this point, you're left with reviewing the queries in your code and
adapting those. Also unhandled, the triggers and stored procedures and
views.

Your turn!

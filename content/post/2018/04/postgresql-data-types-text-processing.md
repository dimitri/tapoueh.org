+++
title = "PostgreSQL Data Types: Text Processing"
date = "2018-04-11T23:15:42+02:00"
tags = ["PostgreSQL","YeSQL","Data Types","Text","Text Processing"]
categories = ["PostgreSQL","YeSQL"]
coverImage = "/img/iaith-ieithoedd-mamiaith.jpg"
coverMeta = "out"
coverSize = "partial"
thumbnailImage = "/img/text-processing-logo.png"
thumbnailImagePosition = "left"

+++

Continuing our series of [PostgreSQL Data Types](/tags/data-types/) today
we're going to introduce some of the PostgreSQL text processing functions.

There's a very rich set of PostgreSQL functions to process text — you can
find them all in the [string functions and
operators](https://www.postgresql.org/docs/current/static/functions-string.html)
documentation chapter — with functions such as *overlay()*, *substring()*,
*position()* or *trim()*. Or aggregates such as *string_agg()*. There are
also *regular expression* functions, including the very powerful
*regexp_split_to_table()*. In this article we see practical example putting
them in practice.

<!--more-->
<!--toc-->

## Text and varchar

PostgreSQL knows how to deal with characters and text, and it implements
several data types for that, all documented in the [character
types](https://www.postgresql.org/docs/current/static/datatype-character.html)
chapter of the documentation.

About the data type itself, it must be noted that *text* and *varchar* are
the same thing as far as PostgreSQL is concerned, and *character varying* is
an alias for *varchar*. When using *varchar(15)* you're basically telling
PostgreSQL to manage a *text* column with a *check* constraint of 15
characters.

## Regular Expressions in PostgreSQL

PostgreSQL has support for dealing with regular expressions, read the main
documentation about them in the [pattern
matching](https://www.postgresql.org/docs/current/static/functions-matching.html#FUNCTIONS-POSIX-REGEXP)
chapter.

Additionnaly to the classic *like* and *ilike* patterns and to the SQL
standard *similar to* operators, PostgreSQL embeds support for a full-blown
*regular expression* matching engine. The main operator implementing regexp
is `~`, and then you find the derivatives for *not matching* and *match
either case*. In total, we have four operators: `~`, `!~`, `~*` and `!~*`.

Note that PostgreSQL also supports indexing for regular expressions thanks
to its trigram extension:
[pg_trgm](https://www.postgresql.org/docs/current/static/pgtrgm.html).

The *regular expression* split functions are powerful in many use cases. In
particular, they are very helpful when you have to work with a messy schema,
in which a single column represents several bits of information in a pseudo
specified way. 

## A Data Set to Play With

An example of such a dataset is available in open data: the [Archives de la
Planète](https://opendata.hauts-de-seine.fr/explore/dataset/archives-de-la-planete/table/?disjunctive.operateur&sort=identifiant_fakir)
or “planet archives”. The data is available as CSV and once loaded looks
like this:

~~~ sql
\pset format wrapped
\pset columns 70
table opendata.archives_planete limit 1;
~~~

And we get the following sample data, all in French (but it doesn't matter
very much for our purposes here):

~~~ psql
─[ RECORD 1 ]──────────────────────────────────────────────
id          │ IF39599
inventory   │ A 2 037
orig_legend │ Serbie, Monastir Bitolj, Un Turc
legend      │ Un Turc
location    │ Monastir (actuelle Bitola), Macédoine
date        │ mai 1913
operator    │ Auguste Léon
...
themes      │ Habillement > Habillement traditionnel,Etres …
            │…humains > Homme,Etres humains > Portrait,Rela…
            │…tions internationales > Présence étrangère
...
collection  │ Archives de la Planète
...
~~~

You can see that the *themes* column contains several categories for a
single entry, separated with a comma. Within that comma separated list, we
find another classification, this time separated with a greater than sign,
which looks like a hierarchical categorization of the themes.

So this picture id *IF39599* actually is relevant to that series of themes:

~~~ psql
   id    │           cat1            │           cat2           
═════════╪═══════════════════════════╪══════════════════════════
 IF39599 │ Habillement               │ Habillement traditionnel
 IF39599 │ Etres humains             │ Homme
 IF39599 │ Etres humains             │ Portrait
 IF39599 │ Relations internationales │ Présence étrangère
(4 rows)
~~~

The question is, how do we get that information? Also, is it possible to
have an idea of the distribution of the whole data set in relation to the
categories embedded in the *themes* column?

## Splitting Text in PostgreSQL

With PostgreSQL, this is easy enough to achieve. First, we are going to
split the *themes* column using a regular expression:

~~~ sql
select id, regexp_split_to_table(themes, ',')
  from opendata.archives_planete
 where id = 'IF39599';
~~~

We get the following table:

~~~ psql
   id    │             regexp_split_to_table              
═════════╪════════════════════════════════════════════════
 IF39599 │ Habillement > Habillement traditionnel
 IF39599 │ Etres humains > Homme
 IF39599 │ Etres humains > Portrait
 IF39599 │ Relations internationales > Présence étrangère
(4 rows)
~~~

Now that we have a table with an entry per theme for the same document, we
can further split each entry into the two-levels category that it looks
like. We do that this time with *regexp_split_to_array()* so as to keep the
categories together:

~~~ sql
select id,
       regexp_split_to_array(
         regexp_split_to_table(themes, ','),
         ' > ')
       as categories
  from opendata.archives_planete
 where id = 'IF39599';
~~~

And now we have:

~~~ psql
   id    │                     categories                     
═════════╪════════════════════════════════════════════════════
 IF39599 │ {Habillement,"Habillement traditionnel"}
 IF39599 │ {"Etres humains",Homme}
 IF39599 │ {"Etres humains",Portrait}
 IF39599 │ {"Relations internationales","Présence étrangère"}
(4 rows)
~~~

We're almost there. For the content to be normalized we want to have the
categories in their own separate columns, say *category* and *subcategory*:

~~~ sql
with categories(id, categories) as
 (
   select id,
          regexp_split_to_array(
            regexp_split_to_table(themes, ','),
            ' > ')
          as categories
     from opendata.archives_planete
 )
 select id,
        categories[1] as category,
        categories[2] as subcategory
   from categories
  where id = 'IF39599';
~~~

And now we make sense of the open data:

~~~ psql
   id    │         category          │       subcategory        
═════════╪═══════════════════════════╪══════════════════════════
 IF39599 │ Habillement               │ Habillement traditionnel
 IF39599 │ Etres humains             │ Homme
 IF39599 │ Etres humains             │ Portrait
 IF39599 │ Relations internationales │ Présence étrangère
(4 rows)
~~~

{{< alert success >}}

As a side note, cleaning up a data set after you've imported it into
PostgreSQL makes the difference clear between the classic *ETL* jobs
(extract, transform, load) and the powerful *ELT* jobs (extract, load,
transform) where you can transform your data using a data processing
language: SQL.

{{< /alert >}}

## Categories, Subcategories, and Statistics

So, now that we know how to have a clear view of the dataset, let's inquire
about the categories used in our dataset:

~~~ sql
with categories(id, categories) as
  (
    select id,
           regexp_split_to_array(
             regexp_split_to_table(themes, ','),
             ' > ')
           as categories
      from opendata.archives_planete
  )
  select categories[1] as category,
         categories[2] as subcategory,
         count(*)
    from categories
group by rollup(category, subcategory);
~~~

That query returns 175 rows, so here's an extract only:

~~~ psql
       category         │         subcategory          │ count 
════════════════════════╪══════════════════════════════╪═══════
 Activite économique    │ Agriculture / élevage        │   138
 Activite économique    │ Artisanat                    │    81
 Activite économique    │ Banque / finances            │     2
 Activite économique    │ Boutique / magasin           │    39
 Activite économique    │ Commerce ambulant            │     5
 Activite économique    │ Commerce extérieur           │     1
 Activite économique    │ Cueillette / chasse          │     9
...
 Art                    │ Peinture                     │    15
 Art                    │ Renaissance                  │    52
 Art                    │ Sculpture                    │    87
 Art                    │ Théâtre                      │     7
 Art                    │ ¤                            │   333
...
 Habillement            │ Uniforme scolaire            │     1
 Habillement            │ Vêtement de travail          │     3
 Habillement            │ ¤                            │   163
 Habitat / Architecture │ Architecture civile publique │    37
 Habitat / Architecture │ Architecture commerciale     │    24
 Habitat / Architecture │ Architecture de jardin       │    31
...
 Vie quotidienne        │ Vie domestique               │     8
 Vie quotidienne        │ Vie rurale                   │     5
 Vie quotidienne        │ ¤                            │    64
 ¤                      │ ¤                            │  4449
(175 rows)
~~~

Each *subcategory* appears only within the same *category* each time, and
we've chosen to do a *roll up* analysis of our data set here. Other
*grouping sets* are available, such as the *cube*, or manually editing the
dimensions you're interested into.

In an *ELT* assignment, we would create a new *categories* table containing
each entry we saw in the rollup query only once, as a catalog, and an
association table in between the main *opendata.archives_planete* table and
this categories catalog, where each archive entry might have several
categories and subcategories assigned and each category, of course, might
have several archive entries assigned.

Here, the topic is about text function processing in PostgreSQL, so we just
run the query against the base data set.

## Conclusion

Finally, when mentioning advanced string matching and the *regular
expression*, we must also mention PostgreSQL's implementation of a [full
text search](https://www.postgresql.org/docs/current/static/textsearch.html)
with support for *documents*, advanced *text search queries*, *ranking*,
*highlighting*, *pluggable parsers*, *dictionaries* and *stemmers*,
*synonyms*, and *thesaurus*. Additionally, it's possible to configure all
those pieces. This is material for another book, so if you need advanced
searches of documents that you manage in PostgreSQL please read the
documentation about it. There are also many online resources on the topic
too.

{{< figure class="right"
             src="/img/MasteringPostgreSQLinAppDev-Cover-th.png"
            link="https://masteringpostgresql.com" >}}

This article is an extract from my book [Mastering PostgreSQL in Application
Development](https://masteringpostgresql.com), which teaches SQL to
developers so that they may replace thousands of lines of code with very
simple queries. The book has a full chapter about data types in PostgreSQL,
check it out!

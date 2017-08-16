+++
title = "Regular Expressions and Grouping Sets"
date = "2017-08-14T16:37:53+02:00"
tags = ["PostgreSQL","YeSQL","ETL","ELT",
        "SQL","Grouping Sets", "Cube", "Rollup", "regex"]
categories = ["PostgreSQL","YeSQL"]
coverImage = "/img/regex.jpg"
coverMeta = "in"
coverSize = "partial"
thumbnailImage = "/img/RegEx.png"
thumbnailImagePosition = "left"

+++

There's a very rich set of PostgreSQL functions to process text, you can
find them all at
the
[String Functions and Operators](https://www.postgresql.org/docs/current/static/functions-string.html) documentation
chapter, with functions such as *overlay*, *substring*, *position* or
*trim*. Or aggregates such as *string_agg*. And then *regular expression*
functions, including the very powerful *regexp_split_to_table*.

<!--more-->

The *regular expression* split functions are powerful in many use cases. In
particular, they are very helpful when you have to work with a messy schema,
in which a single column represents several bits of information in a pseudo
specified way.

<!--toc-->

# Some Open Data to play with

An example of such a data set is available in Open Data:
the
[Archives de la Planète](https://opendata.hauts-de-seine.fr/explore/dataset/archives-de-la-planete/table/?disjunctive.operateur&sort=identifiant_fakir) or
“planet archives”.

Once loaded into [PostgreSQL](https://www.postgresql.org/) thanks
to [pgloader](http://pgloader.io/), the data is available as CSV and once
loaded looks like this:

~~~ sql
\pset format wrapped
\pset columns 70
table opendata.archives_planete limit 1;
~~~

And we get the following sample data, all in French (but it doesn't matter
very much):

~~~
─[ RECORD 1 ]──────────────────────────────────────────────
id          │ IF39599
inventory   │ A 2 037
orig_legend │ Serbie, Monastir Bitolj, Un Turc
legend      │ Un Turc
location    │ Monastir (actuelle Bitola), Macédoine
date        │ mai 1913
...
themes      │ Habillement > Habillement traditionnel,Etres …
            │…humains > Homme,Etres humains > Portrait,Rela…
            │…tions internationales > Présence étrangère
...
collection  │ Archives de la Planète
~~~

# Regular Expression based Splitting

You can see that the *themes* column contains several catagories for a
single entry, separated with a coma. Within that coma separated list, we
find another classification, this time separated with a greater than sign,
for which looks like a hierarchical categorization of the themes.

So this picture id *IF39599* actually is relevant to that series of themes:

~~~
   id    │         category          │       subcategory        
═════════╪═══════════════════════════╪══════════════════════════
 IF39599 │ Habillement               │ Habillement traditionnel
 IF39599 │ Etres humains             │ Homme
 IF39599 │ Etres humains             │ Portrait
 IF39599 │ Relations internationales │ Présence étrangère
(4 rows)
~~~

Question is, how do we get that information? Also, is it possible to have an
idea of the distribution of the whole data set against the categories
embedded in the *themes* column?

Given PostgreSQL, it is easy enough to achieve. First step, we are going to
split the *themes* column against a regular expression:

~~~ sql
select id, regexp_split_to_table(themes, ',')
  from opendata.archives_planete
 where id = 'IF39599';
~~~

We get the following table:

~~~
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
like. We do that this time with *regexp_split_to_array* so as to retain the
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

~~~
   id    │                     categories                     
═════════╪════════════════════════════════════════════════════
 IF39599 │ {Habillement,"Habillement traditionnel"}
 IF39599 │ {"Etres humains",Homme}
 IF39599 │ {"Etres humains",Portrait}
 IF39599 │ {"Relations internationales","Présence étrangère"}
(4 rows)
~~~

We're almost there, for the content to be normalized we want to have the
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

And now we make sense of the Open Data:

~~~
   id    │         category          │       subcategory        
═════════╪═══════════════════════════╪══════════════════════════
 IF39599 │ Habillement               │ Habillement traditionnel
 IF39599 │ Etres humains             │ Homme
 IF39599 │ Etres humains             │ Portrait
 IF39599 │ Relations internationales │ Présence étrangère
(4 rows)
~~~

As a side note, cleaning up a data set after you've imported it into
PostgreSQL makes the difference between the classic *ETL* jobs (Extract,
Transform, Load) and the powerful *ELT* jobs (Extract, Load, Transform)
where you can transform your data using a data processing language, SQL.

# Grouping Sets, Cube, Rollup

So, now that we know how to have a clean view of the data set, let's inquire
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

That query returns 175 rows here, so here's an extract only:

~~~
         category       │          subcategory         │ count 
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
 Habillement            │ Uniforme militaire           │    18
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

Each *subcategory* appearing only within the same *category* each time,
we've choosen to do a *roll up* analysis of our data set here. Other
*grouping sets* are available, such as the *cube*, or manual editing of the
dimensions you're interested into.

In a *ELT* assignment, we would create a new *categories* table containing
each entry we saw in the rollup query only once, as a catalog, and a
association table in between the main *opendata.archives_planete* table and
this categories catalog, where each archive entry may have several
categories and subcategories assigned and each category of course may have
several archive entries assigned.

Here, the topic is about text function processing in PostgreSQL, so we just
run the query against the base data set.

# Conclusion

{{< image classes="fig25 right dim-margin"
              src="/img/old/sql-logo.png"
           title="PostgreSQL is YeSQL" >}}
           
Next time you have to deal with a data set that needs cleaning before you
can work correctly with it, think about the processing you have to apply to
it, and which tool is best for the job. It might be that you need to use
your application's object model to process the data, in which case a
traditional *ETL* job with transformations using the application's code as a
library might be the best solution. It might also be easier and faster to
transform the data using SQL, once loaded in PostgreSQL. It's called an
*ELT*, and you can do it too!

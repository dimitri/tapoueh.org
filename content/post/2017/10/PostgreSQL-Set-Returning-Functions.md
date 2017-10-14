+++
title = "Set Returning Functions and PostgreSQL 10"
date = "2017-10-13T13:25:21+02:00"
tags = ["PostgreSQL","YeSQL","lateral","SRF"]
categories = ["PostgreSQL","YeSQL"]
coverImage = "/img/cog-blackbg.jpg"
coverMeta = "in"
coverSize = "partial"
thumbnailImage = "/img/srf.jpg"
thumbnailImagePosition = "left"
aliases = ["/blog/2017/10/set-returning-fonctions-and-postgresql-10/"]

+++

PostgreSQL 10 is now available for everyone to use, and hinted by [David
Fetter](http://fetter.org) I had to review my previous article [on Json and
SQL](/blog/2017/09/on-json-and-sql/) to adapt to *Set Returning Functions*
changes.

A *Set Returning Function* is a PostgreSQL *Stored Procedure* that can be
used as a relation: from a single call it returns an entire result set, much
like a subquery or a table.

It used to be possible to use *SRF* in the *SELECT* clause, with dubious
(but useful at times) semantics, and also in *scalar* contexts. The
semantics have been fixed and are now much clearer, and the uses in scalar
contexts are forbidden — they were a hack and never made sense anyway.

<!--more-->

In particular, the following change in PostgreSQL 10 impacts a query written
in the previous blog post. The change is part of the [PostgreSQL 10 Release
Notes](https://www.postgresql.org/docs/devel/static/release-10.html) and
reads: _Change the implementation of set-returning functions appearing in a
query's SELECT list (Andres Freund)_.

The release notes then expand to:

> Set-returning functions are now evaluated before evaluation of scalar
> expressions in the SELECT list, much as though they had been placed in a
> LATERAL FROM-clause item. This allows saner semantics for cases where
> multiple set-returning functions are present. If they return different
> numbers of rows, the shorter results are extended to match the longest
> result by adding nulls. Previously the results were cycled until they all
> terminated at the same time, producing a number of rows equal to the least
> common multiple of the functions' periods. In addition, set-returning
> functions are now disallowed within CASE and COALESCE constructs. For more
> information see Section 37.4.8.

The documentation Section 37.4.8 is [SQL Functions Returning
Sets](https://www.postgresql.org/docs/devel/static/xfunc-sql.html#xfunc-sql-functions-returning-set)
and gives all the necessary details to understand the feature and benefit
from it in your application code, as usual in the PostgreSQL Fine Manual.

<script async id="_ck_279686" src="https://forms.convertkit.com/279686?v=6"></script>

Here's the query as previously written:

~~~ sql
  select case jsonb_typeof(booster)
              when 'array'
              then initcap(jsonb_array_elements_text(booster))
              else initcap(booster #>> '{}')
          end
         as rarity,
         count(*)
    from magic.sets,
         jsonb_array_elements(data->'booster') booster
group by rarity
order by count desc;
~~~

And the result we now obtain when running it:

~~~
ERROR:  set-returning functions are not allowed in CASE
LINE 3:               then initcap(jsonb_array_elements_text(booster...
                                   ^
HINT:  You might be able to move the set-returning function into a LATERAL FROM item.
~~~

The *jsonb_array_elements_text()* is a *Set Returning Function* as can be
seen here:

~~~
> \df jsonb_array_elements_text

List of functions
─[ RECORD 1 ]───────┬────────────────────────────────
Schema              │ pg_catalog
Name                │ jsonb_array_elements_text
Result data type    │ SETOF text
Argument data types │ from_json jsonb, OUT value text
Type                │ normal
~~~

Now, I have been quite lazy in the writing of the previous SQL query. Lazy
enough to use an *SRF* in a scalar context, within the *CASE* construct. In
previous PostgreSQL versions, the SQL engine would then expand the select
output into as many rows as returned by the *jsonb_array_elements()* call.

Let's fix the query for PostgreSQL 10 and clear semantics, processing a
single row at a time in our *SELECT* projection clause.

The problem we want to solve in this query is all about poor data structure
semantics in the JSON representation of the dataset we got from the Magic
project. The *booster* entry is either a JSON string or a JSON array of
strings.

To be able to process this information in SQL, we need to have always the
same types of values, so we need to transform the data into a common
representation. Here, the simplest common representation is a JSON array of
strings, and so we need to transform scalar entries into an array containing
a single entry. Then we can resume to our previous processing, where we
*unnest* the array thanks to the specialized *jsonb_array_elements_text()*
function.

In SQL, this translates to the following query:

~~~ sql
with booster(rarity_js) as (
  select case jsonb_typeof(booster)
              when 'array'
              then booster
              else array_to_json(array[booster])::jsonb
          end
    from magic.sets,
         jsonb_array_elements(data->'booster') as booster
)
  select initcap(rarity) as rarity, count(*)
    from booster,
         jsonb_array_elements_text(rarity_js) as t(rarity)
group by rarity
order by count desc;
~~~

In the first part of the query, the *booster* Common Table Expression
normalize the JSON data we have to deal with in the *data->'booster'* nested
JSON document. We find our *CASE* construct again, and this time rather than
using PostgreSQL 9.6 semantics to *unnest* the array for us in the *SELECT*
clause, we arrange our data to always be an array of JSON text.

In the second part of the query it's now possible to apply the *unnest*
function *jsonb_array_elements_text* uniformly to every entry of the CTE
result set, and then we get the same output as in our previous article:

~~~
          rarity          │ count 
══════════════════════════╪═══════
 Common                   │  1138
 Uncommon                 │   331
 Rare                     │   110
 Land                     │    50
 Mythic Rare              │    44
 Marketing                │    43
 Timeshifted Common       │    14
 Checklist                │     5
 Timeshifted Uncommon     │     4
 Foil Uncommon            │     4
 Foil Rare                │     4
 Foil Common              │     4
 Foil Mythic Rare         │     4
 Double Faced Rare        │     2
 Timeshifted Purple       │     2
 Double Faced Mythic Rare │     2
 Double Faced Uncommon    │     2
 Double Faced Common      │     2
 Draft-Matters            │     2
 Timeshifted Rare         │     2
 Power Nine               │     1
 Double Faced             │     1
 Foil                     │     1
 Urza Land                │     1
(24 rows)
~~~

PostgreSQL 10 is another excellent release from the community, it's
available now! Run a CI instance on it now, so that when the time comes for
your production setup to consider migrating, you'll have no surprises!

After all, PostgreSQL 10 is all Open Source, so there's a very limited cost
to deploying it in your testing environments early, and huge benefits to
doing so.


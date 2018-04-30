+++
title = "PostgreSQL Data Types: JSON"
date = "2018-04-30T09:49:33+02:00"
tags = ["PostgreSQL","YeSQL","Data Types","JSON"]
categories = ["PostgreSQL","YeSQL"]
coverImage = "/img/jason-and-the-argonauts.jpg"
coverMeta = "in"
coverSize = "partial"
thumbnailImage = "/img/json-logo.png"
thumbnailImagePosition = "left"

+++

Continuing our series of [PostgreSQL Data Types](/tags/data-types/) today
we're going to introduce the PostgreSQL JSON type.

PostgreSQL has built-in support for JSON with a great range of processing
functions and operators, and complete indexing support. The documentation
covers all the details in the chapters entitled [JSON
Types](https://www.postgresql.org/docs/current/static/datatype-json.html)
and [JSON Functions and
Operators](https://www.postgresql.org/docs/current/static/functions-json.html).

<!--more-->
<!--toc-->

{{< alert info >}}

To know a lot more about processing JSON and normalizing a database model
based on a set of JSON documents, you can read my article [on Json and
SQL](https://tapoueh.org/blog/2017/09/on-json-and-sql/) which contains a
full detailed example using data from [Magic: the Gathering card data in
JSON format](https://mtgjson.com).

{{< /alert >}}

# JSON and JSONB

PostgreSQL implemented a very simple *JSON* datatype back in the 9.2
release. At that time the community pushed for providing a solution for
*JSON* users, in contrast to the usual careful pace, though still speedy.
The *JSON* datatype is actually *text* under the hood, with a verification
that the format is valid *json* input… much like *XML*.

Later, the community realized that the amount of *JSON* processing and
advanced searching required in PostgreSQL would not be easy or reasonable to
implement over a text datatype, and implemented a *binary* version of the
*JSON* datatype, this time with a full set of operators and functions to
work with.

There are some incompatibilities in between the text-based *json* datatype
and the newer *jsonb* version of it, where it's been argued that *b* stands
for *better*:

  - The *json* datatype, being a text datatype, stores the data presentation
    exactly as it is sent to PostgreSQL, including whitespace and
    indentation, and also multiple-keys when present (no processing at all
    is done on the content, only form validation).
    
  - The *jsonb* datatype is an advanced binary storage format with full
    processing, indexing and searching capabilities, and as such
    pre-processes the JSON data to an internal format, which does include a
    single value per key; and also isn't sensible to extra whitespace or
    indentation.

# Introduction to JSON in PostgreSQL

The data type you probably need and want to use is *jsonb*, not the *json*
early draft that is still available for backward compatibility reasons only.
Here's a very quick example showing some differences between those two
datatypes:

~~~ sql
create table js(id serial primary key, extra json);
insert into js(extra)
     values ('[1, 2, 3, 4]'),
            ('[2, 3, 5, 8]'),
            ('{"key": "value"}');
~~~

The *js* table only has a primary key and a *json* column for extra
information. It's not a good design, but we want a very simple example here
and won't be coding any application on top of it, so it will do for the
following couple SQL queries:

~~~ sql
select * from js where extra @> '2';
~~~

When we want to search for entries where the *extra* column contains a
number in its array, we get the following error:

~~~ psql
ERROR:  operator does not exist: json @> unknown
LINE 1: select * from js where extra @> '2';
                                     ^
HINT:  No operator matches the given name and argument type(s). ⏎
You might need to add explicit type casts.
~~~

Right. *json* is only text and not very powerful, and it doesn't offer an
implementation for the *contains* operator. Switching the content to *jsonb*
then:

~~~ sql
alter table js alter column extra type jsonb;
~~~

Now we can run the same query again:

~~~ sql
select * from js where extra @> '2';
~~~

And we find out that of course our sample data set of two rows contains the
number *2* in the extra *jsonb* field, which here only contains arrays of
numbers:

~~~ psql
 id │    extra     
════╪══════════════
  1 │ [1, 2, 3, 4]
  2 │ [2, 3, 5, 8]
(2 rows)
~~~

We can also search for JSON arrays containing another JSON array:

~~~ sql
select * from js where extra @> '[2,4]';
~~~

This time a single row is found, as expected:

~~~ psql
 id │    extra     
════╪══════════════
  1 │ [1, 2, 3, 4]
(1 row)
~~~

# JSON use cases in PostgreSQL

Two use cases for JSON in PostgreSQL are very commonly found:

  - The application needs to manage a set of documents that happen to be
    formatted in *JSON*.
    
  - Application designers and developers aren't too sure about the exact set
    of fields needed for a part of the data model, and want this data model
    to be very easily extensible.
    
In the first case, using *jsonb* is a great enabler in terms of your
application's capabilities to process the documents it manages, including
searching and filtering using the content of the document. See [jsonb
Indexing](https://www.postgresql.org/docs/current/static/datatype-json.html#JSON-INDEXING)
in the PostgreSQL documentation for more information about the
`jsonb_path_ops` which can be used as in the following example and provides
a very good general purpose index for the `@>` operator as used in the
previous query:

~~~ sql
create index on js using gin (extra jsonb_path_ops);
~~~

Now, it is possible to use *jsonb* as a flexible way to maintain your data
model. It is possible to then think of PostgreSQL like a *schemaless*
service and have a heterogeneous set of documents all in a single relation.

# Conclusion

This trade-off sounds interesting from a model design and maintenance
perspective, but is very costly when it comes to daily queries and
application development: you never really know what you're going to find out
in the *jsonb* columns, so you need to be very careful about your SQL
statements as you might easily miss rows you wanted to target, for example.

A good trade-off is to design a model with some static columns are created
and managed traditionally, and an *extra* column of *jsonb* type is added
for those things you didn't know yet, and that would be used only sometimes,
maybe for debugging reasons or special cases.

This works well until the application's code is querying the *extra* column
in every situation because some important data is found only there. At this
point, it's worth promoting parts of the *extra* field content into proper
PostgreSQL attributes in your relational schema.

{{< figure class="right"
             src="/img/MasteringPostgreSQLinAppDev-Cover-th.png"
            link="https://masteringpostgresql.com" >}}
            
This article is an extract from my book [Mastering PostgreSQL in Application
Development](https://masteringpostgresql.com), which teaches SQL to
developers so that they may replace thousands of lines of code with very
simple queries. The book has a full chapter about data types in PostgreSQL,
check it out!


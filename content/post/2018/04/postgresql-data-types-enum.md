+++
title = "PostgreSQL Data Types: ENUM"
date = "2018-05-02T11:00:26+02:00"
tags = ["PostgreSQL","YeSQL","Data Types","ENUM"]
categories = ["PostgreSQL","YeSQL"]
coverImage = "/img/pixel-art-in-a-grid.png"
coverMeta = "out"
coverSize = "partial"
thumbnailImage = "/img/shop_set-31-512.png"
thumbnailImagePosition = "left"

+++

Continuing our series of [PostgreSQL Data Types](/tags/data-types/) today
we're going to introduce the PostgreSQL ENUM type.

This data type has been added to PostgreSQL in order to make it easier to
support migrations from MySQL. Proper relational design would use a
reference table and a foreign key instead.

<!--more-->
<!--toc-->


## Reference Table

Here's an example of a database model using a reference table for available
colors available for some cars brands and models:

~~~ sql
create table color(id serial primary key, name text);

create table cars
 (
   brand   text,
   model   text,
   color   integer references color(id)
 );

insert into color(name)
     values ('blue'), ('red'),
            ('gray'), ('black');

insert into cars(brand, model, color)
     select brand, model, color.id
      from (
            values('ferari', 'testarosa', 'red'),
                  ('aston martin', 'db2', 'blue'),
                  ('bentley', 'mulsanne', 'gray'),
                  ('ford', 'T', 'black')
           )
             as data(brand, model, color)
           join color on color.name = data.color;
~~~

In this setup the table *color* lists available colors to choose from, and
the cars table registers availability of a model from a brand in a given
color.

{{< alert success >}}

You can see that in the previous query we use the form INSERT ... SELECT
that allows doing a reference table lookup directly in the INSERT statement.
Thanks to this technique, we can avoid hard-coding the color numbers in our
INSERT statement.

{{< /alert >}}

The content of the table is as expected:

~~~
    brand     │   model   │ color 
══════════════╪═══════════╪═══════
 aston martin │ db2       │     1
 ferari       │ testarosa │     2
 bentley      │ mulsanne  │     3
 ford         │ T         │     4
(4 rows)
~~~

And to get the color name, a lookup to the reference table is necessary:

~~~ sql
  select brand, model, color.name as color
    from      cars 
         join color
           on color.id = cars.color;
~~~

Which gives the expected answer:

~~~
    brand     │   model   │ color 
══════════════╪═══════════╪═══════
 aston martin │ db2       │ blue
 ferari       │ testarosa │ red
 bentley      │ mulsanne  │ gray
 ford         │ T         │ black
(4 rows)
~~~

## PostgreSQL ENUM Data Type

It's possible to make an *enum* type instead, using the following database
model now:

~~~ sql
create type color_t as enum('blue', 'red', 'gray', 'black');

drop table if exists cars;
create table cars
 (
   brand   text,
   model   text,
   color   color_t
 );

insert into cars(brand, model, color)
     values ('ferari', 'testarosa', 'red'),
            ('aston martin', 'db2', 'blue'),
            ('bentley', 'mulsanne', 'gray'),
            ('ford', 'T', 'black');
~~~

Be aware that in MySQL there's no *create type* statement for *enum* types,
so each column using an *enum* is assigned its own data type. As you now
have a separate anonymous data type per column, good luck maintaining a
globally consistent state if you need it.

When using PostgreSQL, each ENUM type is registered in the system catalogs
and can be used anywhere PostgreSQL expects a type name. Internally, the
ENUM values are stored as integers.

{{< alert warning >}}

It is important to realize that each ENUM type in PostgreSQL is registered
in the system catalogs. This means that editing the ENUM list of accepted
values now is a DDL operation implemented with [ALTER
TYPE](https://www.postgresql.org/docs/current/static/sql-altertype.html).

This comes with some restictions, the main one being the following note as
read in the PostgreSQL documentation:

> ALTER TYPE ... ADD VALUE (the form that adds a new value to an enum type)
> cannot be executed inside a transaction block.

{{< /alert >}}

## Conclusion

Using the *enum* PostgreSQL facility is mostly a matter of taste. After all,
join operations against small reference tables are well supported by the
PostgreSQL SQL engine.

Remember that *enum* are to be used against a very static type definition: a
list of values that you expect never to change in the life time of your
application! As an hint, consider that if you have to provide an
administration panel to edit the list of values, or their ordering, then you
probably should not use an enumarated type in PostgreSQL. Using reference
tables work well in PostgreSQL!

This article is an extract from my book [The Art of
PostgresQL](https://theartofpostgresql.com), which teaches SQL to developers
so that they may replace thousands of lines of code with very simple
queries. The book has a full chapter about data types in PostgreSQL, check
it out!


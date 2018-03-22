+++
title = "Object Relational Database Management System"
date = "2018-03-22T17:40:39+01:00"
tags = ["PostgreSQL","YeSQL","Object Oriented Programming", 
        "Object Relational", "CLOS", "Function Overloading"]
categories = ["PostgreSQL","YeSQL"]
coverImage = "/img/elephant-species.jpg"
coverMeta = "out"
coverSize = "partial"
thumbnailImage = "/img/oop.png"
thumbnailImagePosition = "left"

+++

[PostgreSQL](https://www.postgresql.org) is the world's most advanced open
source database, and per the [PostgreSQL Wikipedia
page](https://en.wikipedia.org/wiki/PostgreSQL) it is _an
**object-relational** database management system (ORDBMS) with an emphasis
on extensibility and standards compliance_.

In this article, we try to understand why would PostgreSQL be named an
*object-relational* thing. What is Object Oriented Programming and how does
that apply to a database system?

<!--more-->

# Object Orientation in Progamming Languages

[Erik Naggum](https://en.wikipedia.org/wiki/Erik_Naggum) was a Norwegian
computer programmer recognized for his work in the fields of SGML, Emacs and
Lisp. He had strong opinions and shared them on
[Usenet](https://en.wikipedia.org/wiki/Usenet). One of my favorite piece
from Erik Naggum is titled [The Long, Painful History of
Time](http://naggum.no/lugm-time.html) and dives into our calendars history,
and how to write clever handling of dates and timestamps and time zone when
you have enough historical background to actually understand what you're
dealing with.

Spoiler:

> The Gregorian calendar improved on the quadrennial leap years in the
> Julian calendar by making only every fourth centennial a leap year, [...]
> but the simplicity of the scheme is quite amazing: a 400-year cycle not
> only starts 2000-03-01 (as it did 1600-03-01), it contains an even number
> of weeks: 20,871. This means that we can make do with a single 400-year
> calculation for all time within the Gregorian calendar with respect to
> days of week, leap days, etc.

Today though, I wanted to use Naggum's rant about the [Common Lisp Object
System](https://en.wikipedia.org/wiki/Common_Lisp_Object_System) and how it
compares to other ones, such as the ones in Smalltalk or in the Java, C++,
Python, Ruby, and other mainstream programming languages. The usenet article
[Re: How much use of
CLOS?](https://www.xach.com/naggum/articles/3243735416407529@naggum.no.html)
is a must read, and mentions that _there are two real approaches to
object-orientation_:

  - Message-Passing

    > The first is known as message-passing. You send an object a message
    > and ask it to deal with it. [...] The meaning of the message is local
    > to the object, which inherits it from the class of which it is an
    > instance, which may inherit it from superclasses of that class. In
    > other words, you have no idea what happens when you send a message to
    > an object, how many arguments it needs to be happy or anything. [...]
    
    > This can get very messy, and it is therefore deemed appropriate to
    > "clean up" this mess by adding compiler checks that that message on
    > that object really takes that argument list. This is the core mistake
    > in the message-passing model. Smalltalk did not make this core
    > mistake, which means that people from the non-OO-"OO"-language camps
    > get all uneasy about Smalltalk.
    
  - Generic Functions
  
    > The second approach is generic functions. A generic function has one
    > definition of its semantics, its argument list, and is only
    > _specialized_ on particular types of arguments. It can be specialized
    > on any argument or any number of arguments in the argument list, on
    > any type each. [...]
    
    > You do not get to make the class implement methods on generic
    > functions. There is no way to add "method" definitions to a
    > `defclass`. [...]

Naguum then writes the following summary:

> What does this mean with respect to when you think you use OO? In the
> message-passing paradigm, you need to define a class to get any methods at
> all and you have to use those methods on instances of that class. In the
> generic function paradigm, you can define generic functions without ever
> defining any classes. This tends to blow "OO" people's mind.

If you're interested into generic functions in the Common Lisp Object
System, one of the best resource available is the book [Practical Common
Lisp](http://www.gigamonkeys.com/book/) from Peter Seibel, and in particular
the chapters [Object Reorientation: Generic
Functions](http://www.gigamonkeys.com/book/object-reorientation-generic-functions.html)
and [Object Reorientation:
Classes](http://www.gigamonkeys.com/book/object-reorientation-classes.html).

If the only Object Model you've ever worked with looks like the Java, C++,
PHP, and Python one, prepare for your mind to be blown.
    
# Object Orientation in PostgreSQL

PostgreSQL is the first RDBMS to have put emphasis on extensible data types
and plugability. As a result, 30 years later, we have many awesome
extensions available, such as [PostGIS](https://postgis.net),
[Citus](https://www.citusdata.com/product),
[prefix](https://github.com/dimitri/prefix) or
[ip4r](https://github.com/RhodiumToad/ip4r), to name but a few.

In the PostgreSQL design, when it comes to data types, functions, operators
and index support, nothing is hardcoded. Even with the simplest queries, the
planner has to lookup the catalogs to understand what's going on.

In the following query, for example, PostgreSQL's planner has to lookup the
catalog tables in order to know what you mean, and any extension could be
providing new data types, operators, functions and index support.

~~~ sql
select x, 
       1 + x as "1+",
       '127.0.0.1'::inet + x as "ip address",
       date 'today' + x as date
  from (values (0), (1), (2), (3)) as t(x);
~~~

The query has the following result, where you can see that the `+` operator
implementation obviously depends on the datatype:

~~~
 x │ 1+ │ ip address │    date    
═══╪════╪════════════╪════════════
 0 │  1 │ 127.0.0.1  │ 2018-03-22
 1 │  2 │ 127.0.0.2  │ 2018-03-23
 2 │  3 │ 127.0.0.3  │ 2018-03-24
 3 │  4 │ 127.0.0.4  │ 2018-03-25
(4 rows)
~~~

How does that work? Well, the PostgreSQL catalogs contains useful hints to
answer that question:

~~~ sql
  SELECT CASE WHEN o.oprkind='l' THEN NULL
              ELSE pg_catalog.format_type(o.oprleft, NULL)
          END AS "Left arg type",
       
         CASE WHEN o.oprkind='r' THEN NULL
              ELSE pg_catalog.format_type(o.oprright, NULL)
          END AS "Right arg type",
       
         pg_catalog.format_type(o.oprresult, NULL) AS "Result type",
         
         oprcode::regprocedure as function
  
    FROM pg_catalog.pg_operator o
         LEFT JOIN pg_catalog.pg_namespace n ON n.oid = o.oprnamespace
         LEFT JOIN pg_catalog.pg_type l ON l.oid = o.oprleft
         
   WHERE o.oprname = '+'
     AND l.typname in ('integer', 'inet', 'date')
     AND pg_catalog.pg_operator_is_visible(o.oid)
    
ORDER BY 1, 2, 3, 4;
~~~

At query planning time, PostgreSQL does a catalog lookup that is equivalent
to our catalog query above. For optimisation purposes, the lookup is done
in-memory, using an efficient cache of the catalog contents, though.

Here's the result of our manual catalog search for the operator `+` and the
data types named `integer`, `inet`, and `date`:

~~~
 Left arg type │     Right arg type     │         Result type         │                 function                 
═══════════════╪════════════════════════╪═════════════════════════════╪══════════════════════════════════════════
 date          │ integer                │ date                        │ date_pli(date,integer)
 date          │ interval               │ timestamp without time zone │ date_pl_interval(date,interval)
 date          │ time with time zone    │ timestamp with time zone    │ datetimetz_pl(date,time with time zone)
 date          │ time without time zone │ timestamp without time zone │ datetime_pl(date,time without time zone)
 inet          │ bigint                 │ inet                        │ inetpl(inet,bigint)
(5 rows)
~~~

Each operator is then associated with a PostgreSQL function that embeds the
actual implementation of the expected behavior. Function themselves have
support for polymorphism, much as the *generic functions* mentionned above.
See the documentation part about [Function
Overloading](https://www.postgresql.org/docs/current/static/xfunc-overload.html).

In PostgreSQL the function overloading is adapted to the SQL engine static
typing needs: the type of a SQL query result must be computed entirely
before we run the query. Given that, a PostgreSQL function can be overloaded
only in such a way that every specialized implementation must return the
same data type.

That's the reason why each implementation of the `+` operator is using a
different function: we have a second level of overloading that takes place
at the operator level.

Included in PostgreSQL is the function `date_part` that implements the
[EXTRACT](https://www.postgresql.org/docs/current/static/functions-datetime.html#FUNCTIONS-DATETIME-EXTRACT)
standard syntax. As every specific implementation of the `date_part`
function returns the same result type, we can see function overloading here:

~~~
                                   List of functions
   Schema   │   Name    │ Result data type │        Argument data types        │  Type  
════════════╪═══════════╪══════════════════╪═══════════════════════════════════╪════════
 pg_catalog │ date_part │ double precision │ text, abstime                     │ normal
 pg_catalog │ date_part │ double precision │ text, date                        │ normal
 pg_catalog │ date_part │ double precision │ text, interval                    │ normal
 pg_catalog │ date_part │ double precision │ text, reltime                     │ normal
 pg_catalog │ date_part │ double precision │ text, time with time zone         │ normal
 pg_catalog │ date_part │ double precision │ text, time without time zone      │ normal
 pg_catalog │ date_part │ double precision │ text, timestamp with time zone    │ normal
 pg_catalog │ date_part │ double precision │ text, timestamp without time zone │ normal
(8 rows)
~~~

Here's an example of calling that function, where the expression `date
'today'` is of type *date* and the expression `now()` is of type _timestamp
with time zone_.

~~~ sql
select date_part('dow', date 'today') as dow,
       date_part('month', now()) as month;
~~~

Which, today, gives:

~~~
 dow │ month 
═════╪═══════
   4 │     3
(1 row)
~~~

As a user of PostgreSQL you can write functions that are using that
overloading mechanism, much like you do when writing Java, C++, Python, PHP,
or Ruby code, to list just some of the languages having polymorphism
features.

# Object-Relational Database Management System

PostgreSQL is an Object-Relational Database Management System. It's possible
to relate its object orientation to having implemented [table
inheritance](https://www.postgresql.org/docs/10/static/tutorial-inheritance.html),
which kind of looks like object inheritance in some ways.

This article shows that object systems also can be defined in terms of
[generic functions](https://en.wikipedia.org/wiki/Generic_function) that can
be implemented separately depending on the type of arguments used at run
time.

PostgreSQL provides a complete implementation of function overloading and
operator overloading and uses it a basis for advanced indexing support. A
PostgreSQL extension can define new data types and provide for their
indexing support, all without having to hack on PostgreSQL source code,
thanks to the extensibility support of the system. And several successful
PostgreSQL extension do just that, our best example being
[PostGIS](https://postgis.net).

PostgreSQL truely is the world's most advanced open source database.
PostgreSQL is YeSQL!

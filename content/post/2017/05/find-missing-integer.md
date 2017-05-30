+++
date = "2017-05-30T19:56:54+02:00"
title = "Find The Missing Integer"
categories = ["PostgreSQL","YeSQL"]
tags = ["PostgreSQL","YeSQL",
        "right join","generate_series",
        "anti join","except","not exists"]
coverImage = "/img/sum-to-100.jpg"
coverMeta = "out"
coverSize = "partial"
thumbnailImage = "/img/karl-friedrich-gauss.jpg"
thumbnailImagePosition = "left"

+++

A recent interview question that I had to review was spelled like this:

> Find missing int element into array 1..100

Of course at first read I got it wrong, you have only one integer to look
for into the array. So while the obvious idea was to apply classing
*sorting* techniques and minimize array traversal to handle complexity (time
and space), it turns out there's a much simpler way to do it if you remember
your math lessons from younger. But is is that much simpler?

<!--more-->
<!--toc-->

# Is it math or computer science?

The expected solution was to remember the Gauss trick that the sum of the
number from 1 to n is 1/2 * (n * n+1), that is 5050 in our case. If you sum
the numbers from the list missing one random element, then compute the
difference between that sum and 5050, you find the missing element:

~~~ common-lisp
(let* ((n       100)
       (all     (loop for i upto n collect i))
       (missing (remove (random n) all)))
  (- (* 1/2 n (+ n 1))
     (loop for i in missing sum i)))
54
~~~

# Back to computer science

While I can understand the *smarts* behind that approach, I would argue that
it's a *cleverer* approach, too smart for its own good.

My first question is going to be about integer overflow and how to handle
it, in cases when your target is not 100 elements but a lot more that this?

Then, what about having not one but two missing numbers in the list, or
maybe an unknown number of them? What about a list of something else than
numbers?

# Back to the drawing board

Rather than trying to write ourselves all the details of a good generic
algorithm that would work even with big numbers and then with unicode
strings and several missing items, let's try to use advanced tooling.

The general exercice we are up to now is called *set difference*, and
rightly available in the Common Lisp standard under that name:

~~~ lisp
(let* ((n       100)
       (all     (loop for i upto n collect i))
       (missing (remove (random n) all)))
  (set-difference all missing :test '=))
(66)
~~~

In python you can use
a [set](https://docs.python.org/3/tutorial/datastructures.html#sets) data
structure and then the code looks like this:

~~~ python
>>> all = {x for x in range(100)}
>>> missing = all.copy()
>>> missing.remove(int(random()*100))
>>> all - missing
set([86])
~~~

In both cases it's possible to manipulate *sets* containing an arbitrary
data type, not just integers, and we will find more than a single missing
entry. That's pretty good.

Let's note that in Common Lisp
the
[set-difference](http://www.lispworks.com/documentation/HyperSpec/Body/f_set_di.htm) function
accepts arguments `key` and `test` (and `test-not`) in order to be generic.
You can then pass as argument a `key` function that extracts the key you're
comparing (so that nested and complex data structures are taken care of),
and a `test` function to compare the values, it's not assumed to be `=`, it
could be something specific to your application. All the advanced list based
functions allow that in Common Lisp. Back to why that matters later.

# Where the data comes from?

If we want to generalize our approach here, we need to consider that maybe
the data is coming from the database, right? In which case, you might want
to avoid fetching it all in the client's program local memory to then
process it when all you need is the missing elements.

Can we code the *set difference* easily in SQL then? Of course we can, SQL
is all about working with sets. There's even more than one way to do it. We
are going to see the *set operation* technique, then the *anti join*
technique, and finally the *where not exists* technique.

Let's first setup the data set with a `create table` then `delete`
statement.

~~~ sql
> create table ints as
      select i
        from generate_series(1,100) as t(i);
SELECT 100

> with rnd(i) as (select (random()*100)::int)
  delete from ints using rnd where ints.i = rnd.i;
DELETE 1
~~~

## The SQL set operations

The SQL standard
includes
[SQL Set Operations](https://www.postgresql.org/docs/9.6/static/queries-union.html),
which allow for combining query results and consider them as sets. The most
known operation here is *union*. What we need today is *except*:

~~~ sql
> select i from generate_series(1,100) as t(i)
  except
  select i from ints;
 i
----
 55
(1 row)
~~~

The query plan is quite a direct mapping of how the query is written, you
can obtain it with the command `explain (costs off) <query here>` and here's
what we get:

~~~
                      QUERY PLAN                      
══════════════════════════════════════════════════════
 HashSetOp Except
   ->  Append
         ->  Subquery Scan on "*SELECT* 1"
               ->  Function Scan on generate_series t
         ->  Subquery Scan on "*SELECT* 2"
               ->  Seq Scan on ints
(6 rows)
~~~

## The Anti Join Technique

This technique is a *join* where you're interested into element that fail
the join condition, and can be written as in the following form:

~~~ sql
> select series.i
    from ints
         right join (select i
                       from generate_series(1,100) t(i)
                    ) series
                 on series.i = ints.i
   where ints.i is null;

 i
----
 55
(1 row)
~~~

The SQL statement then might look complex because it's using a *right join*
against a *subquery*. Remember that a *right join* is exactly the same thing
as a *left join*, the only difference being that the **reference relation**
is either on the *left* or on the *right* hand side of the *join* keyword.

I usually advice against using *right join* in production code, because of
its surprise factor. Your colleagues might not like to have to read the
PostgreSQL
[Table Expressions](https://www.postgresql.org/docs/current/static/queries-table-expressions.html) documentation
just because you felt *right* inclined that day.

Also you can notice that the *table expression* (or *relation*) we are
joining against is actually a query where we use `generate_series` to
produce the data we need at query time. The SQL from clause includes more
than just tables, it also supports join results and subqueries, and other
things. Think of it as a *relation* or a *data source* that you have server
side. That's what it is.

That query technique is named an *anti-join* as reported by the `explain
(costs off) <query here>` command in PostgreSQL:

~~~
                QUERY PLAN                
══════════════════════════════════════════
 Hash Anti Join
   Hash Cond: (t.i = ints.i)
   ->  Function Scan on generate_series t
   ->  Hash
         ->  Seq Scan on ints
(5 rows)
~~~

As its name suggests an *anti-join* allows to find rows that don't match
given the joining criteria, here `series.i = ints.i`. It means we find all
the rows that don't exist in the *series* data source. If we `delete`
another row in the *ints* table then we find all the missing entries by
running the same query again:

~~~ sql
> delete from ints where i = (random()*100)::int;
DELETE 1

> select series.i
    from ints
         right join (select i
                       from generate_series(1,100) t(i)
                    ) series
                 on series.i = ints.i
   where ints.i is null;

 i  
----
  6
 55
(2 rows)
~~~

Also, if we were to deal with any other data type (that has an equality
operator) then we could use the same query again and just happily find our
missing elements.

## The NOT EXISTS technique

Another way to solve our problem in SQL is using the *not exists*
construction, as shown in the following example:

~~~ sql
> select i
    from generate_series(1,100) as t(i)
   where not exists (select 1
                       from ints
                      where ints.i = t.i);
 i  
----
  6
 55
(2 rows)

~~~

You might be surprised by the way this query is written. For each of our
series from 1 to 100 we have a look into the *correlated subquery*
introduced by the `not exists` SQL construct, and we keep only rows for
which this subquery returns **no rows**. So the subquery is written to
return a single constant (the number 1 here) in the case where it finds
something, because we are *not* interested into matches here.

And the query plan is an *anti-join* again, exactly the same as in the
previous section:

~~~ 
                QUERY PLAN                
══════════════════════════════════════════
 Hash Anti Join
   Hash Cond: (t.i = ints.i)
   ->  Function Scan on generate_series t
   ->  Hash
         ->  Seq Scan on ints
(5 rows)
~~~

That's because PostgreSQL is smart enough to realise that both the writings
are actually meaning the exact same thing, so the *query optimizer* is now
finding the same best way to solve our query for us. And that's an *Hash
Anti Join* here, given the size of our data set and the lack of any
indexing.

## Comparing, sorting, computing hashes

All those SQL techniques are using the `=` operator in order to compare
items here. This operator is implemented by a different function for each
data type. Let's have a look at some of the comparison operators for
integers:

~~~ psql
> select format('%s(%s,%s)', o.oprname,
                             lt.typname,
                             rt.typname)
         as operator,
         oprcode::regprocedure as function
    from pg_operator o
         join pg_type rt on o.oprright = rt.oid
         join pg_type lt on o.oprleft = lt.oid
   where     o.oprkind = 'b'
         and o.oprname = '='
         and lt.typname ~ 'int';

         operator         |              function               
--------------------------+-------------------------------------
 =(int4,int8)             | int48eq(integer,bigint)
 =(int2,int2)             | int2eq(smallint,smallint)
 =(int4,int4)             | int4eq(integer,integer)
 =(int2vector,int2vector) | int2vectoreq(int2vector,int2vector)
 =(int8,int8)             | int8eq(bigint,bigint)
 =(int8,int4)             | int84eq(bigint,integer)
 =(int2,int4)             | int24eq(smallint,integer)
 =(int4,int2)             | int42eq(integer,smallint)
 =(tinterval,tinterval)   | tintervaleq(tinterval,tinterval)
 =(interval,interval)     | interval_eq(interval,interval)
 =(int2,int8)             | int28eq(smallint,bigint)
 =(int8,int2)             | int82eq(bigint,smallint)
(12 rows)
~~~

So while it might be hard to argue that SQL is object-oriented, depending on
your definition of the term, it clearly goes pretty far in being *generic*
and allowing *polymorphism* for its data types. Previously with *Common
Lisp* we got side tracked about the `key` and `test` arguments to
`set-difference`, and here's why it was important. We want a generic
solution that we can reuse easily, and with good performances
characteristics, and we see two different ways to obtain that.

This means that in our example, we can reuse the exact same query for any
datatype we have to deal with.

# Conclusion

In conclusion this little interview question got us thinking some more about
real world use cases and how to solve them with the best tool at hand.
Sometimes it's going to be *set-difference*, which is much better than
Gauss' trick because it's easy to read and because you can adjust it to
other data types and situation (not just a single missing number).

{{< image classes="fig25 right dim-margin"
              src="/img/old/sql-logo.png"
           title="PostgreSQL is YeSQL" >}}

Sometimes the dataset you're working with is already available in your
database server and then it might not be worth it to transfer the whole
dataset on the network and then store it in memory on the client machine
when a simple enough SQL query is going to be able to handle it for you,
right?

The message I want to draw attention to is that SQL is very powerful and
it's worth learning!

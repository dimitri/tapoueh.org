+++
date = "2013-08-27T17:35:00.000000+02:00"
title = "Auditing Changes with Hstore"
tags = ["PostgreSQL", "Triggers", "Extensions", "hstore", "YeSQL"]
categories = ["PostgreSQL","YeSQL"]
thumbnailImage = "/img/old/Audit-Conseil.jpg"
thumbnailImagePosition = "left"
coverImage = "/img/old/Audit-Conseil.jpg"
coverSize = "partial"
coverMeta = "out"
aliases = ["/blog/2013/08/27-auditing-changes-with-hstore",
           "/blog/2013/08/27-auditing-changes-with-hstore.html"]
+++

In a previous article about 
[Trigger Parameters](/blog/2013/08/23-parametrized-triggers) we have been using the
extension 
[hstore](http://www.postgresql.org/docs/9.2/static/hstore.html) in order to compute some extra field in our records, where
the fields used both for the computation and for storing the results were
passed in as 
*dynamic parameters*. Today we're going to see another 
*trigger*
use case for 
*hstore*: we are going to record changes made to our tuples.



## Comparing hstores

One of the operators that hstore propose is the 
`hstore - hstore` operator
whose documentation says that it will 
*delete matching pairs from left
operand*.

~~~
# select 'f1 => a, f2 => x'::hstore - 'f1 => b, f2 => x'::hstore as diff;
   diff    
-----------
 "f1"=>"a"
(1 row)
~~~


That's what we're going to use in our 
*changes auditing trigger* now, because
it's pretty useful a format to understand what did change.


## Auditing changes with a trigger

First we need some setup, a couple of tables to use in our worked out
example:

~~~
create table example
 (
   id   serial,
   f1   text,
   f2   text
 );

create table audit
 (
  change_date timestamptz default now(),
  before hstore,
  after  hstore
 );
~~~


The idea is to add a row in the 
`audit` table each time it is updated, with
the 
*hstore* representation of the data in flight before and after the change.
So as to avoid the problem of not being able to easily rebuild the current
value of a row at any time in the history, we're going to store a couple of
full 
*hstore* representations here.

~~~
create function audit()
  returns trigger
  language plpgsql
as $$
begin
  INSERT INTO audit(before, after)
       SELECT hstore(old), hstore(new);
  return new;
end;
$$;
~~~



{{< image classes="fig50 center fancybox dim-margin" src="/img/old/course-de-domino-bois.jpg" >}}


*I can't help but visualize triggers this way...*

Now, we need to attach the trigger to the table which is the source of our
events. Note that we could attach the same trigger to any table in fact, as
the details of the 
`audit` table has nothing specific about the 
`example` table.
If you want to do that, though, you will certainly want to add the name of
the source table of the event you're processing, available from within your
trigger as 
`TG_TABLE_NAME`. Oh and maybe add 
`TG_TABLE_SCHEMA` while at it!

Be sure to check the 
[PL/pgSQL Trigger Procedures](http://www.postgresql.org/docs/current/interactive/plpgsql-trigger.html) documentation.

~~~
create trigger audit
      after update on example
          for each row
 execute procedure audit();
~~~



## Testing it

With that in place, let's try it out:

~~~
insert into example(id, f1, f2) values(1, 'a', 'a');
update example set f1 = 'b' where id = 1;
update example set f2 = 'c' where id = 1;
~~~


And here's what we can see:


{{< image classes="fig50 center fancybox dim-margin" src="/img/old/engine-diff.jpg" >}}


*Another kind of differential*
~~~
# select change_date, after - before as diff from audit;
          change_date          |   diff    
-------------------------------+-----------
 2013-08-27 17:59:19.808217+02 | "f1"=>"b"
 2013-08-27 17:59:19.808217+02 | "f2"=>"c"
(2 rows)
~~~


The 
*hstore* extension is really useful and versatile, and we just saw another
use case for it!

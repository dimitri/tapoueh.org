+++
date = "2013-02-26T12:44:00.000000+01:00"
title = "HyperLogLog Unions"
tags = ["PostgreSQL", "Extensions", "YeSQL"]
categories = ["PostgreSQL","YeSQL"]
thumbnailImage = "/img/old/SetOperations.480.png"
thumbnailImagePosition = "left"
coverImage = "/img/old/SetOperations.480.png"
coverSize = "partial"
coverMeta = "out"
aliases = ["/blog/2013/02/26-hll-union",
           "/blog/2013/02/26-hll-union.html"]
+++

In the article from yesterday we talked about 
[PostgreSQL HyperLogLog](http://tapoueh.org/blog/2013/02/25-postgresql-hyperloglog.html) with
some details. The real magic of that extension has been skimmed over though,
and needs another very small article all by itself, in case you missed it.

<center>*Which Set Operation do you want for counting unique values?*</center>

The first query here has the default level of magic in it, really. What
happens is that each time we do an update of the 
*HyperLogLog* 
*hash* value, we
update some data which are allowing us to compute its cardinality.

~~~
=> select date,
          #users as daily,
          pg_column_size(users) as bytes
     from daily_uniques
 order by date;
    date    |      daily       | bytes 
------------+------------------+-------
 2013-02-22 | 401676.779509985 |  1287
 2013-02-23 | 660187.271908359 |  1287
 2013-02-24 | 869980.029947449 |  1287
 2013-02-25 | 580865.296677817 |  1287
 2013-02-26 | 240569.492722719 |  1287
(5 rows)
~~~


And has advertized the data is kept in a static sized data structure. The
magic here all happens at 
`hll_add()` time, the function you have to call to
update the data.

Now on to something way more magic!

<center>
{{< image classes="fig50 fancybox dim-margin" src="/img/old/aggregates2.jpg" >}}
</center>

<center>*Are those the aggregates you're looking for?*</center>

~~~
=> select to_char(date, 'YYYY/MM') as month,
          round(#hll_union_agg(users)) as monthly
     from daily_uniques group by 1;
  month  | monthly 
---------+---------
 2013/02 | 1960380
(1 row)
~~~


The 
*HyperLogLog* data structure is allowing the implementation of an 
***union***
algorithm that will be able to compute how many unique values you happen to
have registered in both one day and the next. Extended in its general form,
and doing SQL, what you get is an 
*aggregate* that you can use in 
`GROUP BY`
constructs and 
[window functions](http://www.postgresql.org/docs/9.2/static/tutorial-window.html). Did you read about them yet?

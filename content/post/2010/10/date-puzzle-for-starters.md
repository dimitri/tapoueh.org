+++
date = "2010-10-08T10:00:00.000000+02:00"
title = "Date puzzle for starters"
tags = ["PostgreSQL", "9.1", "tricks"]
categories = ["PostgreSQL"]
thumbnailImage = "/img/old/tips-and-tricks.jpg"
thumbnailImagePosition = "left"
coverImage = "/img/old/tips-and-tricks.jpg"
coverSize = "partial"
coverMeta = "out"
aliases = ["/blog/2010/10/08-date-puzzle-for-starters",
           "/blog/2010/10/08-date-puzzle-for-starters.html"]
+++

The 
[PostgreSQL](http://www.postgresql.org/) 
`IRC` channel is a good place to be, for all the very good help
you can get there, because people are always wanting to remain helpful,
because of the off-topics discussions sometime, or to get to talk with
community core members. And to start up your day too.

This morning's question started simple : “how can I check if today is the
"first sunday fo the month". or "the second tuesday of the month" etc?”

And the first version of the answer, quite simple it is too:

~~~
dim=#   with begin(d) as (select date_trunc('month', 'today'::date)::date) 
dim-# select d + 7 - extract(dow from d)::int as sunday from begin;
   sunday   
------------
 2010-10-03
(1 row)
~~~


So you just have to compare the result of the function with 
`'today'::date`
and there you go. The problem is that the question could be read in the
other way round, like, what is today in 
*first* or 
*second* 
*day name* of this
month 
*format*? Once more, 
[RhodiumToad](http://blog.rhodiumtoad.org.uk/) to the rescue:

~~~
select to_char(current_date,
               '"' || ((ARRAY['First','Second','Third','Fourth','Fifth'])
                             [(extract(day from current_date)::integer - 1)/7 + 1]
                      ) 
                   || '" Day');
     to_char      
------------------
 Second Friday   
(1 row)
~~~


That's a straight answer to the question, read that way!

But the part that I found nice to play with was my first reading of the
question, as I don't get to lose my ideas that easily, you see… so what
about writing a function to return the date of any 
*nth* occurrence of a given
*day of week* in a 
*given month*, defaulting to this very month?

~~~
create or replace function get_nth_dow_of_month
 (
  nth int,
  dow int,
  begin date default current_date
 )
 returns date
 language sql
 strict
 as
$$
with month(d) as (
  select generate_series(date_trunc('month', $3), 
                         date_trunc('month', $3) + interval '1 month - 1 day', 
                         interval '1 day')::date
), 
     repeat as (
  select d, extract(dow from d) as dow, (d - date_trunc('month', $3)::date) / 7 as repeat
    from month
) 
select d
  from repeat
 where dow = $2 and repeat = $1;
$$;

dim=# select get_nth_dow_of_month(0, 0);
 get_nth_dow_of_month 
----------------------
 2010-10-03
(1 row)

dim=# select get_nth_dow_of_month(1, 4, '2010-09-12');
 get_nth_dow_of_month 
----------------------
 2010-09-09
(1 row)
~~~


So you see we just got the first Sunday of this month 
`(0, 0)` and the second
Thursday 
`(1, 4)` of the previous one. Any date within a month is a good way
to tell which month you want to work in, as the function's written, abusing
`date_trunc` like it does.

Now the way the function is written is unfinished. You want to fix it in one
of two ways. Either stop using 
`generate_series` to only output one row at a
time, or fix the 
`API` so that you can ask for more than a 
*nth dow* at a
time. Of course, that was a starter for me, not a problem I need to solve
directly, and that was a good excuse for a blog entry, so I won't fix
it. That's left as an exercise to our interested readers!

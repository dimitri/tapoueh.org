+++
date = "2010-08-30T11:00:00.000000+02:00"
title = "Happy Numbers"
tags = ["PostgreSQL", "Emacs", "YeSQL"]
categories = ["PostgreSQL","YeSQL"]
thumbnailImage = "/img/old/1_mathematics_digits.gif"
thumbnailImagePosition = "left"
coverImage = "/img/old/1_mathematics_digits.gif"
coverSize = "partial"
coverMeta = "out"
aliases = ["/blog/2010/08/30-happy-numbers",
           "/blog/2010/08/30-happy-numbers.html"]
+++

After discovering the excellent 
[Gwene](http://gwene.org/) service, which allows you to subscribe
to 
*newsgroups* to read 
`RSS` content (
*blogs*, 
*planets*, 
*commits*, etc), I came to
read this nice article about 
[Happy Numbers](http://programmingpraxis.com/2010/07/23/happy-numbers/). That's a little problem that
fits well an interview style question, so I first solved it yesterday
evening in 
[Emacs Lisp](http://www.gnu.org/software/emacs/emacs-lisp-intro/html_node/List-Processing.html#List-Processing) as that's the language I use the most those days.

>   A happy number is defined by the following process. Starting with any
>   positive integer, replace the number by the sum of the squares of its
>   digits, and repeat the process until the number equals 1 (where it will
>   stay), or it loops endlessly in a cycle which does not include 1. Those
>   numbers for which this process ends in 1 are happy numbers, while those
>   that do not end in 1 are unhappy numbers (or sad numbers).


Now, what about implementing the same in pure 
`SQL`, for more fun? Now that's
interesting! After all, we didn't get 
`WITH RECURSIVE` for tree traversal
only, 
[did we](http://archives.postgresql.org/message-id/e08cc0400911042333o5361b21cu2c9438f82b1e55ce@mail.gmail.com)?

Unfortunately, we need a little helper function first, if only to ease the
reading of the recursive query. I didn't try to inline it, but here it goes:

~~~
create or replace function digits(x bigint)
  returns setof int
  language sql
as $$
  select substring($1::text from i for 1)::int
    from generate_series(1, length($1::text)) as t(i)
$$;
~~~


That was easy: it will output one row per digit of the input number — and
rather than resorting to powers of ten and divisions and remainders, we do
use plain old text representation and 
`substring`. Now, to the real
problem. If you're read what is an happy number and already did read the
fine manual about 
[Recursive Query Evaluation](http://www.postgresql.org/docs/8.4/interactive/queries-with.html), it should be quite easy to
read the following:

~~~
with recursive happy(n, seen) as (
    select 7::bigint, '{}'::bigint[]
  union all
    select sum(d*d), h.seen || sum(d*d)
      from (select n, digits(n) as d, seen
              from happy
           ) as h
  group by h.n, h.seen
    having not seen @> array[sum(d*d)]
)
  select * from happy;
  n  |       seen
-----+------------------
   7 | {}
  49 | {49}
  97 | {49,97}
 130 | {49,97,130}
  10 | {49,97,130,10}
   1 | {49,97,130,10,1}
(6 rows)

Time: 1.238 ms
~~~


That shows how it works for some 
*happy* number, and it's easy to test for a
non-happy one, like for example 
`17`. The query won't cycle thanks to the 
`seen`
array and the 
`having` filter, so the only difference between an 
*happy* and a
*sad* number will be that in the former case the last line output by the
recursive query will have 
`n = 1`. Let's expand this knowledge
into a proper function (because we want to be able to have the number we
test for happiness as an argument):

~~~
create or replace function happy(x bigint)
  returns boolean
  language sql
as $$
with recursive happy(n, seen) as (
    select $1, '{}'::bigint[]
  union all
    select sum(d*d), h.seen || sum(d*d)
      from (select n, digits(n) as d, seen
              from happy
           ) as h
  group by h.n, h.seen
    having not seen @> array[sum(d*d)]
)
  select n = 1 as happy
    from happy
order by array_length(seen, 1) desc nulls last
   limit 1
$$;
~~~


We need the 
`desc nulls last` trick in the 
`order by` because the 
`array_length()`
of any dimension of an empty array is 
`NULL`, and we certainly don't want to
return all and any number as unhappy on the grounds that the query result
contains a line 
`input, {}`. Let's now play the same tricks as in the puzzle
article:

~~~
=# select array_agg(x) as happy
     from generate_series(1, 50) as t(x)
    where happy(x);
              happy
----------------------------------
 {1,7,10,13,19,23,28,31,32,44,49}
(1 row)

Time: 24.527 ms

=# explain analyze select x
                     from generate_series(1, 10000) as t(x)
                    where happy(x);
                      QUERY PLAN
------------------------------------------------------------
 Function Scan on generate_series t
     (cost=0.00..265.00 rows=333 width=4)
     (actual time=2.938..3651.019 rows=1442 loops=1)
   Filter: happy((x)::bigint)
 Total runtime: 3651.534 ms
(3 rows)

Time: 3652.178 ms
~~~


(Yes, I tricked the 
`EXPLAIN ANALYZE` output so that it fits on the page width
here). For what it's worth, finding the first 
`10000` happy numbers in 
*Emacs
Lisp* on the same laptop takes 
`2830 ms`, also running a recursive version of
the code.


## Update, the Emacs Lisp version, inline:
~~~
(defun happy? (&optional n seen)
  "return true when n is a happy number"
  (interactive)
  (let* ((number    (or n (read-from-minibuffer
			   "Is this number happy: ")))
	 (digits    (mapcar
		     'string-to-int
		     (subseq (split-string number "") 1 -1)))
	 (squares   (mapcar (lambda (x) (* x x)) digits))
	 (happiness (apply '+ squares)))
    (cond ((eq 1 happiness)      t)
	  ((memq happiness seen) nil)
	  (t
	   (happy? (number-to-string happiness)
		   (push happiness seen))))))

(defun find-happy-numbers (&optional limit)
  "find all happy numbers from 1 to limit"
  (interactive)
  (let ((count (or limit
                   (read-from-minibuffer
		    "List of happy numbers from 1 to: ")))
	happy)
    (dotimes (n (string-to-int count))
      (when (happy? (number-to-string (1+ n)))
	(push (1+ n) happy)))
    (nreverse happy)))
~~~


+++
date = "2010-02-16T16:23:00.000000+01:00"
title = "Resetting sequences. All of them, please!"
tags = ["PostgreSQL", "catalogs"]
categories = ["PostgreSQL","Catalogs"]
thumbnailImage = "/img/old/library-card-catalogs.small.jpg"
thumbnailImagePosition = "left"
coverImage = "/img/old/library-card-catalogs.small.jpg"
coverSize = "partial"
coverMeta = "out"
aliases = ["/blog/2010/02/16-resetting-sequences-all-of-them-please",
           "/blog/2010/02/16-resetting-sequences-all-of-them-please.html"]
+++

So, after restoring a production dump with intermediate filtering, none of
our sequences were set to the right value. I could have tried to review the
process of filtering the dump here, but it's a 
*one-shot* action and you know
what that sometimes mean. With some pressure you don't script enough of it
and you just crawl more and more.

Still, I think how I solved it is worthy of a blog entry. Not that it's
about a super unusual 
*clever* trick, quite the contrary, because questions
involving this trick are often encountered on the support 
`IRC`. 

The idea is to query the catalog for all sequences, and produce from there
the 
`SQL` command you will have to issue for each of them. Once you have this
query, it's quite easy to arrange from the 
`psql` prompt as if you had dynamic
scripting capabilities. Of course in 
`9.0` you will have 
*inline anonymous* 
`DO`
blocks.

~~~
#> \o /tmp/sequences.sql
#> \t
Showing only tuples.
#> YOUR QUERY HERE
#> \o
#> \t
Tuples only is off.
~~~


Once you have the 
`/tmp/sequences.sql` file, you can ask 
`psql` to execute its
command as you're used to, that's using 
`\i` in an explicit transaction block.

Now, the interresting part if you got here attracted by the blog entry title
is in fact the query itself. A nice way to start is to 
`\set ECHO_HIDDEN` then
describe some table, you now have a catalog example query to work with. Then
you tweak it somehow and get this:

~~~
SELECT 'select ' 
        || trim(trailing ')' 
           from replace(pg_get_expr(d.adbin, d.adrelid),
                        'nextval', 'setval'))
        || ', (select max( ' || a.attname || ') from only '
        || nspname || '.' || relname || '));' 
  FROM pg_class c 
       JOIN pg_namespace n on n.oid = c.relnamespace 
       JOIN pg_attribute a on a.attrelid = c.oid
       JOIN pg_attrdef d on d.adrelid = a.attrelid 
                          and d.adnum = a.attnum
                          and a.atthasdef 
 WHERE relkind = 'r' and a.attnum > 0 
       and pg_get_expr(d.adbin, d.adrelid) ~ '^nextval';
~~~


Coming next, a 
`recode` based script in order to get from 
`SQL_ASCII` to 
`UTF-8`,
and some strange looking queries too.

~~~
recode.sh [-npdf0TI] [-U user ] -s schema [-m mintable] pattern
~~~


Stay tuned!

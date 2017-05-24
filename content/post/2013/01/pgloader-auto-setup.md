+++
date = "2013-01-17T14:32:00.000000+01:00"
title = "Automated Setup for pgloader"
tags = ["PostgreSQL", "pgloader", "catalogs"]
categories = ["Projects","pgloader"]
thumbnailImage = "/img/old/dauphin-logo.jpg"
thumbnailImagePosition = "left"
coverImage = "/img/old/dauphin-logo.jpg"
coverSize = "partial"
coverMeta = "out"
aliases = ["/blog/2013/01/17-pgloader-auto-setup",
           "/blog/2013/01/17-pgloader-auto-setup.html"]
+++

Another day, another migration from 
*MySQL* to 
[PostgreSQL](http://www.postgresql.org/)... or at least
that's how it feels sometimes. This time again I've been using some quite
old scripts to help me do the migration.

<center>*That's how I feel for MySQL users*</center>


## Migrating the schema

For the 
*schema* parts, I've been using 
[mysql2pgsql](http://pgfoundry.org/projects/mysql2pgsql/) with success for many
years. This tool is not complete and will do only about 
*80%* of the work. As
I think that the schema should always be validated manually when doing a
migration anyway, I happen to think that it's good news.


## Getting the data out

Then for the data parts I keep on using 
[pgloader](../../../pgsql/pgloader.html). The data is never quite
right, and the ability to filter out what you can't readily import in a
*reject* file proves itself a a must have here. The problems you have in the
exported MySQL data are quite serious:

<center>
{{< image classes="fig50 fancybox dim-margin" src="/img/old/data-unlocked.320.png" >}}
</center>

<center>*Can I have my data please?*</center>

First, date formating is not compatible with what PostgreSQL expects,
sometimes using 
`20130117143218` instead of what we expect: 
`2013-01-17
14:32:18`, and of course even when the format is right (that seems to depend
on the MySQL server's version), you still have to transform the 
`0000-00-00
00:00:00` into 
`NULL`.

>     Before thinking about the usage of that particular date rather than
>     using 


Then, text encoding is often mixed up, even when the MySQL databases are
said to be in 
*latin1* or 
*unicode*, you somehow always end up finding texts in
*win1252* or some other 
*code page* in there.

And of course, MySQL provides no tool to export the data to 
`CSV`, so you have
to come up with your own. The 
`SELECT INTO OUTFILE` command on the server
produces non conforming CSV (
`\n` can appear in non-escaped field contents),
and while the 
`mysql` client manual page details that it outputs 
`CSV` when
stdout is not a terminal, it won't even try to quote fields or escape 
`\t`
when they appear in the data.

So, we use the 
[mysqltocsv](https://github.com/slardiere/mysqltocsv) little script to export the data, and then use
that data to feed 
[pgloader](../../../pgsql/pgloader.html).


## Loading the data in

Now, we have to write down a configuration file for pgloader to know what to
load and where to find the data. What about generating the file from the
database schema instead, using the query in 
[generate-pgloader-config.sql](generate-pgloader-config.sql):

~~~
with reformat as (
   select relname, attnum, attname, typname,
          case typname
               when 'timestamptz'
               then attname || ':mynull:timestamp'
               when 'date'
               then attname || ':mynull:date'
           end as reformat
      from pg_class c
           join pg_namespace n on n.oid = c.relnamespace
           left join pg_attribute a on c.oid = a.attrelid
           join pg_type t on t.oid = a.atttypid
     where c.relkind = 'r'
           and attnum > 0
           and n.nspname = 'public'
),
 config_reformat as (
  select relname,
	 '['||relname||']' || E'\n' ||
	 'table = ' || relname || E' \n' ||
	 'filename = /path/to/csv/' || relname || E'.csv\n' ||
	 'format = csv' || E'\n' ||
	 'field_sep = \t' || E'\n' ||
	 'columns = *' || E' \n' ||
         'reformat = ' || array_to_string(array_agg(reformat), ', ')
         || E'\n' as config
    from reformat
   where reformat is not null
group by relname
),
 noreformat as (
   select relname, bool_and(reformat is null) as noreformating
     from reformat
 group by relname
),
 config_noreformat as (
  select relname,
	 '['||relname||']' || E'\n' ||
	 'table = ' || relname || E' \n' ||
	 'filename = /path/to/csv/' || relname || E'.csv\n' ||
	 'format = csv' || E'\n' ||
	 'field_sep = \t' || E'\n' ||
	 'columns = *' || E' \n'
         || E'\n' as config
    from reformat join noreformat using (relname)
   where noreformating
group by relname
),
allconfs as (
    select relname, config from config_reformat
 union all
    select relname, config from config_noreformat
)
select config
  from allconfs
 where relname not in ('tables', 'wedont', 'wantto', 'load')
 order by relname;
~~~


To work with the setup generated, you will have to prepend a global section
for pgloader and to include a reformating module in python, that I named
[mynull.py](mynull.py):

~~~
# Author: Dimitri Fontaine <dimitri@2ndQuadrant.fr>
#
# pgloader mysql reformating module

def timestamp(reject, input):
    """ Reformat str as a PostgreSQL timestamp

    MySQL timestamps are ok this time:  2012-12-18 23:38:12
    But may contain the infamous all-zero date, where we want NULL.
    """
    if input == '0000-00-00 00:00:00':
        return None

    return input

def date(reject, input):
    """ date columns can also have '0000-00-00'"""
    if input == '0000-00-00':
        return None

    return input
~~~


Now you can launch 
`pgloader` and profit!


## Conclusion

There are plenty of tools to assist you migrating away from MySQL and other
databases. When you make that decision, you're not alone, and it's easy
enough to find people to come and help you.

While MySQL is Open Source and is not a 
*lock in* from a licencing
perspective, I still find it hard to swallow that there's no provided tools
for getting data out in a sane format, and that so many little
inconsistencies exist in the product with respect to data handling (try to
have a 
`NOT NULL` column, then enjoy the default empty strings that have been
put in there). So at this point, yes, I consider that moving to 
[PostgreSQL](http://www.postgresql.org/)
is a way to 
*free your data*:

<center>
{{< image classes="fig50 fancybox dim-margin" src="/img/old/free-our-open-data.jpg" >}}
</center>

<center>*Free your data!*</center>

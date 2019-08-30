+++
title = "Setting up psql, the PostgreSQL CLI"
date = "2017-12-22T15:23:43+01:00"
tags = ["PostgreSQL","YeSQL","psql","setup"]
categories = ["PostgreSQL","YeSQL"]
coverImage = "/img/terminal-bg.jpg"
coverMeta = "in"
coverSize = "partial"
thumbnailImage = "/img/icon-cli.png"
thumbnailImagePosition = "left"
aliases = ["/blog/2017/12/setting-up-psql/"]

+++

PostgreSQL ships with an interactive console with the command line tool
named [psql](https://www.postgresql.org/docs/current/static/app-psql.html).
It can be used both for scripting and interactive usage and is moreover
quite a powerful tool. Interactive features includes *autocompletion*,
*readline* support (history searches, modern keyboard movements, etc), input
and output redirection, formatted output, and more.

<!--more-->

{{< figure class="right"
             src="/img/TAOP_Book_Cover_200x260.png"
            link="https://theartofpostgresql.com" >}}
            
> _This article is extracted from my book [The Art of
> PostgreSQL](https://theartofpostgresql.com). If you like what you read
> here, you might enjoy a full book of SQL learning material!_

<!--toc-->

New users of PostgreSQL often want to find an advanced visual query editing
tool and are confused when *psql* is the answer. Most PostgreSQL advanced
users and experts don't even think about it and use *psql*. In this chapter,
you will learn how to fully appreciate that little command line tool.

# Intro to psql

*psql* implements a REPL: the famous read-eval-print loop. It's one of the
best ways to interact with the computer when you're just learning and trying
things out. In the case of PostgreSQL you might be discovering a schema, a
data set, or just working on a query.

We often see the SQL query when it's fully formed, and rarely get to see the
steps that led us there. It's the same with code, most often what you get to
see is its final form, not the intermediary steps where the author tries
things and refine their understanding of the problem at hand, or the
environment in which to solve it.

The process to follow to get to a complete and efficient SQL query is the
same as when writing code: iterating from a very simple angle towards a full
solution to the problem at hand. Having a *REPL* environment offers an easy
way to build up on what you just had before.


# The psqlrc Setup

Here we begin with a full setup of *psql* and in the rest of the chapter, we
are going to get back to each important point separately. Doing so allows
you to have a fully working environment from the get-go and play around in
your PostgreSQL console while reading the book.

~~~ psql
\set PROMPT1 '%~%#%x '
\x auto
\set ON_ERROR_STOP on
\set ON_ERROR_ROLLBACK interactive
\set HISTFILE ~/.psql_history-:DBNAME

\set VERBOSITY verbose

-- \pset null '⦱'
\pset null '¤'
\pset linestyle 'unicode'

\pset unicode_border_linestyle single
\pset unicode_column_linestyle single
\pset unicode_header_linestyle double

set intervalstyle to 'postgres_verbose';

\setenv LESS '-iMFXSx4R'
\setenv EDITOR '/Applications/Emacs.app/Contents/MacOS/bin/emacsclient -nw'
~~~

Save that setup in the `~/.psqlrc` file, which is read at startup by the
*psql* application. As you've already read in the PostgreSQL documentation
for *psql*, we have three different settings to play with here:

  - `\set [ name [ value [ ... ] ] ]`
  
    This sets the psql variable name to value, or if more than one value is
    given, to the concatenation of all of them. If only one argument is
    given, the variable is set with an empty value. To unset a variable, use
    the `\unset` command.
  
  - `\setenv name [ value ]`
  
    This sets the environment variable name to value, or if the value is not
    supplied, unsets the environment variable.

    Here we use this facility to setup specific environment variables we
    need from within `psql`, such as the *LESS* setup. It allows invoking
    the *pager* for each result set but having it take the control of the
    screen only when necessary.
  
  - `\pset [ option [ value ] ]`
  
    This command sets options affecting the output of query result tables.
    *option* indicates which option is to be set. The semantics of *value*
    vary depending on the selected option. For some options, omitting
    *value* causes the option to be toggled or unset, as described under the
    particular option. If no such behavior is mentioned, then omitting
    *value* just results in the current setting being displayed.

Note that you will need to adapt the `EDITOR` setting to match your
preference in both editor and Operating System.

# Transactions and psql Behavior

In our case we set several psql variables that change its behavior:

  - `\set ON_ERROR_STOP on`
  
    The name is quite a good description of the option. It allows *psql* to
    know that it is not to continue trying to execute all your commands when
    a previous one is throwing an error. It's primarily practical for
    scripts and can be also set using the command line. As we'll see later,
    we can easily invoke scripts interactively within our session with the
    `\i` and `\ir` commands, so the option is still useful to us now.
  
  - `\set ON_ERROR_ROLLBACK interactive`

    This setting changes how *psql* behaves with respect to transactions. It
    is a very good interactive setup, and must be avoided in batch scripts.
    
    From the documentation: When set to on, if a statement in a transaction
    block generates an error, the error is ignored and the transaction
    continues. When set to interactive, such errors are only ignored in
    interactive sessions, and not when reading script files. When unset or
    set to off, a statement in a transaction block that generates an error
    aborts the entire transaction. The error rollback mode works by issuing
    an implicit SAVEPOINT for you, just before each command that is in a
    transaction block, and then rolling back to the savepoint if the command
    fails.

With the `\set PROMPT1 '%~%x%# '` that we are using, *psql* displays a
little star in the prompt when there's a transaction in flight, so you know
you need to finish the transaction. More importantly, when you want to type
in anything that will have a side effect on your database (modifying the
data set or the database schema), then without the star you know you need to
first type in `BEGIN`.

Let's see an example output with *ON_ERROR_ROLLBACK* set to off. Here's its
default value:

~~~ psql
f1db# begin;
BEGIN
f1db#* select 1/0;
ERROR:  division by zero
f1db#! select 1+1;
ERROR:  current transaction is aborted, commands ignored until end of transaction block
f1db#! rollback;
ROLLBACK
~~~

We have an error in our transaction, and we notice that the star prompt is
now a flag. The SQL transaction is marked invalid, and the only thing
PostgreSQL will now accept from us is to finish the transaction, with either
a *commit* or a *rollback* command. Both will result in the same result from
the server: `ROLLBACK`.

Now, let's do the same SQL transaction again, this time with
*ON_ERROR_ROLLBACK* being set to *interactive*. Now, before each command we
send to the server, *psql* sends a
[savepoint](https://www.postgresql.org/docs/current/static/sql-savepoint.html)
command, which allows it to then issue a [rollback to
savepoint](https://www.postgresql.org/docs/current/static/sql-rollback-to.html)
command in case of an error. This *rollback to savepoint* is also sent
automatically:

~~~ psql
f1db# begin;
BEGIN
f1db#* select 1/0;
ERROR:  division by zero
f1db#* select 1+1;
 ?column? 
══════════
        2
(1 row)

f1db#* commit;
COMMIT
~~~

Notice how this time not only do we get to send successful commands after
the error, while still being in a transaction — also we get to be able to
*COMMIT* our work to the server.

# A Reporting Tool

Getting familiar with *psql* is a very good productivity enhancer, so my
advice is to spend some quality time with the documentation of the tool and
get used to it. In this chapter, we are going to simplify things and help
you to get started.

There are mainly two use cases for *psql*, either as an interactive tool or
as a scripting and reporting tool. In the first case, the idea is that you
have plenty of commands to help you get your work done, and you can type in
SQL right in your terminal and see the result of the query.

In the scripting and reporting use case, you have advanced formatting
commands: it is possible to run a query and fetch its result directly in
either *asciidoc* or *HTML* for example, given `\pset format`. Say we have a
query that reports the N bests known results for a given driver surname. We
can use *psql* to set dynamic variables, display tuples only and format the
result in a convenient HTML output:

~~~ bash
~ psql --tuples-only      \
       --set n=1          \
       --set name=Alesi   \
       --no-psqlrc        \
       -P format=html     \
       -d f1db            \
       -f report.sql
~~~

~~~ html
<table border="1">
  <tr valign="top">
    <td align="left">Alesi</td>
    <td align="left">Canadian Grand Prix</td>
    <td align="right">1995</td>
    <td align="right">1</td>
  </tr>
</table>
~~~

It is also possible to set the connection parameters as environment
variables, or to use the same connection strings as in your application's
code, so you can test them with copy/paste easily, there's no need to
transform them into the `-d dbname -h hostname -p port -U username` syntax:

~~~ bash
~ psql -d postgresql://dim@localhost:5432/f1db
f1db# 

~ psql -d "user=dim host=localhost port=5432 dbname=f1db"
f1db#
~~~

The query in the `report.sql` file uses the `:'name'` variable syntax. Using
`:name` would be missing the quotes around the literal value injected, and
`:''` allows one to remedy this even with values containing spaces. *psql*
also supports `:"variable"` notation for double-quoting values, which is
used for dynamic SQL when identifiers are a parameter (column name or table
names).

~~~ sql
  select surname, races.name, races.year, results.position
    from results
         join drivers using(driverid)
         join races using(raceid)
   where drivers.surname = :'name'
         and position between 1 and 3
order by position
   limit :n;
~~~

When running *psql* for reports, it might be good to have a specific setup.
In this example, you can see I've been using the `--no-psqlrc` switch to be
sure we're not loading my usual interactive setup all with all the UTF-8
bells and whistles, and with *ON_ERROR_ROLLBACK*. Usually, you don't want to
have that set for a reporting or a batch script.

You might want to set *ON_ERROR_STOP* though, and maybe some other options.

# Unicode Borders

The `.psqlrc` file sets *linestyle* to *unicode* and some more parameters
related to the output style, so that we get the following output in our
console:

~~~
 surname │        name         │ year │ position 
═════════╪═════════════════════╪══════╪══════════
 Alesi   │ Canadian Grand Prix │ 1995 │        1
(1 row)
~~~

A useful trick when copy and pasting SQL result sets from the console to a
document, report, bug report or such is to limit the line length of the
results. You can achieve that with the following:

~~~ psql
\pset format wrapped
\pset columns 70
~~~

# Discovering a Schema

Let's get back to the interactive features of *psql*. The tool's main task
is to send SQL statements to the database server and display the result of
the query, and also server notifications and error messages. On top of that
*psql* provides a set of client-side commands all beginning with a
*backslash* character.

Most of the provided commands are useful for discovering a database schema.
All of them are implemented by doing one or several *catalog queries*
against the server. Again, it's sending a SQL statement to the server, and
it is possible for you to learn how to query the PostgreSQL catalogs by
reviewing those queries.

As an example, say you want to report the size of your databases but you
don't know where to look for that information. Reading the [psql
documentation](https://www.postgresql.org/docs/current/static/app-psql.html)
you find that the `\l+` command can do that, and now you want to see the SQL
behind it:

~~~ psql
~# \set ECHO_HIDDEN true
~# \l+
********* QUERY **********
SELECT d.datname as "Name",
       pg_catalog.pg_get_userbyid(d.datdba) as "Owner",
       pg_catalog.pg_encoding_to_char(d.encoding) as "Encoding",
       d.datcollate as "Collate",
       d.datctype as "Ctype",
       pg_catalog.array_to_string(d.datacl, E'\n') AS "Access privileges",
       CASE WHEN pg_catalog.has_database_privilege(d.datname, 'CONNECT')
            THEN pg_catalog.pg_size_pretty(pg_catalog.pg_database_size(d.datname))
            ELSE 'No Access'
       END as "Size",
       t.spcname as "Tablespace",
       pg_catalog.shobj_description(d.oid, 'pg_database') as "Description"
FROM pg_catalog.pg_database d
  JOIN pg_catalog.pg_tablespace t on d.dattablespace = t.oid
ORDER BY 1;
**************************

List of databases
...
~# \set ECHO_HIDDEN false
~~~

So now if you only want to have the database name and its on-disk size in
bytes, it is as easy as running the following query:

~~~ sql
  SELECT datname,
         pg_database_size(datname) as bytes
    FROM pg_database
ORDER BY bytes desc;
~~~

There's not much point in this book including the publicly available
documentation of all the commands available in *psql*, so go read the whole
manual page to find gems you didn't know about — there are plenty of them!

# Interactive Query Editor

You might have noticed that we did set the *EDITOR* environment variable
early in this section. This is the command used by *psql* each time you use
visual editing commands such as `\e`. This command launches your *EDITOR* on
the last edited query (or an empty one) in a temporary file, and will
execute the query once you end the editing session.

If you're using *emacs* or *vim* typing with a full-blown editor from within
a terminal, it is something you will be very happy to do. In other cases, it
is, of course, possible to set *EDITOR* to invoke your favorite IDE if your
*psql* client runs locally.

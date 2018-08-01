+++
date = "2017-05-30T14:11:33"
title = "Mastering psql"
categories = ["PostgreSQL","YeSQL"]
tags = ["PostgreSQL","YeSQL","psql"]
coverImage = "/images/foo.jpg"
coverMeta = "in"
coverSize = "partial"
thumbnailImage = "/images/foo.png"
thumbnailImagePosition = "left"
draft = true
+++

<!--more-->
<!--toc-->

https://www.postgresql.org/docs/current/static/app-psql.html

http://www.craigkerstiens.com/2013/02/21/more-out-of-psql/

## intro / behavior

transactions
timing

## interactive

PROMPT

\set ECHO_HIDDEN

\g
\gset
\gexec

\watch

\x expanded table formatting mode

\!
\cd
\copy

\crosstabview https://wiki.postgresql.org/wiki/Crosstabview

\errverbose

## interactive query editor

EDITOR
\e
\ef
\ev

\sf+
\sv+

## discovering a schema

## finding that systems function again

\dfS *xlog*
\set ECHO_HIDDEN

## save the output

\o

## scripts and reports

variables
includes (\i and \ir)
on_error_stop
tuples_only

--single-transaction
--single-step
--set

## display

formatted output (asciidoc, html, latex)
linestyle
interval

\watch

~~~
\set PROMPT1 '%~%x%# '
\x auto
\set PAGER off
\set ON_ERROR_STOP
\set ON_ERROR_ROLLBACK
\set VERBOSITY verbose

\pset null '⦱'
\pset linestyle 'unicode'
\pset unicode_border_linestyle single
\pset unicode_column_linestyle single
\pset unicode_header_linestyle double
set intervalstyle to 'postgres_verbose';
~~~

Note: we could be using `\setenv` instead

~~~ bash
# PostgreSQL setup
export LESS='-iMFXSx4R'
export PATH=$PATH:/Applications/Postgres.app/Contents/Versions/latest/bin

# From psql open queries with our Emacs
export EDITOR="/Applications/Emacs.app/Contents/MacOS/bin/emacsclient -nw"
~~~

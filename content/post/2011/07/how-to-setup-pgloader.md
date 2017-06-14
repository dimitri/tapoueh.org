+++
date = "2011-07-29T15:00:00.000000+02:00"
title = "How to Setup pgloader"
tags = ["PostgreSQL", "pgloader"]
categories = ["Projects","pgloader"]
thumbnailImage = "/img/old/toy-loader.320.jpg"
thumbnailImagePosition = "left"
coverImage = "/img/old/toy-loader.320.jpg"
coverSize = "partial"
coverMeta = "out"
aliases = ["/blog/2011/07/29-how-to-setup-pgloader",
           "/blog/2011/07/29-how-to-setup-pgloader.html"]
+++

In a previous article we
detailed
[how to use pgloader](http://tapoueh.org/blog/2011/07/22-how-to-use-pgloader.html),
let's now see how to write the `pgloader.conf` that
instructs [pgloader](http://pgloader.io) about what to
do.

<!--more-->

{{< alert danger >}}

This article is about versions 2.x of pgloader, which are not supported
anymore. Consider using [pgloader](http://pgloader.io) version 3.x instead.
Also the following examples are still available in the 3.x series and you
can see the *command files* at the GitHub repository for pgloader:

<https://github.com/dimitri/pgloader/blob/master/test/>.

{{< /alert >}}

<!--toc-->

This file is expected in the `INI` format, with a *global* section then one
section per file you want to import. The *global* section defines some
default options and how to connect to
the [PostgreSQL](http://tapoueh.org/pgsql/index.html) server.

The configuration setup is fully documented on the 
[pgloader man page](http://pgloader.projects.postgresql.org/) that
you can even easily find online.  As all 
*unix* style man pages, though, it's
more a complete reference than introductory material.  Let's review.


# global section

Here's the *global* section of
the
[examples/pgloader.conf](https://github.com/dimitri/pgloader/blob/master/examples/pgloader.conf) file
of the source files. Well, some options are *debugger* only options, really,
so I changed their value so that what you see here is a better starting
point.

~~~ ini
[pgsql]
base = pgloader

log_file            = /tmp/pgloader.log
log_min_messages    = INFO
client_min_messages = WARNING

lc_messages         = C
pg_option_client_encoding = 'utf-8'
pg_option_standard_conforming_strings = on
pg_option_work_mem = 128MB

copy_every      = 15000

null         = ""
empty_string = "\ "

max_parallel_sections = 4
~~~


You don't see all the connection setup, here `base` was enough. You might
need to setup `host`, `port` and `user`, and maybe even `pass`, too, to be
able to connect to the PostgreSQL server.

The logging options allows you to set a file where to log all `pgloader`
messages, that are categorized as either `DEBUG`, `INFO`, `WARNING`, `ERROR`
or `CRITICAL`. The options `log_min_messages` and `client_min_messages` are
another good idea stolen from [PostgreSQL](http://www.postgresql.org/) and
allow you to setup the level of chatter you want to see on the interactive
console (standard output and standard error streams) and on the log file.

Please note that the `DEBUG` level will produce more that 3 times as many
data as the data file you're importing. If you're not a `pgloader`
contributor or helping them, well, *debug* it, you want to avoid setting the
log chatter to this value.

The `client_encoding` will
be [SET](http://www.postgresql.org/docs/current/static/sql-set.html)
by [pgloader](http://tapoueh.org/pgsql/pgloader.html) on the PostgreSQL
connection it establish. You can now even set any parameter you want by
using the `pg_option_parameter_name` magic settings. Note that the command
line option `--pg-options` (or `-o` for brevity) allows you to override
that.

Then, the `copy_every` parameter is set to `5` in the examples, because the
test files are containing less than 10 lines and we want to test several
*batches* of commits when using them. So for your real loading, stick to
default parameters ( `10 000` lines per `COPY` command), or more. You can
play with this parameter, depending on the network (or local access) and
disk system you're using you might see improvements by reducing it or
enlarging it. There's no so much theory of operation as empirical testing
and setting here. For a one-off operation, just remove the lines from the
configuration.

The parameters `null` and `empty_string` are related to interpreting the
data in the text or `csv` files you have, and the documentation is quite
clear about them. Note that you have global setting and per-section setting
too.

The last parameter of this example, `max_parallel_sections`, is detailed
later in the article.


# files section

After the *global* section come as many sections as you have file to load.
Plus the *template* sections, that are only there so that you can share a
bunch of parameters in more than one section. Picture a series of data file
all of the same format, the only thing that will change is the `filename`.
Use a template section in this case!

Let's see an example:

~~~ ini
[simple_tmpl]
template     = True
format       = text
datestyle    = dmy
field_sep    = |
trailing_sep = True

[simple]
use_template    = simple_tmpl
table           = simple
filename        = simple/simple.data
columns         = a:1, b:3, c:2
skip_head_lines = 2

# those reject settings are defaults one
reject_log   = /tmp/simple.rej.log
reject_data  = /tmp/simple.rej

[partial]
table        = partial
format       = text
filename     = partial/partial.data
field_sep    = %
columns      = *
only_cols    = 1-3, 5
~~~


That's 2 of the examples from
the
[examples/pgloader.conf](https://github.com/dimitri/pgloader/blob/master/examples/pgloader.conf) file,
in 3 sections so that we see one template example. Of course, having a
single section using the template, it's just here for the example.


# data file format

The most important setting that you have to care about is the file format.
Your choice here is either `text`, `csv` or `fixed`. Mostly, what we are
given nowadays is `csv`. You might remember having read that the nice thing
about standards is that there's so many to choose from... well, the `csv`
land is one where it's pretty hard to find different producers that
understand it the same way.

So when you fail to have pgloader load your *mostly csv* files with a `csv`
setup, it's time to consider using `text` instead. The `text` file format
accept a lot of tunables to adapt to crazy situations, but is all `python`
code when the [python csv module](http://docs.python.org/library/csv.html)
is a C-coded module, more efficient.

If you're wondering what kind of format we're talking about here, here's
the
[cluttered pgloader example](https://github.com/dimitri/pgloader/blob/master/examples/cluttered/cluttered.data) for
your reading pleasure, using `^` (carret) as the field separator:

~~~ raw
1^some multi\
line text with\
newline escaping^and some other data following^
2^and another line^clean^
3^and\
a last multiline\
escaped line
with a missing\
escaping^just to test^
4^\ ^empty value^
5^^null value^
6^multi line\
escaped value\
\
with empty line\
embeded^last line^
~~~


And here's what we get by loading that:

~~~ psql
pgloader/examples$ pgloader -c pgloader.conf -s cluttered
Table name        |    duration |    size |  copy rows |     errors 
====================================================================
cluttered         |      0.193s |       - |          6 |          0

pgloader/examples$ psql pgloader -c "table cluttered;"
 a |               b               |        c         
---+-------------------------------+------------------
 1 | and some other data following | some multi
                                   : line text with
                                   : newline escaping
 2 | clean                         | and another line
 3 | just to test                  | and
                                   : a last multiline
                                   : escaped line
                                   : with a missing
                                   : escaping
 4 | empty value                   | 
 5 | null value                    | 
 6 | last line                     | multi line
                                   : escaped value
                                   : 
                                   : with empty line
                                   : embeded
(6 rows)
~~~


So when you have such kind of data, well, it might be that `pgloader` is
still able to help you!

Please refer to
the [pgloader man page](http://pgloader.projects.postgresql.org/) to know
about each and every parameter that you can define and the values accepted,
etc. And the *fixed* data format is to be used when you're not given a field
separator but field positions in the file. Yes, we still encounter those
from time to time. Who needs variable size storage, after all?

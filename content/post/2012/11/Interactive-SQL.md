+++
date = "2012-11-06T09:55:00.000000+01:00"
title = "Editing SQL"
tags = ["PostgreSQL", "Emacs", "pgdevenv-el"]
categories = ["Emacs","Emacs Tips"]
thumbnailImage = "/img/old/pg-el.png"
thumbnailImagePosition = "left"
coverImage = "/img/old/pg-el.png"
coverSize = "partial"
coverMeta = "out"
aliases = ["/blog/2012/11/06-Interactive-SQL",
           "/blog/2012/11/06-Interactive-SQL.html"]
+++

It's hard to read my blog yet not know I'm using 
[Emacs](http://www.gnu.org/software/emacs/#Platforms). It really is a great
tool and has a lot to compare to 
[PostgreSQL](http://www.postgresql.org/) in terms of extensibility,
documentation quality and community. And there's even a native
implementation of the 
[PostgreSQL Protocol](http://www.postgresql.org/docs/current/static/protocol.html) written in 
[Emacs Lisp](http://www.gnu.org/software/emacs/emacs-lisp-intro/).

<center>
<div class="figure dim-margin">
  <a href="http://www.online-marketwatch.com/pgel/pg.html">
    <img src="/img/old/pg-el.png">
  </a>
</div>
</center>

One of the things where 
*Emacs* really shines is that interactive development
environment you get when working on some 
*Emacs Lisp* code. Evaluating an
function as easy as a single 
*key chord*, and that will both compile in the
function and load it in the running process. I can't tell you how many times
I've been missing that ability when editing C code.

With 
*PostgreSQL* too we get a pretty interactive environment with the 
[psql](http://www.postgresql.org/docs/current/static/app-psql.html)
console application, or with 
[pgAdmin](http://www.pgadmin.org/). One feature from 
*pgAdmin* that I've
often wished I had in 
*psql* is the ability to edit my query online and easily
run it in the console, rather than either using the 
*readline* limited history
editing features or launching a new editor process each time with 
`\e`. At the
same time I would much prefer using my usual 
*Emacs* editor to actually 
*edit*
the query.

If you've been reading that blog before you know what to expect. My solution
to the stated problem is available in 
[pgdevenv-el](https://github.com/dimitri/pgdevenv-el), an 
*Emacs* package aimed at
helping 
*PostgreSQL* developers. Most of the features in there are geared
toward the 
*core backend* developers, except for this one I want to talk about
today (I'll blog about the other ones too I guess).

<center>
{{< image classes="fig50 fancybox dim-margin" src="/img/old/pgdevenv-el-eval-sql.png" >}}
</center>

What you can see from that screenshot is that the selected query text has
been sent to the 
*psql* buffer and exectuted over there. And that the 
*psql*
buffer is echoing all queries sent to it. What you can not see straight from
that picture is the interaction to get there. Well, I've been implementing
some 
*elisp* features that I was missing.

First, movement: you can do 
`C-M-a` and 
`C-M-e` to navigate to the beginning and
the end of the SQL query at point, like you do in 
`C` or in 
`lisp` in 
*Emacs*.

Then, selection: you can do 
`C-M-h` to select the SQL query at point, you
don't have to navigate yourself, 
[pgdev-sql-mode](https://github.com/dimitri/pgdevenv-el) knows how to do that. Side
note, 
`pgdev-sql-mode` is the name of the 
*minor mode* you need to activate in
your SQL buffers to have the magic available.

Last but not least, evaluation: as when editing lisp code, you can now use
`C-M-x` to send the current query text to an associated 
*psql* buffer.

The way to associate the 
*psql* buffer to an 
*SQL* buffer is currently done
thanks to the other 
*pgdevenv-el* features that this blog post is not talking
about, and the setup is addressed in the documentation: you have to let know
*pgdevenv-el* where your PostgreSQL branches are installed locally so that it
can prepare you a 
*Shell* buffer with 
`PGDATA` and 
`PGPORT` already set for you.
And currently, for 
`C-M-x` to work you need to open the buffer yourself before
hand, using 
`C-c - n` (to run the command 
`pgdev-open-shell`), and type 
`psql` in
the 
*Shell* prompt.

What that means for me is that I can at least edit SQL (in 
*PostgreSQL*
regression files and other places) in my usual 
*Emacs* buffer and actually
refine it as I go until it does exactly what I need, without having to use
the 
*readline* history editing or the 
`\e` command, which is not great when your
*Shell* is in already running inside 
*Emacs*.

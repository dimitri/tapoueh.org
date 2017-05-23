+++
date = "2010-10-15T11:30:00.000000+02:00"
title = "Extensions: writing a patch for PostgreSQL"
tags = ["PostgreSQL", "Extensions", "backup"]
categories = ["PostgreSQL","Extensions"]
thumbnailImage = "/img/postgresql-512.jpg"
thumbnailImagePosition = "left"
coverImage = "/img/postgresql-512.jpg"
coverSize = "partial"
coverMeta = "out"
aliases = ["/blog/2010/10/15-extensions-writing-a-patch-for-postgresql",
           "/blog/2010/10/15-extensions-writing-a-patch-for-postgresql.html"]
+++

These days, thanks to my 
[community oriented job](http://2ndquadrant.com/), I'm working full time on a
[PostgreSQL](http://www.postgresql.org/) patch to terminate basic support for 
[extending SQL](http://www.postgresql.org/docs/9/static/extend.html). First thing I
want to share is that patching the 
*backend code* is not as hard as one would
think. Second one is that 
[git](http://git-scm.com/) really is helping.

*“Not as hard as one would think*, are you kidding me?”, I hear some
say. Well, that's true. It's 
`C` code in there, but with a very good layer of
abstractions so that you're not dealing with subtle problems that much. Of
course it happens that you have to, and managing the memory isn't an
option. That said, 
`palloc()` and the 
*memory contexts* implementation makes
that as easy as 
*in lots of cases, you don't have to think about it*.

PostgreSQL is very well known for its reliability, and that's not something
that just happened. All the source code is organized in a way that makes it
possible, so your main task is to write code that looks as much as possible
like the existing surrounding code. And we all know how to 
*copy paste*,
right?

So, my current work on the 
*extensions* is to make it so that if you install
[hstore](http://www.postgresql.org/docs/9.0/interactive/hstore.html) in your database (to pick an example), your backup won't contain any
*hstore* specific objects (types, functions, operators, index support objects,
etc) but rather a single line that tells PostgreSQL to install 
*hstore* again.

~~~
CREATE EXTENSION hstore;
~~~


The feature already works in 
[my git branch](http://git.postgresql.org/gitweb?p=postgresql-extension.git;a=shortlog;h=refs/heads/extension) and I'm extracting infrastructure
work in there to ease review. That's when 
`git` helps a lot. What I've done is
create a new branch from the master one, then 
[cherry pick](http://www.kernel.org/pub/software/scm/git/docs/git-cherry-pick.html) the patches of
interest. Well sometime you have to resort to helper tools. I've been told
after the fact that using 
`git cherry-pick -n` would have allowed the
following to be much simpler:

~~~
dim ~/dev/PostgreSQL/postgresql-extension git cherry-pick 3f291b4f82598309368610431cf2a18d7b7a7950
error: could not apply 3f291b4... Implement dependency tracking for CREATE EXTENSION, and DROP EXTENSION ... CASCADE.
hint: after resolving the conflicts, mark the corrected paths
hint: with 'git add <paths>' or 'git rm <paths>'
hint: and commit the result with 'git commit -c 3f291b4'
dim ~/dev/PostgreSQL/postgresql-extension git status \
| awk '/modified/ && ! /both/ && ! /genfile/ {print $3}
       /deleted/ {print $5}
       /both/    {print $4}' \
| xargs echo git reset -- \
| sh
Unstaged changes after reset:
M	src/backend/catalog/dependency.c
M	src/backend/catalog/heap.c
M	src/backend/catalog/pg_aggregate.c
M	src/backend/catalog/pg_conversion.c
M	src/backend/catalog/pg_namespace.c
M	src/backend/catalog/pg_operator.c
M	src/backend/catalog/pg_proc.c
M	src/backend/catalog/pg_type.c
M	src/backend/commands/extension.c
M	src/backend/commands/foreigncmds.c
M	src/backend/commands/opclasscmds.c
M	src/backend/commands/proclang.c
M	src/backend/commands/tsearchcmds.c
M	src/backend/nodes/copyfuncs.c
M	src/backend/nodes/equalfuncs.c
M	src/backend/parser/gram.y
M	src/include/catalog/dependency.h
M	src/include/commands/extension.h
M	src/include/nodes/parsenodes.h
~~~


That's what I did to prepare a side branch containing only changes to a part
of my current work. I had to filter the diff so much only because I'm
commiting in rather big steps, rather than very little chunks at a time. In
this case that means I had a single patch with several 
*units* of changes and
I wanted to extract only one. Well, it happens that even in such a case, 
`git`
is helping!

There's more to say about the 
*extension* related feature of course, but
that'll do it for this article. I'd just end up with the following nice
*diffstat* of 4 days of work:

~~~
dim ~/dev/PostgreSQL/postgresql-extension git --no-pager diff master..|wc -l
    3897
dim ~/dev/PostgreSQL/postgresql-extension git --no-pager diff master..|diffstat
 doc/src/sgml/extend.sgml               |   46 ++
 doc/src/sgml/ref/allfiles.sgml         |    2 
 doc/src/sgml/ref/create_extension.sgml |   95 ++++
 doc/src/sgml/ref/drop_extension.sgml   |  115 +++++
 doc/src/sgml/reference.sgml            |    2 
 src/backend/access/transam/xlog.c      |   95 ----
 src/backend/catalog/Makefile           |    1 
 src/backend/catalog/dependency.c       |   25 +
 src/backend/catalog/heap.c             |    9 
 src/backend/catalog/objectaddress.c    |   14 
 src/backend/catalog/pg_aggregate.c     |    7 
 src/backend/catalog/pg_conversion.c    |    7 
 src/backend/catalog/pg_namespace.c     |   13 
 src/backend/catalog/pg_operator.c      |    7 
 src/backend/catalog/pg_proc.c          |    7 
 src/backend/catalog/pg_type.c          |    8 
 src/backend/commands/Makefile          |    3 
 src/backend/commands/comment.c         |    6 
 src/backend/commands/extension.c       |  688 +++++++++++++++++++++++++++++++++
 src/backend/commands/foreigncmds.c     |   19 
 src/backend/commands/functioncmds.c    |    7 
 src/backend/commands/opclasscmds.c     |   13 
 src/backend/commands/proclang.c        |    7 
 src/backend/commands/tsearchcmds.c     |   25 +
 src/backend/nodes/copyfuncs.c          |   22 +
 src/backend/nodes/equalfuncs.c         |   18 
 src/backend/parser/gram.y              |   51 ++
 src/backend/tcop/utility.c             |   27 +
 src/backend/utils/adt/genfile.c        |  193 +++++++++
 src/backend/utils/init/postinit.c      |    3 
 src/backend/utils/misc/Makefile        |    2 
 src/backend/utils/misc/cfparser.c      |  113 +++++
 src/backend/utils/misc/guc-file.l      |   26 -
 src/backend/utils/misc/guc.c           |  160 ++++++-
 src/bin/pg_dump/common.c               |    6 
 src/bin/pg_dump/pg_dump.c              |  520 ++++++++++++++++++++++--
 src/bin/pg_dump/pg_dump.h              |   10 
 src/bin/pg_dump/pg_dump_sort.c         |    7 
 src/bin/psql/command.c                 |    3 
 src/bin/psql/describe.c                |   45 ++
 src/bin/psql/describe.h                |    3 
 src/bin/psql/help.c                    |    1 
 src/include/catalog/dependency.h       |    1 
 src/include/catalog/indexing.h         |    6 
 src/include/catalog/pg_extension.h     |   61 ++
 src/include/catalog/pg_proc.h          |   13 
 src/include/catalog/toasting.h         |    1 
 src/include/commands/extension.h       |   54 ++
 src/include/nodes/nodes.h              |    2 
 src/include/nodes/parsenodes.h         |   20 
 src/include/parser/kwlist.h            |    1 
 src/include/utils/builtins.h           |    4 
 src/include/utils/cfparser.h           |   18 
 src/include/utils/guc.h                |   11 
 src/makefiles/pgxs.mk                  |   21 -
 55 files changed, 2456 insertions(+), 188 deletions(-)
~~~


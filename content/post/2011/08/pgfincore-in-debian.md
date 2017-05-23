+++
date = "2011-08-19T23:00:00.000000+02:00"
title = "pgfincore in debian"
tags = ["PostgreSQL", "pgfincore", "debian", "Extensions"]
categories = ["debian"]
thumbnailImage = "/img/old/debian-logo.png"
thumbnailImagePosition = "left"
coverImage = "/img/old/debian-logo.png"
coverSize = "partial"
coverMeta = "out"
aliases = ["/blog/2011/08/19-pgfincore-in-debian",
           "/blog/2011/08/19-pgfincore-in-debian.html"]
+++

As of pretty recently, 
[pgfincore](http://villemain.org/projects/pgfincore) is now in debian, as you can see on its
[postgresql-9.0-pgfincore](http://packages.debian.org/sid/postgresql-9.0-pgfincore) page.  The reason why it entered the 
[debian](http://www.debian.org/)
archives is that it reached the 
`1.0` release!

Rather than talking about what 
*pgfincore* is all about (
*A set of functions to
manage pages in memory from PostgreSQL*), I will talk about its packaging and
support as a 
*debian package*.  Here's the first example of a modern
multi-version packaging I have to offer.  
[pgfincore packaging](https://github.com/dimitri/pgfincore/tree/master/debian) supports
building for 
`8.4` and 
`9.0` and 
`9.1` out of the box, even if the only binary
you'll find in 
*debian* sid is the 
`9.0` one, as you can check on the
[pgfincore debian source package](http://packages.debian.org/source/sid/pgfincore) page.

Also, this is the first package I've done properly using the newer version
of 
[debhelper](http://kitenet.net/~joey/code/debhelper/), which make the 
[debian/rules](https://github.com/dimitri/pgfincore/blob/master/debian/rules) file easier than ever.  Let's have
a look at it:

~~~
SRCDIR = $(CURDIR)
TARGET = $(CURDIR)/debian/pgfincore-%v
PKGVERS = $(shell dpkg-parsechangelog | awk -F '[:-]' '/^Version:/ { print substr($$2, 2) }')
EXCLUDE = --exclude-vcs --exclude=debian

include /usr/share/postgresql-common/pgxs_debian_control.mk

override_dh_auto_clean: debian/control
	pg_buildext clean $(SRCDIR) $(TARGET) "$(CFLAGS)"
	dh_clean 

override_dh_auto_build:
	# build all supported version
	pg_buildext build $(SRCDIR) $(TARGET) "$(CFLAGS)"

override_dh_auto_install: 
	# then install each of them
	for v in `pg_buildext supported-versions $(SRCDIR)`; do \
		dh_install -ppostgresql-$$v-pgfincore ;\
	done

orig: clean
	cd .. && tar czf pgfincore_$(PKGVERS).orig.tar.gz $(EXCLUDE) pgfincore

%:
	dh $@
~~~


The 
`debian/rules` file is known to be the corner stone of your debian
packaging, and usually is the most complex part of it.  It's a 
`Makefile` at
its heart, and we can see that thanks to the 
`debhelper` magic it's not that
complex to maintain anymore.

Then, this file is using support from a bunch of helpers command, each of
them comes with its own man page and does a little part of the work.  The
overall idea around 
`debhelper` is that what it does covers 90% of the cases
around, and it's not aiming for more.  You have to 
*override* the parts where
it defaults to being wrong.

Here for example the build system has to produce files for all three
supported versions of 
[PostgreSQL](http://www.postgresql.org/), which means invoking the same build system
three time with some changes in the 
*environment* (mainly setting the
`PG_CONFIG` variable correctly).  But even for that we have a 
*debian* facility,
that comes in the package 
[postgresql-server-dev-all](http://packages.debian.org/sid/postgresql-server-dev-all), called 
`pg_buildext`.  As
long as your extension build system is 
`VPATH` friendly, it's all automated.

Please read that last sentence another time.  
`VPATH` is the thing that allows
`Make` to find your source tree somewhere in the system, not in the current
working directory.  That allows you to cleanly build the same sources in
different build locations, which is exactly what we need here, and is
cleanly supported by 
[PGXS](http://www.postgresql.org/docs/9.1/static/extend-pgxs.html), the 
[PostgreSQL Extension Building Infrastructure](http://www.postgresql.org/docs/9.1/static/extend-pgxs.html).

Which means that the main 
`Makefile` of 
*pgfincore* had to be simplified, and
the code layout too.  Some advances 
`Make` features such as 
`$(wildcard ...)`
and all will not work here.  See what we got at the end:

~~~
ifndef VPATH
SRCDIR = .
else
SRCDIR = $(VPATH)
endif

EXTENSION    = pgfincore
EXTVERSION   = $(shell grep default_version $(SRCDIR)/$(EXTENSION).control | \
               sed -e "s/default_version[[:space:]]*=[[:space:]]*'\([^']*\)'/\1/")

MODULES      = $(EXTENSION)
DATA         = sql/pgfincore.sql sql/uninstall_pgfincore.sql
DOCS         = doc/README.$(EXTENSION).rst

PG_CONFIG    = pg_config

PG91         = $(shell $(PG_CONFIG) --version | grep -qE "8\.|9\.0" && echo no || echo yes)

ifeq ($(PG91),yes)
all: pgfincore--$(EXTVERSION).sql

pgfincore--$(EXTVERSION).sql: sql/pgfincore.sql
	cp $< $@

DATA        = pgfincore--unpackaged--$(EXTVERSION).sql pgfincore--$(EXTVERSION).sql
EXTRA_CLEAN = sql/$(EXTENSION)--$(EXTVERSION).sql
endif

PGXS := $(shell $(PG_CONFIG) --pgxs)
include $(PGXS)

deb:
	dh clean
	make -f debian/rules orig
	debuild -us -uc -sa
~~~


No more 
`Make` magic to find source files.  Franckly though, when your sources
are 1 
`c` file and 2 
`sql` files, you don't need that much magic anyway.  You
just want to believe that a single generic 
`Makefile` will happily build any
project you throw at it, only requiring minor adjustment.  Well, the reality
is that you might need some more little adjustments if you want to benefit
from 
`VPATH` building, and having the binaries for 
`8.4` and 
`9.0` and 
`9.1` built
seemlessly in a simple loop.  Like we have here for 
*pgfincore*.

Now the 
`Makefile` still contains a little bit of magic, in order to parse the
extension version number from its 
*control file* and produce a 
*script* named
accordingly.  Then you'll notice a difference between the
[postgresql-9.1-pgfincore.install](https://github.com/dimitri/pgfincore/blob/master/debian/postgresql-9.1-pgfincore.install) file and the
[postgresql-9.0-pgfincore.install](https://github.com/dimitri/pgfincore/blob/master/debian/postgresql-9.0-pgfincore.install).  We're just not shipping the same files:

~~~
debian/pgfincore-9.0/pgfincore.so usr/lib/postgresql/9.0/lib
sql/pgfincore.sql usr/share/postgresql/9.0/contrib
sql/uninstall_pgfincore.sql usr/share/postgresql/9.0/contrib
~~~


As you can see here:

~~~
debian/pgfincore-9.1/pgfincore.so usr/lib/postgresql/9.1/lib
debian/pgfincore-9.1/pgfincore*.sql usr/share/postgresql/9.1/extension
sql/pgfincore--unpackaged--1.0.sql usr/share/postgresql/9.1/extension
~~~


So, now that we uncovered all the relevant magic, packaging and building
your next extension so that it supports as many PostgreSQL major releases as
you need to will be that easy.

For reference, you might need to also tweak
`/usr/share/postgresql-common/supported-versions` so that it allows you to
build for all those versions you claim to support in the 
[debian/pgversions](https://github.com/dimitri/pgfincore/blob/master/debian/pgversions)
file.

~~~
$ sudo dpkg-divert \
--divert /usr/share/postgresql-common/supported-versions.distrib \
--rename /usr/share/postgresql-common/supported-versions

$ cat /usr/share/postgresql-common/supported-versions
#! /bin/bash

dpkg -l postgresql-server-dev-* \
| awk -F '[ -]' '/^ii/ && ! /server-dev-all/ {print $6}'
~~~


All of this will come pretty handy when we finally sit down and work on a
way to provide binary packages for PostgreSQL and its extensions, and all
supported versions of those at that.  This very project is not dead, it's
just sleeping some more.

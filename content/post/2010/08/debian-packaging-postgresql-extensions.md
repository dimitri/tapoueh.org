+++
date = "2010-08-06T13:00:00.000000+02:00"
title = "debian packaging PostgreSQL extensions"
tags = ["PostgreSQL", "debian", "Extensions", "release", "prefix"]
categories = ["Projects","prefix"]
thumbnailImage = "/img/old/debian-logo.png"
thumbnailImagePosition = "left"
coverImage = "/img/old/debian-logo.png"
coverSize = "partial"
coverMeta = "out"
aliases = ["/blog/2010/08/06-debian-packaging-postgresql-extensions",
           "/blog/2010/08/06-debian-packaging-postgresql-extensions.html"]
+++

In trying to help an extension 
*debian packaging* effort, I've once again
proposed to handle it. That's because I now begin to know how to do it, as
you can see in my 
[package overview](http://qa.debian.org/developer.php?login=dim%40tapoueh.org) page at 
*debian QA* facility. There's a
reason why I proposed myself here, it's that yet another tool of mine is now
to be found in 
*debian*, and should greatly help 
*extension packaging*
there. You can already check for the 
[postgresql-server-dev-all](http://packages.debian.org/sid/postgresql-server-dev-all) package page
if you're that impatient!

Back? Ok, so I used to have two main gripes against debian support for
[PostgreSQL](http://www.postgresql.org/). The first one, which is now feeling alone, is that both project
[release support policy](http://wiki.postgresql.org/wiki/PostgreSQL_Release_Support_Policy) aren't compatible enough for debian stable to include
all currently supported stable PostgreSQL major version. That's very bad
that debian stable will only propose one major version, knowing that the
support for several of them is in there.

The problem is two fold: first, debian stable has to maintain any
distributed package. There's no 
*deprecation policy* allowing for droping the
ball. So the other side of this coin is that debian developers must take on
themselves maintaining included software for as long as stable is not
renamed 
`oldstable`. And it so happens that there's no debian developer that
feels like maintaining 
*end of lined* PostgreSQL releases without help from
[PostgreSQL Core Team](http://www.postgresql.org/community/contributors/). Or, say, without official statement that they would
help.

Now, why I don't like this situation is because I'm pretty sure there's very
few software development group offering as long and reliable maintenance
policy as PostgreSQL is doing, but debian will still happily distribute
*unknown-maintenance-policy* pieces of code in its stable repositories. So the
*uncertainty* excuse is rather poor. And highly frustrating.

>   


The consequence of this fact leads to my second main gripe against debian
support for PostgreSQL: the extensions. It so happens that the PostgreSQL
extensions are developped for supporting several major versions from the
same source code. So typically, all you need to do is recompile the
extension against the new major version, and there you go.

Now, say debian new stable is coming with 
[8.4](http://packages.debian.org/squeeze/postgresql-8.4) rather than 
[8.3](http://packages.debian.org/lenny/postgresql-8.3) as it used
to. You should be able to just build the extensions (like 
[prefix](http://packages.debian.org/squeeze/postgresql-8.4-prefix)), without
changing the source package, nor droping 
`postgresql-8.3-prefix` from the
distribution on the grounds that 
`8.3` ain't in debian stable anymore.

I've been ranting a lot about this state of facts, and I finally provided a
patch to the 
[postgresql-common](http://packages.debian.org/sid/postgresql-common) debian packaging, which made it into version
`110`: welcome 
[pg_buildext](http://packages.debian.org/sid/postgresql-server-dev-all). An exemple of how to use it can be found in the
git branch for 
[prefix](http://github.com/dimitri/prefix), it shows up in 
[debian/pgversions](http://github.com/dimitri/prefix/blob/master/debian/pgversions) and 
[debian/rules](http://github.com/dimitri/prefix/blob/master/debian/rules)
files. 

As you can see, the 
`pg_buildext` tool allows you to list the PostgreSQL major
versions the extension you're packaging supports, and only those that are
both in your list and in the current debian supported major version list
will get built. 
`pg_buildext` will do a 
`VPATH` build of your extension, so it's
capable of building the same extension for multiple major versions of
PostgreSQL. Here's how it looks:

~~~
# build all supported version
	pg_buildext build $(SRCDIR) $(TARGET) "$(CFLAGS)"

	# then install each of them
	for v in `pg_buildext supported-versions $(SRCDIR)`; do \
		dh_install -ppostgresql-$$v-prefix ;\
	done
~~~


And the files are to be found in those places:

~~~
dim ~/dev/prefix cat debian/postgresql-8.3-prefix.install 
debian/prefix-8.3/prefix.so usr/lib/postgresql/8.3/lib
debian/prefix-8.3/prefix.sql usr/share/postgresql/8.3/contrib

dim ~/dev/prefix cat debian/postgresql-8.4-prefix.install                                                                         
debian/prefix-8.4/prefix.so usr/lib/postgresql/8.4/lib
debian/prefix-8.4/prefix.sql usr/share/postgresql/8.4/contrib
~~~


So you still need to maintain 
[debian/pgversions](http://github.com/dimitri/prefix/blob/master/debian/pgversions) and the
`postgresql-X.Y-extension.*` files, but then a change in debian support for
PostgreSQL major versions will be handled automatically (there's a facility
to trigger automatic rebuild when necessary).

All this ranting to explain that pretty soon, the extenion's packages that I
maintain will no longer have to be patched when dropping a previously
supported major version of PostgreSQL. I'm breathing a little better, so
thanks a lot 
[Martin](http://www.piware.de/category/debian/)!

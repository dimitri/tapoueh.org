+++
date = "2011-09-05T17:14:00.000000+02:00"
title = "PostgreSQL and debian"
tags = ["PostgreSQL", "debian"]
categories = ["debian"]
thumbnailImage = "/img/old/debian-logo.png"
thumbnailImagePosition = "left"
coverImage = "/img/old/debian-logo.png"
coverSize = "partial"
coverMeta = "out"
aliases = ["/blog/2011/09/05-apt-postgresql-org",
           "/blog/2011/09/05-apt-postgresql-org.html"]
+++

After talking about it for a very long time, work finally did begin!  I'm
talking about the 
[apt.postgresql.org](https://github.com/dimitri/apt.postgresql.org) build system that will allow us, in the
long run, to propose 
`debian` versions of binary packages for 
[PostgreSQL](http://www.postgresql.org/) and
its extensions, compiled for a bunch of debian and ubuntu versions.

We're now thinking to support the 
`i386` and 
`amd64` architectures for 
`lenny`,
`squeeze`, 
`wheezy` and 
`sid`, and also for 
`maverick` and 
`natty`, maybe 
`oneiric` too
while at it.

It's still the very beginning of the effort, and it was triggered by the
decision to move 
`sid` to 
`9.1`.  While it's a good decision in itself, I still
hate to have to pick only one PostgreSQL version per debian stable release
when we have all the technical support we need to be able to support all
stable releases that 
*upstream* is willing to maintain. If you've been living
under a rock, or if you couldn't care less about 
`debian` choices, the problem
here for debian is ensuring security (and fixes) updates for PostgreSQL —
they promise they will handle the job just fine in the social contract, and
don't want to have to it without support from PostgreSQL if a 
*debian stable*
release contains a deprecated PostgreSQL version.

That opens the door for PostgreSQL community to handle the packaging of its
solutions as a service to its debian users.  We intend to open with support
for 
`8.4`, 
`9.0` and 
`9.1`, and maybe 
`8.3` too, as 
[Christoph Berg](http://qa.debian.org/developer.php?login=myon) is doing good
progress on this front.  See, it's teamwork here!

We still have more work to do, and setting up the build environment so that
we are able to provide the packages for so much targets will indeed be
interesting. Getting there, a step after another.

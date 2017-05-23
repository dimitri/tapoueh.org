+++
date = "2011-04-11T11:30:00.000000+02:00"
title = "Some notes about Skytools3"
tags = ["PostgreSQL", "debian", "release", "skytools", "restore"]
categories = ["debian"]
thumbnailImage = "/img/old/debian-logo.png"
thumbnailImagePosition = "left"
coverImage = "/img/old/debian-logo.png"
coverSize = "partial"
coverMeta = "out"
aliases = ["/blog/2011/04/11-some-notes-about-skytools3",
           "/blog/2011/04/11-some-notes-about-skytools3.html"]
+++

I've been working on 
[skytools3](http://github.com/markokr/skytools) packaging lately.  I've been pushing quite a
lot of work into it, in order to have exactly what I needed out of the box,
after some 3 years of production and experiences with the products.  Plural,
yes, because even if 
[pgbouncer](http://wiki.postgresql.org/wiki/PgBouncer) and 
[plproxy](http://wiki.postgresql.org/wiki/PL/Proxy) are siblings to the projets (same
developers team, separate life cycle and releases), then 
`skytools` still
includes several sub-projects.

Here's what the 
`skytools3` packaging is going to look like:

~~~
skytools3              Skytool's replication and queuing
python-pgq3            Skytool's PGQ python library
python-skytools3       python scripts framework for skytools
skytools-ticker3       PGQ ticker daemon service
skytools-walmgr3       high-availability archive and restore commands
postgresql-8.4-pgq3    PGQ server-side code (C module for PostgreSQL)
postgresql-9.0-pgq3    PGQ server-side code (C module for PostgreSQL)
~~~


This split is needed so that you can install your 
*daemons* (we call them
*consumers*) on separate machines than where you run 
[PostgreSQL](http://postgresql.org).  But for the
`walmgr` part, it makes no sense to install it if you don't have a local
PostgreSQL service, as it's providing 
`archive` and 
`restore` commands.  Then
the 
*ticker*, you're free to run it on any machine really, so just package it
this way (in 
`skytools3` the 
*ticker* is written in 
`C` and does not depend on the
python framework any more).

What you can't see here yet is the new goodies that wraps it as a quality
`debian` package.  A new 
`skytools` user is created for you when you install the
`skytools3` package (which contains the services), along with a skeleton file
`/etc/skytools.ini` and a user directory 
`/etc/skytools/`.  Put in there your
services configuration file, and register those service in the
`/etc/skytools.ini` file itself.  Then they will get cared about in the 
`init`
sequence at startup and shutdown of your server.

The services will run under the 
`skytools` system user, and will default to
put their log into 
`/var/log/skytools/`.  The 
`pidfile` will get into
`/var/run/skytools/`.  All integrated, automated.

Next big 
*TODO* is about documentation, reviewing it and polishing it, and I
think that 
`skytools3` will then get ready for public release.  Yes, you read
it right, it's happening this very year!  I'm very excited about it, and
have several architectures that will greatly benefit from the switch to
`skytools3`.  More on that later, though!  (Yes, my 
*to blog later* list is
getting quite long now).

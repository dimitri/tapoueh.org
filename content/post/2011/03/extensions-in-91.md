+++
date = "2011-03-01T16:30:00.000000+01:00"
title = "Extensions in 9.1"
tags = ["PostgreSQL", "debian", "pgcon", "Conferences", "Extensions", "release", "ip4r", "9.1"]
categories = ["Conferences","PostgreSQL Confs"]
thumbnailImage = "/img/old/conferences.jpg"
thumbnailImagePosition = "left"
coverImage = "/img/old/conferences.jpg"
coverSize = "partial"
coverMeta = "out"
aliases = ["/blog/2011/03/01-extensions-in-91",
           "/blog/2011/03/01-extensions-in-91.html"]
+++

If you've not been following closely you might have missed out on extensions
integration.  Well, 
[Tom](http://en.wikipedia.org/wiki/Tom_Lane_(computer_scientist)) spent some time on the patches I've been preparing
for the last 4 months.  And not only did he commit most of the work but he
also enhanced some parts of the code (better factoring) and basically
finished it.

At the 
[previous developer meeting](http://wiki.postgresql.org/wiki/PgCon_2010_Developer_Meeting) his advice was to avoid putting too much
into the very first version of the patch for it to stand its chances of
being integrated, and while in the review process more than one major
[PostgreSQL](http://www.postgresql.org/) contributor expressed worries about the size of the patch and the
number of features proposed.  Which is the usual process.

Then what happened is that 
***Tom*** finally took a similar reasoning as mine
while working on the feature.  To maximize the benefits, once you have the
infrastructure in place, it's not that much more work to provide the really
interesting features.  What's complex is agreeing on what exactly are their
specifications.  And in the 
*little* time window we got on this commit fest
(well, we hijacked about 2 full weeks there), we managed to get there.

So in the end the result is quite amazing, and you can see that on the
documentation chapter about it:
[35.15. Packaging Related Objects into an Extension](http://developer.postgresql.org/pgdocs/postgres/extend-extensions.html).

All the 
*contrib* modules that are installing 
`SQL` objects into databases for
you to use them are now converted to 
***Extensions*** too, and will get released
in 
`9.1` with an upgrade script that allows you to 
*upgrade from unpackaged*.
That means that once you've upgraded from a past PostgreSQL release up to
`9.1`, it will be a command away for you to register 
*extensions* as such.  I
expect third party 
*extension* authors (from 
[ip4r](http://pgfoundry.org/projects/ip4r/) to 
[temporal](http://pgfoundry.org/projects/temporal)) to release a
*upgrade-from-unpackaged* version of their work too.

Of course, a big use case of the 
*extensions* is also in-house 
`PL` code, and
having version number and multi-stage upgrade scripts there will be
fantastic too, I can't wait to work with such a tool set myself.  Some later
blog post will detail the benefits and usage.  I'm already trying to think
how much of this version and upgrade facility could be expanded to classic
`DDL` objects…

So expect some more blog posts from me on this subject, I will have to talk
about 
*debian packaging* an extension (it's getting damn easy with
[postgresql-server-dev-all](http://packages.debian.org/squeeze/postgresql-server-dev-all) — yes it has received some planing ahead), and
about how to package your own extension, manage upgrades, turn your current
`pre-9.1` extension into a 
*full blown extension*, and maybe how to stop
worrying about extension when you're a DBA.

If you have some features you would want to discuss for next releases,
please do contact me!

Meanwhile, I'm very happy that this project of mine finally made it to 
*core*,
it's been long in the making.  Some years to talk about it and then finally
4 months of coding that I'll remember as a marathon.  Many Thanks go to all
who helped here, from 
[2ndQuadrant](http://www.2ndquadrant.com/) to early reviewers to people I talked to
over beers at conferences… lots of people really.

To an extended PostgreSQL (and beyond) :)

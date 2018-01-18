+++
title = "Working at Citus"
date = "2018-01-18T12:05:13+01:00"
tags = ["PostgreSQL","distributed","citus"]
categories = ["PostgreSQL","Citus"]
coverImage = "/img/Citus_Data_Architecture_PressKit_Nov2017_005-b6bc8d0eb1dd9e1d92b308a0dec4eb7f.jpg"
coverMeta = "in"
coverSize = "partial"
thumbnailImage = "/img/icon-distributed-db-fbb8b92b.png"
thumbnailImagePosition = "left"

+++

You might have read it in the news already in Citus' blog post by [Sumedh
Pathak](https://www.citusdata.com/about/team/): [PostgreSQL Expert Dimitri
Fontaine joins Citus
Data](https://www.citusdata.com/blog/2018/01/12/dimitri-fontaine-postgresql-contributor-joins-citus-data/).
I am very happy to join a talented team here at Citus, and excited to work
on an Open Source solution for distributed SQL on-top of PostgreSQL! In this
article I'm going to cover my first technical contributions to Citus, as it
happens that a few patches of mine made it to the main source tree already.

**TL;DR** It's good to be working on PostgreSQL related source code again,
and to have the opportunity to solve PostgreSQL related problems at scale!

<!--more-->
<!--contents-->

# Contributing to an Open Source project

Work done on Citus is Open Source and public, so you can see the GitHub
repository for the main Citus extension and its
[citus/commits/master](https://github.com/citusdata/citus/commits/master)
project history.

A good way to discover a new code base is either fixing known bugs or
extending the basic functionality, doing some of the _grunt work_ that
nobody had the time to handle yet.

Doing _small things_ at the begining is important, so as to cover the basics
and discovering how things are done at the source code level. Moreover, it
allows to discover how to contribute to the project: workflow, explicit and
implicit policies, communication with the team, tests and coverage, the CI
integration, all those things that revolves around the code itself, and are
even more important if you want to be able to contribute!

So the first couple of weeks at Citus have been just that for me. Lots of
things to learn and information to process, and thanks to a great onboarding
by [Marco](https://github.com/marcocitus), some patches could be written in
the meantime.

# Early patches

So what do we have? My first ever patch to Citus implements [ALTER TABLE ...
RENAME TO
...](https://github.com/citusdata/citus/commit/e01023828046a6b517109c255a07765f9bc0a3fb)
for distributed tables. As you can see, it's a small feature on-top of the
existing product, all about convenience for the end-users. A perfect way to
start contributing!

The next patch is pretty obvious and implements [ALTER INDEX ... RENAME TO
...](https://github.com/citusdata/citus/commit/17266e3301dd7c39fa8a41ae79fc6c4f9fea7398)
for Citus distributed tables. Some more convenience!

While in the theme, there was also [ALTER TABLE|INDEX ... SET|RESET
()](https://github.com/citusdata/citus/commit/952da72c55d6919d72efb93001aaa4fa29aa8758)
on distributed tables to be handled by Citus yet, so you know what? Yeah, we
added that.

This had us realize that a known bug in a related area needed to be taken
care of, so the next patch in the series allows to [fix CREATE INDEX with
storage options on distributed
tables](https://github.com/citusdata/citus/commit/c9760fbb64e5c08913b682c9d25822721f0693fc).

In the making of those patches, we covered lots of ground with Marco and the
team. I'm quite excited about what's next on the plan now ;-)

# Plans 

Well with some good luck and lots of hard work, we might some day write “and
the rest is history.” But we're not there yet, are we? So in the following
weeks and months, I'm going to work on continuing to make Citus easier to
use.

{{< figure class="right"
             src="/img/icon-scaling-8a5e7040.png"
            link="https://www.citusdata.com" >}}

It's already quite amazing all the things that you'll find in this
*PostgreSQL extension*, really: Citus implements a clean semantics for
[distruted
transactions](https://www.citusdata.com/blog/2017/11/22/how-citus-executes-distributed-transactions/)
and [transparent query
routing](https://www.citusdata.com/blog/2017/06/02/scaling-complex-sql-transactions/),
and builds on-top of that a very well integrated product with no-downtime
scale-out properties.

I'm joining a talented team working on a mature product. I expect most of
the work to be in rough edges of it, at the beginning, working on improving
a smooth user experience everywhere possible!

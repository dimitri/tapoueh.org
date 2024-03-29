+++
date = "2012-09-15T18:43:00.000000+02:00"
title = "PostgreSQL 9.3"
tags = ["PostgreSQL"]
categories = ["PostgreSQL"]
thumbnailImage = "/img/postgresql-512.jpg"
thumbnailImagePosition = "left"
coverImage = "/img/postgresql-512.jpg"
coverSize = "partial"
coverMeta = "out"
aliases = ["/blog/2012/09/15-PostgreSQL-9",
           "/blog/2012/09/15-PostgreSQL-9.html",
           "/blog/2012/09/15-PostgreSQL-9.3",
           "/blog/2012/09/15-PostgreSQL-9.3.html"]
+++

[PostgreSQL 9.2](http://www.postgresql.org/) is released! It's an awesome new release that I urge you to
consider trying and adopting, an upgrade from even 
`9.1` should be very well
worth it, as your hardware could suddenly be able to process a much higher
load. Indeed, better performances mean more work done on the same budget,
that's the name of the game!

As a 
*PostgreSQL contributor* though, the release of 
`9.2` mainly means to me
that it's time to fully concentrate on preparing 
`9.3`. The developement
*season* of which as already begun, by the way, so some amount of work has
already been done here.

<center>
{{< image classes="fig50 fancybox dim-margin" src="/img/old/event-trigger.jpg" >}}
</center>

The list of things I want to be working on for that next release is quite
long, and looks more like a christmas list than anything else. Let's only
talk about those things I might as well make happen rather than all the
things I wish I was able to be delivering in a single release...


## Event Triggers

We missed 
`9.2` for wanting to include too big a feature in one go, leading to
too many choices to review and take decision about, for once, and also to
some non optimal choices that had to be reconsidered. Thanks to 
[PGCON](../06/24-back-from-pgcon.html) in
Ottawa earlier this year, I could meet in person with 
**Robert Haas** and we've
been able to decide how to attack that big patch I had. The first step has
been to 
*commit* in the PostgreSQL tree only infrastructure parts, on which we
will be able to build the feature itself.


### Infrastructure

What we already have today is the ability to run 
*user defined function* when
some event occurs, and an event can only be a 
`ddl_command_start` as of now.
Also the 
*trigger* itself must be written in 
`PLpgSQL` or 
`PL/C`, as the support
for the other languages was not included from the patch.

That leaves some work to be done in the next months, right?


### PL support

The 
*user defined function* will get some information from 
*magic variables*
such as 
`TG_EVENT` and such. That allows easier integration of future
information we want to add, without disrupting those existing 
*triggers* that
you wrote (no 
`API` change), at the cost of having to write a specific
integration per 
*procedural language*.

So one of the first things to do now is to take the support for the others
`PL` that I had in my proposal and make a new patch with only that in there.


### Fill-in more information

Then again, this first infrastructure part was all about being actually able
to run a user function and left behind most of the information I would like
the function to have. The information already there is the 
`command tag`, the
`event name` and the 
`parsetree` that's only usable if you're writing your
trigger in 
`C`, which we expect some users to be doing.

To supplement that, we're talking about the 
`Object ID` that has been the
target of the 
*event*, the 
`schema` it leaves in when applicable, the 
`Object
Name`, the 
`Operation` that's running (
`CREATE`, 
`ALTER`, 
`DROP`), the 
`Object Kind`
being the target of said operation (e.g. 
`TABLE` or 
`FUNCTION`), and the 
`command
string`.


### Publishing the Command String

Publishing the 
*Command String* here is not an easy task, because we have to
rebuild a normalized version of it. Or maybe we can go with passing explicit
context in which the command is running, such as the 
`search_path`.

Even with an explicit context that would be easy enough to 
`SET` back again
(in a remote server where you would be replicating the 
`DDL`, say), it would
be better to normalize the 
*command string* so as to remove extra spaces and
make it easier to parse and process from a 
*user defined function*.

That part looks like where most of the work is going to happen in the next
*commit fests*.


### Events

The other big thing I want to be working on with respect to this feature is
the 
*event* support, which is basically 
*hard coded* to be 
`ddl_command_start` in
the current state of the 
`9.3` code.

We certainly will want to be able to run 
*user defined function* not only at
the very beginning of a 
*DDL command*, but also just before it finishes so
that the newly created object already exists, for example.

We might also be interested into supporting triggers on more than 
`DDL`, there
I doubt we will see that happening in 
`9.3`, as some people in the community
would go crazy about complex use cases. Time is limited, and I think this is
better kept open for the next release, as the way our beloved PostgreSQL
works is by delivering reliable features: quality first.


### Use cases

I'm always happy to hear about use cases for the features I'm working on,
and this one has the potential to be covering a non trivial amount of them.
I already can think of 
*trigger based replication systems* and some integrated
*extension network facilities*. With your help we can give those the place
they should have: early days use cases in a great collection.


## Extensions
<center>
{{< image classes="fig50 fancybox dim-margin" src="/img/old/extensions-cords.jpg" >}}
</center>

So yes, 
*event triggers* first use case for me is in relation with 
*extensions*.
Surprise! There's still some more I want to do with 
*extensions*, so much that
I could consider their implementation in 
`9.1` just an enabler. In 
`9.1` the
game has been to offer the best support we could design for existing 
`contrib`
modules, with a very strong angle toward clean support for 
*dump* and 
*restore*.

The typical contrib module exports in SQL a list of C coded functions,
sometime supporting a new datatype, sometime a set of administration
functions. It's quite rare that contrib modules are handling 
*user data*
embedded in their SQL definition, and when it happens it's mostly
*configuration* kind of data, such as with 
[PostGIS](TODO: add the link).

Now we want to fully support 
*extensions* that are maintaining their own 
*user
data*, or even those that are all about them. The main difficulty here is
that our current design of 
*dump* and 
*restore* support is following a model
where installing the same extesion in a new database is all covered by
`create extension foo;`. This is a limited model of the reality, that we need
to expand.

The first manifestation of those problems is in the 
`SEQUENCE` support in
extensions, and that impacts one of my favorite extensions: 
[PGQ](http://wiki.postgresql.org/wiki/Skytools).


## PostgreSQL releases

[PostgreSQL](http://www.postgresql.org/) just released an awesome release with 
`9.2`, where we get
tremendous performance optimisations and truly innovative features, such as
`RANGE TYPE`. How not to consider PostgreSQL as a part of your application
stack, where to develop and host your features.

While users are enjoying the newer release, contributors are already
preparing the next one, hard at work again!

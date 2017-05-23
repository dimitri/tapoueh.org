+++
date = "2010-05-27T14:26:00.000000+02:00"
title = "Back from PgCon2010"
tags = ["PostgreSQL", "pgcon", "Extensions", "backup", "restore", "9.1"]
categories = ["PostgreSQL","Extensions"]
thumbnailImage = "/img/pgcon2010.jpg"
thumbnailImagePosition = "left"
coverImage = "/img/pgcon2010.jpg"
coverSize = "partial"
coverMeta = "out"
aliases = ["/blog/2010/05/27-back-from-pgcon2010",
           "/blog/2010/05/27-back-from-pgcon2010.html"]
+++

This year's edition has been the 
[best pgcon](http://www.pgcon.org/2010/) ever for me. Granted, it's only
my third time, but still :) As 
[Josh said](http://blog.endpoint.com/2010/05/pgcon-hall-track.html) the 
*"Hall Track"* in particular was
very good, and the 
[Dev Meeting](http://wiki.postgresql.org/wiki/PgCon_2010_Developer_Meeting) has been very effective!


## Extensions

This time I prepared some 
[slides to present the extension design](http://wiki.postgresql.org/wiki/Image:Pgcon2010-dev-extensions.pdf) and I tried
hard to make it so that we get to agree on a plan, even recognizing it's not
solving all of our problems from the get go. I had been talking about the
concept and design with lots of people already, and continued to do so while in
Ottawa on Monday evening and through all Tuesday. So Wednesday, I felt
prepared. It proved to be a good thing, as I edited the slides with ideas from
several people I had the chance to expose my ideas to! Thanks 
*Greg Stark* and
*Heikki Linnakangas* for the part we talked about at the meeting, and a lot more
people for the things we'll have to solve later (Hi 
*Stefan*!).

So the current idea for 
**extensions** is for the 
*backend* support to start with a
file in 
``pg_config --sharedir`/extensions/foo/control` containing
the 
*foo* extension's 
*metadata*. From that we know if we can install an extension
and how. Here's an example:

~~~
name = foo
version = 1.0
custom_variable_classes = 'foo'
depends  = bar (>= 1.1), baz
conflicts = bla (< 0.8)
~~~


The other files should be 
`install.sql`, 
`uninstall.sql` and 
`foo.conf`. The only
command the user will have to type in order for using the extension in his
database will then be:

~~~
INSTALL EXTENSION foo;
~~~


For that to work all that needs to happen is for me to write the code. I'll
keep you informed as soon as I get a change to resume my activities on the
[git branch](http://git.postgresql.org/gitweb?p=postgresql-extension.git;a=shortlog;h=refs/heads/extension) I'm using. You can already find my first attempt at a
`pg_execute_from_file()` function 
[there](http://git.postgresql.org/gitweb?p=postgresql-extension.git;a=commitdiff;h=6eed4eca0179cbdeb737b9783084e9f03fcb7470).

Building atop that backend support we already have two gentlemen competing on
features to offer to 
[distribute](http://justatheory.com/computers/databases/postgresql/pgan-bikeshedding.html) and 
[package](http://petereisentraut.blogspot.com/2010/05/postgresql-package-management.html) extensions! That will complete the
work just fine, thanks guys.


## Hot Standby

Heikki's talk about 
[Built-in replication in PostgreSQL 9.0](http://www.pgcon.org/2010/schedule/events/264.en.html) left me with lots of
thinking. In particular it seems we need two projects out of core to complete
what 
`9.0` has to offer, namely something very simple to prepare a base backup
and something more involved to manage a pool of standbys.


### pg_basebackup

The idea I had listening to the talk was that it might be possible to ask the
server, in a single SQL query, for the list of all the files it's using. After
all, there's those 
`pg_ls_files()` and 
`pg_read_file()` functions, we could put
them to good use. I couldn't get the idea out of my head, so I had to write
some code and see it running: 
[pg_basebackup](http://github.com/dimitri/pg_basebackup) is there at 
`github`, grab a copy!

What it does is very simple, in about 100 lines of self-contained python code
it get all the files from a running server through a normal PostgreSQL
connection. That was my first 
[recursive query](http://www.postgresql.org/docs/8.4/interactive/queries-with.html). I had to create a new function
to get the file contents as the existing one returns text, and I want 
`bytea`
here, of course.

Note that the code depends on the 
`bytea` representation in use, so it's only
working with 
`9.0` as of now. Can be changed easily though, send a patch or just
ask me to do it!

Lastly, note that even if 
`pg_basebackup` will compress each chunk it sends over
the 
`libpq` connection, it won't be your fastest option around. Its only
advantage there is its simplicity. Get the code, run it with 2 arguments: a
connection string and a destination directory. There you are.


### wal proxy, wal relay

The other thing that we'll miss in 
`9.0` is the ability to both manage more than
a couple of 
*standby* servers and to manage failover gracefully. Here the idea
would be to have a proxy server acting as both a 
*walreceiver* and a
*walsender*. Its role would be to both 
*archive* the WAL and 
*relay* them to the real
standbys.

Then in case of master's failure, we could instruct this 
*proxy* to be fed from
the elected new master (manual procedure), the other standbys not being
affected. Well apart than apparently changing the 
*timeline* (which will happen
as soon as you promote a standby to master) while streaming is not meant to be
supported. So the 
*proxy* would also disconnect all the 
*slaves* and have them
reconnect.

If we need such a finesse, we could have the 
`restore_command` on the 
*standbys*
prepared so that it'll connect to the 
*proxy's archive*. Now on failover, the
*standbys* are disconnected from the stream, get a 
`WAL` file with a new 
*timeline*
from the 
*archive*, replay it, and reconnect.

That means that for a full 
`HA` scenario you could get on with three
servers. You're back to two servers at failover time and need to rebuild the
crashed master as a standby, running a base backup again.

If you've followed the idea, I hope you liked it! I still have to motivate some
volunteers so that some work gets done here, as I'm probably not the one to ask
to as far as coding this is concerned, if you want it out before 
`9.1` kicks in!


## Queuing

We also had a nice 
*Hall Tack* session with 
*Jan Wieck*, 
*Marko Kreen* and 
*Jim Nasby*
about how to get a single general (enough) queueing solution for PostgreSQL. It
happens that the Slony queueing ideas made their way into 
`PGQ` and that we'd
want to add some more capabilities to this one.

What we talked about was adding more interfaces (event producers, event format
translating at both ends of the pipe) and optimising how many events from the
past we keep in the queue for the subscribers, in a cascading environment.

It seems that the basic architecture of the queue is what 
`PGQ 3` provides
already, so it could even be not that much of a hassle to get something working
out of the ideas exchanged.

Of course, one of those ideas has been discussed at the 
[Dev Meeting](http://wiki.postgresql.org/wiki/PgCon_2010_Developer_Meeting), it's about
deriving the transaction commit order from the place which already has the
information rather than 
*reconstructing* it after the fact. We'll see how it
goes, but it started pretty well with a design mail thread.


## Other talks 

I went to some other talks too, of course, unfortunately with an attention span
far from constant. Between the social events (you should read that as 
*beer
drinking evenings*) and the hall tracks, more than once my brain were less
present than my body in the talks. I won't risk into commenting them here, but
overall it was very good: in about each talk, new ideas popped into my
head. And I love that.


## Conclusion: I'm addicted.

The social aspect of the conference has been very good too. Once more, a warm
welcome from the people that are central to the project, and who are so easily
available for a chat about any aspect of it! Or just for sharing a drink.

Meeting our users is very important too, and 
[pgcon](http://www.pgcon.org/2010/) allows for that also. I've
met some people I'm used to talk to via 
`IRC`, and it was good fun sharing a beer
over there.

All in all, I'm very happy I made it to Ottawa despite the volcano activity,
there's so much happening over there! Thanks to all the people who made it
possible by either organizing the conference or attending to it! See you next
year, I'm addicted...

+++
date = "2010-07-19T16:30:00.000000+02:00"
title = "Background writers"
tags = ["PostgreSQL"]
categories = ["PostgreSQL"]
thumbnailImage = "/img/postgresql-512.jpg"
thumbnailImagePosition = "left"
coverImage = "/img/postgresql-512.jpg"
coverSize = "partial"
coverMeta = "out"
aliases = ["/blog/2010/07/19-background-writers",
           "/blog/2010/07/19-background-writers.html"]
+++

There's currently a thread on 
[hackers](http://archives.postgresql.org/pgsql-hackers/) about 
[bg worker: overview](http://archives.postgresql.org/pgsql-hackers/2010-07/msg00493.php) and a series
of 6 patches. Thanks a lot 
***Markus***! This is all about generalizing a concept
already in use in the 
*autovacuum* process, where you have an independent
subsystem that require having an autonomous 
*daemon* running and able to start
its own 
*workers*.

I've been advocating about generalizing this concept for awhile already, in
order to have 
*postmaster* able to communicate to subsystems when to shut down
and start and reload, etc. Some external processes are only external because
there's no need to include them 
*by default* in to the database engine, not
because there's no sense to having them in there.

So even if 
***Markus*** work is mainly about generalizing 
*autovacuum* so that he
has a 
*coordinator* to ask for helper backends to handle broadcasting of
*writesets* for 
[Postgres-R](http://postgres-r.org/), it still could be a very good first step towards
something more general. What I'd like to see the generalization handle are
things like 
[PGQ](http://wiki.postgresql.org/wiki/PGQ_Tutorial), or the 
*pgagent scheduler*. In some cases, 
[pgbouncer](http://pgbouncer.projects.postgresql.org/doc/usage.html) too.

What we're missing there is an 
*API* for everybody to be able to extend
PostgreSQL with its own background processes and workers. What would such a
beast look like? I have some preliminary thoughts about this in my
[Next Generation PostgreSQL](char10.html#sec16) article, but that's still early thoughts. The
main idea is to steal as much as sensible from
[Erlang Generic Supervisor Behaviour](http://www.erlang.org/doc/man/supervisor.html), and maybe up to its
[Generic Finite State Machines](http://www.erlang.org/doc/design_principles/fsm.html) 
*behavior*. In the 
*Erlang* world, a 
*behavior* is a
generic process.

The 
*FSM* approach would allow for any user daemon to provide an initial state
and register functions that would do some processing then change the
state. My feeling is that if those functions are exposed at the SQL level,
then you can 
*talk* to the daemon from anywhere (the Erlang ideas include a
globally —cluster wide— unique name). Of course the goal would be to
provide an easy way for the 
*FSM* functions to have a backend connected to the
target database handle the work for it, or be able to connect itself. Then
we'd need something else here, a way to produce events based on the clock. I
guess relying on 
`SIGALRM` is a possibility.

I'm not sure about how yet, but I think getting back in consultancy after
having opened 
[2ndQuadrant](http://2ndQuadrant.com) 
[France](http://2ndQuadrant.fr) has some influence on how I think about all
that. My guess is that those blog posts are a first step on a nice journey!

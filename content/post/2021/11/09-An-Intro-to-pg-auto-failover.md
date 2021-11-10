+++
title = "An introduction to the pg_auto_failover project"
date = "2021-11-10T17:11:29+0100"
tags = ["PostgreSQL","Failover","pg_auto_failover","HA"]
categories = ["PostgreSQL","YeSQL"]
coverImage = "/img/rajendra-biswal-8_wAKXgPxE0-unsplash.jpg"
coverMeta = "in"
coverSize = "partial"
thumbnailImage = "img/redundanzen_500x500px.png"
thumbnailImagePosition = "left"

+++

We just released [pg_auto_failover version
1.6.3](https://github.com/citusdata/pg_auto_failover/releases/tag/v1.6.3) on
GitHub, and the binary packages should be already available at the usual
PGDG and CitusData places, both for debian based distributions and RPM based
distributions too.

This article is an introduction to the pg_auto_failover project: we answer
the _Five W_ questions, starting with why does the project exist in the
first place?

**TL;DR** pg_auto_failover is an awesome project. It fills the gap between
*“Postgres is awesome, makes developping my application so much easier, it
solves so many problems for me!”* and the next step *“so, how do I run
Postgres in Production?”*. If you're not sure how to bridge that gap
yourself, how to deploy your first production system with automated
failover, then pg_auto_failover is for you. It is simple to use, user
friendly, and well documented. **Star it** on the [pg_auto_failover GitHub
repository](https://github.com/citusdata/pg_auto_failover) and get started
today. Consider contributing to the project, it is fully Open Source, and
you are welcome to join us.

Buckle up, our guide tour is starting now!

<!--more-->
<!--toc-->

## Why pg_auto_failover?

The [pg_auto_failover](https://github.com/citusdata/pg_auto_failover)
project was started quite some years ago now. We made it Open Source in May
2019, after some years of internal brewing at [Citus
Data](https://www.citusdata.com). 

### Early History

At the time, our customers when running Citus on-prem had a choice of
implementing HA with either Patroni, repmgr, or some kind of in-house stuff.
The feedback we had from our customers was quite clear: Patroni was way to
complex for them to operate. Setting-up is complex already, and then making
sure that the distributed consensus is running fine at all times is
non-trivial. Then repmgr was hard to setup in a way that would pass our
customers QA.

Our customers asked us to deliver something simple to setup and operate,
easy to understand and trouble-shoot, and compatible with a Citus cluster
out of the box. If a Citus coordinator failover looks exactly like a
standalone Postgres failover, then when it comes to a Citus worker failover,
it's possible to be way smarter.

### Prototype days

The idea then was born, and some of my colleagues got started on a first
prototype, one that would use a centralized Finite State Machine to operate
two Postgres nodes and orchestrate failovers when that's needed. Even in the
very early prototype, the idea of supporting both failover and switchover in
a single Finite State Machine was implemented.

To recap, our context was a direct customer ask. The main goal was to build
a Postgres failover solution that would be simple and robust:

  - simple to setup, simple to operate, simple to trouble shoot and debug
    when running in production;
    
  - robust against the typical one-node failures one should expect in
    production, and robust when facing several variations of network splits.
    
Robust also means that our solution should include *split-brain* prevention
without external components.

### Simple and Robust failover solution for PostgreSQL

And I'm proud to say, that's exactly the user experience we deliver with
[pg_auto_failover](https://github.com/citusdata/pg_auto_failover). It still
is simple and robust, even now that we have support for quite [flexible
architectures with multiple standby
nodes](https://pg-auto-failover.readthedocs.io/en/master/architecture-multi-standby.html).

How can I say that myself? Well, we keep getting nice feedback about the
product. Go read **Rasmus Porsager**'s comments in GitHub discussion #618
that they titled [Congratulations and
thanks](https://github.com/citusdata/pg_auto_failover/discussions/618).
Rasmus maintains the [Postgres.js](https://github.com/porsager/postgres)
javascript client (driver) for Postgres, so I take it that they know a
couple things about using Postgres in development and in production, and
what to expect in terms of simplicity and robustness when it comes to a
failover solution.

<center>
    <hr style="width: 0px;"/>
</center>

## What is pg_auto_failover?

The early prototype was a very simple thing. A Postgres extension that
drives an FSM (Finite State Machine) with one or two Postgres instances per
group, and a couple of shell scripts. Yeah, a Postgres extension written in
C, and two shell scripts, around 1000 lines each. That was it!

Since then the architecture of the project is still the same, with a monitor
that is implemented as a Postgres extension, and a `pg_autoctl` command
(written in C lang) that replaces the two shell scripts.

### The pg_auto_failover Monitor

The [pg_auto_failover
architecture](https://pg-auto-failover.readthedocs.io/en/master/architecture.html)
adds a single monitor node to your Postgres deployment. We use Postgres
itself on that monitor, so that we benefit from all the usual ACID
guarantees that Postgres offers. Any state change that we record on the
monitor is transactional, and durable.

Now, this means that the monitor is a SPOF (Single Point of Failure). There
are only two ways to solve that problem: either use distributed consensus
(and lose the Postgres ACID guarantees, trading them for a completely
different model), or lower the impact of a monitor failure and make it easy
to replace the monitor online.

Given the context of the project, and our customers feedback, we didn't want
to implement distributed consensus: our customers reported very clearly that
this makes the whole thing too hard to operate. Think about it, people who
know how to operate a distributed consensus solution to orchestrate Postgres
nodes are not typically people who ask you to help them setup their first
ever Postgres instance and then want you on-call to support their production
terms of services. 

Also, if you want a distributed consensus based orchestration mechanism,
have a look at [Patroni](https://patroni.readthedocs.io/en/latest/). And
take time to understand the specific failure modes of a distributed
consensus solution and their impact on your Postgres production setup, as
detailed in the article [Patroni & etcd in High Availability
Environments](https://blog.crunchydata.com/blog/patroni-etcd-in-high-availability-environments).
TL;DR: File System Is Full on the consensus parts may lead to spurious
Postgres failovers. Fun!

### Postgres nodes

Once you have a monitor running, you can deploy Postgres nodes and register
them to the monitor. While still running a pristine Postgres Core server,
without any extension of any kind, the Postgres instance needs to be
controlled by the `pg_autoctl` command.

Think about the case when you reboot the primary instance, and it takes long
enough to trigger a failover. At restart, the old-primary Postgres instance
should not be restarted as-is, we really want to edit the setup and make it
a standby to the current primary. Otherwise, that's a *split-brain*
situation. By running `pg_autoctl` instead of `postgres` at start-up, we can
easily avoid this problem and make your production setup “just work”.

### Achieving Postgres Core Flexibility in a User-Friendly way

Finally, in terms of flexibility, any Postgres node can be set to
participate in the replication quorum or abstain from it, and can be set to
be a candidate for failover with a priority that we compare with the other
nodes available.

A basic thing that Postgres Core allows but that's been hard to achieve with
other software is to have a primary and two standby nodes, and make it so
that a failover would preferably target one of the nodes. 

There are several use-cases for being able to setup a priority system that
way. One of them is when hardware is upgraded, it's sometimes useful to keep
the previous generation of hardware around for data security, even if it
would be best to avoid running your primary workload on older machines.
Another use case involves multiple regions (or data centers), and having a
strong preference over which is the primary.

When using pg_auto_failover, it's easy to tweak the candidate priority
property of your nodes. Then at failover time, if your favourite node is not
the most advanced standby available, pg_auto_failover will go as far as
fetching missing WAL bytes from another standby node before promotion. If
you know about `pg_rewind`, then you will understand why we name this
operation a *fast-forward*.

And *fast-forward* is included, automatic, and covered in our unit testing.

When tweaking [pg_auto_failover replication
settings](https://pg-auto-failover.readthedocs.io/en/master/architecture-multi-standby.html#replication-settings-and-postgres-architectures)
it is also possible to setup a physical standby that maintains a copy of
your PGDATA, participates in the quorum, and is *never* to be promoted
primary.

Heck, it's even possible to require manual intervention to promote nodes:
assign candidate priority zero to all the nodes, and when a failover should
happen, pg_auto_failover then does nothing about it. To manually trigger a
node promotion one might edit the candidate priority of the target new
primary node, or use the `pg_autoctl perform promotion` command.

### Postgres Failover made Simple

What pg_auto_failover offers is an easy and integrated way to orchestrate
Postgres operations, using Postgres Core capabilities and commands. With a
limited set of [replication
settings](https://pg-auto-failover.readthedocs.io/en/master/architecture-multi-standby.html#replication-settings-and-postgres-architectures)
and the [main pg_autoctl
commands](https://pg-auto-failover.readthedocs.io/en/master/how-to.html) we
have a very simple and robust solution to operate Postgres architectures in
production.

We even include full support for automated failover with manual decision
making, if that's what you need.

<center>
    <hr style="width: 0px;"/>
</center>

## How does pg_auto_failover implements Postgres failover?

So pg_auto_failover is a simple, robust, and flexible way to implement a
Postgres Architecture in Production. It comes with a monitor that doubles as
a network witness, implemented as a Postgres extension. So when running N
Postgres nodes, with pg_auto_failover you need to provision and run N+1
nodes: the monitor, and then the N Postgres nodes. Instead of using
`initdb`, you now use `pg_autoctl create monitor` and then `pg_autoctl
create postgres`.

### Using Postgres Core tooling

Then the `pg_autoctl` process sits on the top of a process tree where we
have our communication protocol client (we name that the *node-active*
protocol, and then the *node-active* process), and then the whole Postgres
process tree itself is a sub-tree of the main `pg_autoctl` process.

When implementing a failover, `pg_autoctl` knows when to start and stop
`postgres`, and uses `pg_rewind` to reconnect to the new primary, and even
falls back to using `pg_basebackup` when required.

To be ready for failovers, `pg_autoctl` implements and maintains physical
replication slots on all the Postgres nodes, the primary and the standby
nodes.

In order to ease the first steps of deploying Postgres streaming
replication, the `pg_autoctl` command also automates SSL self-signed
certificates creation, and goes as far as editing the Postgres HBA. As nice
as this might be, make sure to review our [security
settings](https://pg-auto-failover.readthedocs.io/en/master/security.html)
documentation before deploying in production.

### Distributed Computing, Network Splits, Health Checks, Availability

Even when using a single-node monitor in our architecture, the
pg_auto_failover architecture is distributed in nature. A minimal
installation is composed of 3 nodes, each running on their own server (or
VM, or container), with their own network stack. It could be the same
physical network, or a network that abstracts away a deployment over
multiple buildings.

If you're not familiar with them yet, now is a good time to review the
famous [fallacies of distributed
computing](https://en.wikipedia.org/wiki/Fallacies_of_distributed_computing).
If you're familiar with them already, now is a good time to review them
again!

The pg_auto_failover documentation includes a whole chapter about [Failover
and Fault
Tolerance](https://pg-auto-failover.readthedocs.io/en/master/fault-tolerance.html)
that dive into the different aspects of how to detect unhealthy nodes and
network partitions, and how to react to them in the context of running a
Postgres service in produciton.

The implementation we use in pg_auto_failover relies on the following
components:

  - the monitor implements a health-check background worker that connects
    once in a while to all the registered Postgres nodes, and updates
    metadata with last time it could connect and if there was a connection
    error,
    
  - the `pg_autoctl` process tree that runs your Postgres instances also
    runs the *node-active* protocol client: this connects to the monitor
    about every second and reports the current known state of the local
    Postgres instance, including metadata such as if Postgres is
    up-and-running, the current timeline LSN of the node, and the FSM state
    of the node,
    
  - the `pg_autoctl` process also checks if other nodes are visible from the
    primary in the `pg_stat_replication` system view, allowing to
    differenciate certain cases of network split.
    
With this information, the monitor can decide when and how to implement a
failover. There is still this case when the primary is isolated on the
network, also mentioned as being *in the losing side of a network split*.
This situation can be detected when the `pg_autoctl` *node-active* process
can't connect to the monitor and also `pg_stat_replication` is empty (and
not expected to be!). In that case, `pg_autoctl` locally takes the decision
to stop Postgres to avoid a potential *split-brain* situation.

When implementing Postgres HA, there is always a trade-off to be made
between availability of the service or of the data. In pg_auto_failover, we
try to be smart about that trade-off and also give options to the users.

That said, when it comes to potential *split-brain* situation, we believe
that we should make sure to avoid it, even if that means stopping a
production instance of Postgres. And that's exactly what `pg_autoctl` does.

To recap, yes, *split-brain* detection is included, automatic, and covered
in our unit testing. One of the tests we run in our CI creates Linux network
namespaces and then does a `ifconfig down` on the primary's network
interface to check that we implement the proper response to the situation.

This implementation exposes some timeouts that a user may want to edit and
adapt to their own situation and network properties, so please review our
[configuration](https://pg-auto-failover.readthedocs.io/en/master/ref/configuration.html)
docs, and the [pg_autoctl config
set](https://pg-auto-failover.readthedocs.io/en/master/ref/pg_autoctl_config_set.html)
manual page too.

### Robust Against Single-Node Failures

The pg_auto_failover mechanics have been designed (and tested!) to be robust
to any one node failure. It could be the monitor, a Postgres node that
currently is the primary, or another Postgres node. It could be a transient
failure (the node is going to come back) or a permanent one.

Handling multiple nodes failures at the same time is more complex to handle,
and that is where some distributed consensus solutions are proven reliable.
That said, a distributed consensus solution requires a carefully engineered
deployment to provide its guarantees.

One aspect of the deployment is: where are the distributed consensus nodes
deployed? When using the same nodes as the Postgres nodes, at least the
network split detection should be reliable for both services. When using
separate nodes, network splits must be handled at both levels. And in any
case, the distributed consensus election mechanism only selects the
distributed consensus leader, it doesn't elect and promote the Postgres
service leader.

In pg_auto_failover, when it comes to handling multiple node failures at the
same time, we still consider our solution to be a “best effort” approach.
Many cases will just work, and some will be problematic. Our design focuses
on making it easy (or at least possible, in the worst case) to trouble-shoot
and repair your production architecture.

Is that perfect? No. Can it be made better? Sure. Is it robust enough for
your own deployment? Well that's your job to answer that question, I
believe.

<center>
    <hr style="width: 0px;"/>
</center>

## Who contributes to pg_auto_failover? Who uses it?

The pg_auto_failover project is fully Open Source. The source code is
available publically under the terms of *The PostgreSQL License*. The
development aspects of the project also happens in public: issues,
discussions, code reviews, pull requests, feature requests, milestones and
sprints, it's all happening in the open.

The company that started it all is [Citus Data](https://www.citusdata.com),
and it has been acquired by Microsoft, so the main contributions happens to
come from Microsoft employees at the moment. That said, other companies such
as VMWare are contributing to pg_auto_failover.

My understanding is that VMWare goes as far as using pg_auto_failover
internally for all their Postgres production architectures, both for their
own services and also for their customer offerings.

Other companies are using pg_auto_failover, and a production ready set of
[pg_auto_failover Ansible
Playbooks](https://github.com/neuroforgede/pg_auto_failover_ansible) are
actively maintained separately. Martin Braun participates in the development
of pg_auto_failover by reporting issues, hinting at new use-cases and
missing features, and offering some trouble-shooting diagnostics on some
issues by other Open Source users too.

Some issues that have been opened on the pg_auto_failover repository also
show yet other users who integrated our solution in their container based
deployments, using Docker, and sometimes using Kubernetes. As a result, we
have improved `pg_autoctl` with advanced retry mechanisms and we support
automated deployments where nodes may pop-up in any ordering. As a
contributor to the project, I often test that kind of scenario with
docker-compose.

<center>
    <hr style="width: 0px;"/>
</center>

## When can I start using pg_auto_failover? Is it production-ready?

We also have feedback from users that are running pg_auto_failover in
production, and are happy with it. We even have customers with support
contracts, and are on-call for their production environment. And if you know
me, you know that I do prefer sleeping without interruption. I tend to be
vocal about it, and sometimes measure MTBF as Mean Time Between
F**-wake-ups.

So yes, pg_auto_failover is production ready.

This doesn't mean we managed to produce the first ever software without any
bug in it, of course. You will run into surprises if you deploy without
testing and playing with the software first, and you might need some time to
adjust your mental model to the way pg_auto_failover actually works. As with
any other software, really.

With the release of version 1.6.3, we have included a new interactive
command that can be used as a text-based dashboard application: `pg_autoctl
watch` is like `top` for your Postgres failover orchestration. This makes it
visible and quite clear what's happening as seen from the monitor, and thus
getting started with pg_auto_failover has never been easier.

Now is a perfect time to jump-in!

<center>
    <hr style="width: 0px;"/>
</center>

## When can I start contributing to pg_auto_failover?

The current pg_auto_failover team appreciates all contributions to the
project and software. Even opening a quick issue for a typo found in the
docs is seen as a good contribution... as in [GitHub issue
#548](https://github.com/citusdata/pg_auto_failover/issues/548), thanks
Daniel!

Other than reporting issues, you may also want to take it to the next level
and open a pull request to *Fix a small typo* as in [GitHub PR
#601](https://github.com/citusdata/pg_auto_failover/pull/601), thanks Rodo!

Some other contributors can spend time on pg_auto_failover during their
working hours, and it shows! See for instance [issue
#813](https://github.com/citusdata/pg_auto_failover/pull/813) that spends
quality time to describe a bug in details, and then is solved in [pull
request #818](https://github.com/citusdata/pg_auto_failover/pull/818):
thanks again Rachel!

Your contributions are welcome: detailed bug reporting that allows to fix a
problem in the product, some use-case that is not handled yet and seems to
be in the scope of pg_auto_failover, a pull-request to fix a small typo you
found while browsing our documentation, a bug fix, or even a new feature. We
like it all!

Also, personally, I believe it's important to be welcoming and kind. If
you're not sure where to start maybe have a look at the issues with the
label [good first
issue](https://github.com/citusdata/pg_auto_failover/issues?q=is%3Aopen+is%3Aissue+label%3A%22good+first+issue%22),
some are still opened sometimes.

Feel free to open a work-in-progress PR and ask questions there, if you feel
like it will help you to contribute to the projet. I will happily spend time
and try to ease your journey into contributing to pg_auto_failover!

As usual, the best first steps are playing with the software, getting
familiar with its mode of operations, and reading the docs, then reading the
code README files, and then the code comments. That's a lot of material.
That's also the best way to get acquainted with the system!

<center>
    <hr style="width: 0px;"/>
</center>

## Conclusion

Thanks to have read this much, this ended-up as a longer article than I
thought it would be. That said covering the Five-Ws in a single run can lead
to longer pieces I suppose. To recap, here is what I believe is the TL;DR of
this article, the main message to get home with.

<center>
    <h4>pg_auto_failover is an awesome project.</h4>
</center>

It fills the gap between *“Postgres is awesome, makes developping my
application so much easier, it solves so many problems for me!”* and the
next step *“so, how do I run Postgres in Production?”*. 

If you're not sure how to bridge that gap yourself, how to deploy your first
production system with automated failover, then pg_auto_failover is for you.
It is simple to use, user friendly, and well documented. 

**Star it** on the [pg_auto_failover GitHub
repository](https://github.com/citusdata/pg_auto_failover) and get started
today. 

Consider contributing to the project, it is fully Open Source, and you are
welcome to join us.

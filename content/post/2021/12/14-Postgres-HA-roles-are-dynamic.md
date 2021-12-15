+++
title = "Postgres HA: roles are dynamic"
date = "2021-12-14T22:36:36+0100"
tags = ["PostgreSQL","Failover","pg_auto_failover","HA"]
categories = ["PostgreSQL","YeSQL"]
coverImage = "/img/rajendra-biswal-8_wAKXgPxE0-unsplash.jpg"
coverMeta = "in"
coverSize = "partial"
thumbnailImage = "img/redundanzen_500x500px.png"
thumbnailImagePosition = "left"

+++

High-Availability comes with some impact on your architecture choices, in
particular when applied to RDBMS such as Postgres. One such impact is the
idea of a failover. When implementing database HA, it is usually expected
that both the service and the data are maintained available in the face of
operational faults. The most common way to implement resilience includes
automated (or manual) failover, where a new primary is elected among a list
of standby nodes.

In other words, as soon as Postgres High-Availability is implemented, the
roles of your Postgres nodes are dynamic. The fact that a given node is a
primary or a standby at any given point in time ceases to be relevant to
understanding your architecture. In fact, the only thing that's now given
about the role of a node is that it will change. Otherwise you don't have
failover capability, and then, you probably don't have HA in the first
place, right?

In this article we are going to try and understand what having dynamic roles
for Postgres nodes in a HA system means.

<!--more-->
<!--toc-->

## Postgres HA setup: nodes and roles

Postgres instances default to being independent primary nodes. When
provisionning a standby node a specific setup is required, that depends on
Postgres version. Starting with Postgres 12 a `standby.signal` file is
required, and before that it used to be the `recovery.conf` file.

Some of the Postgres configuration file settings only apply to the recovery
mechanisms that are used either when doing PITR or when running a standby
node. Those settings used to be managed in their own configuration file, and
from Postgres 12 onwards those settings can be set in the main
`postgresql.conf` file.

So with recent versions of Postgres, the exact same `postgresql.conf` can be
shipped to all the nodes. The presence of the `standby.conf` file determines
if a node is asked to start as a primary node or as a standby node.

Reversing that choice is not easy with Postgres. In most cases `pg_rewind`
is necessary, and that in turn requires that the instance was shutdown
properly.

With that we can see that in Postgres core itself the idea that an instance
role should be dynamic is not well ingrained in the software. That's the
first reason why a failover management software is needed.

## Roles are dynamic

When implementing Postgres HA, the only reason why standby nodes are
provisioned and deployed is so that they may be elected as a primary if
needed. So by definition, the role of a specific node is dynamic. A node
could be a server, a VM, a pod, a container, or maybe even something else.

{{< figure classes="center"
               src="/img/arch-single-standby.svg"
              link="https://pg-auto-failover.readthedocs.io/en/master/intro.html" >}}

At the initial provisioning stage, one node must be selected as the current
primary, and that's where we run `pg_ctl initdb`. Other nodes in the same
_cluster_ — or _formation_, as the _cluster_ name is overloaded in the
Postgres glossary already — are going to be standby nodes and initialised
using `pg_basebackup`, typically.

Then if the current primary node fails or becomes unavailable for a long
enough time to trigger a topology change, one of the standby nodes should be
promoted to be the new primary. It is possible to use `pg_ctl promote` to
implement this step, and the Postgres setup must also be edited so that a
further restart down the line allows the node to restart as a primary still.

Also the other standby nodes must be reconfigured to use the new primary as
their upstream replication source. Editing the `primary_conninfo` setting in
Postgres requires a restart until Postgres 12 included, and can be changed
with a reload starting with Postgres 13.

When the former primary node gets back to being available — maybe that was
just a spurious reboot after all, or a maintenance operation that didn't go
by the book, as it happens sometimes — well then it may re-join the
*formation* as a standby to the current primary. For that to happen it's
necessary to use `pg_rewind`, and in some cases even this tool will fail to
re-join. In that case it might be necessary to resort to using
`pg_basebackup` or another means (such as restoring a backup) to fetch a
full copy of PGDATA all over again.

When all of those options are implemented correctly in some automation
layer, then we have Postgres nodes with actual dynamic roles, rather than
just the architectural concept that roles should be dynamic.

To get started with a failover automation that implements the notion of
dynamic roles, I recommend playing with
[pg_auto_failover](https://pg-auto-failover.readthedocs.io/en/master/intro.html#single-standby-architecture).
Disclaimer: I am the main developer of this tool, so that I know it well
enough to trust it with failover orchestration in production.

Now here is a short list of points that may seem obvious to veterans of HA
system design, or to those of you with memories of production outages. It
might be still good to go over some of the basics to help readers new to HA
planning getting started.

### Capacity planning

As outlined before, the only reason why we provision and maintain standby
nodes in an HA system is for those nodes to take over the primary role when
the current primary node is not available. Not only will that situation
happen, but also we are deploying standby nodes as a way to plan for our
production to tolerate such a fault.

After all High Availability is mostly just a fancy way to say Fault
Tolerance, and in many cases it boils down to Business Continuity.

So if the standby nodes are being provisioned with the single purpose of
taking over the primary role, then the nodes must be as close as possible in
specifications to the current primary. Each one of the HA standby nodes is
expected to become a primary node sometime, so it must have the capacity to
handle whatever workload the current primary is handling.

It means in particular that you want the standby nodes to have the following
properties identical to the primary:

  - CPU power (number of sockets, cores, speed of each of them, bus speed, etc)
  - RAM specifications, bus speed, access latency, amount of RAM
  - Disk space available for the data set
  - Network bandwith and latency to the application nodes
  - Same configuration (kernel, file systems, Postgres tuning, etc)
  - Same security rules (firewall, Postgres HBA, etc)
  - And probably more elements should be added to this list to make it comprehensive.

We can stress out the Postgres HBA file in that list. Connection privileges
must be opened in the same way for the application to be able to connect to
the new primary when a failover has happened, of course, and also the other
standby that you may have must be able to connect to the new primary too.

When listed here in an article that focuses on High Availability roles and
their dynamic nature, I certainly hope all of this sounds obvious. Well, as
obvious as it sounds, the most recent on-call situation I was involved in
where things went wrong because the newly elected primary was not comparable
to the former primary node, and then the application workload was slowed
down a lot... was last week.

So while I agree that it sounds as obvious as preventing _File System is
Full_ on your database instances and their WAL subsystem, well, we still
have to talk about it apparently.

### Long term maintenance of production systems

Production systems are sometimes referred to as “legacy systems”, and they
tend to require some maintenance and even hardware upgrades after a while.
Nowadays, the “hardware upgrade” might be as easy as an automated deploy of
a new fleet of newer spec nodes (VMs, pods, containers, what have you).

Still, when upgrading the capabitilies of the system, then we have to deal
with an heterogeous set of nodes. What then? How do we respect the previous
constraints about every node in the HA system being able to provide with the
same capacity, to handle the same load?

When using pg_auto_failover to for implementing your Postgres HA systems,
simply make sure that any node that has a non-zero [candidate
priority](https://pg-auto-failover.readthedocs.io/en/master/architecture-multi-standby.html#candidate-priority)
is capable of running your actual production workload.

When upgrading your nodes to more powerful and capable systems, simply
adjust the *candidate priority* of the older nodes to zero when the new
nodes are ready, so that pg_auto_failover will refrain from putting the
lesser capable systems in production as primary nodes.

And also remember that you can use [pg_autoctl perform
promotion](https://pg-auto-failover.readthedocs.io/en/master/ref/pg_autoctl_perform_promotion.html)
to smoothly switch over to a new primary when it's ready to take over. Then
it is also possible to use [pg_autoctl drop
node](https://pg-auto-failover.readthedocs.io/en/master/ref/pg_autoctl_drop_node.html)
on the previous generation of nodes to implement their end-of-life cycle.

### Load Balancing

Because proper standby nodes are as expensive as the current primary node, a
negociation might arise where those nodes somehow need to pull their own
weight. Being available when the current primary isn't anymore doesn't cut
it.

The cost of the standby nodes should be compared to the cost of an
interruption of the current primary for the business, possibly for a long
time while it's being rebuilt from pieces — hardware, new VMs that need
provisioning from restoring backups, etc. A common alternative consists of
comparing the price of operating the standby nodes with the fact that most
of their processing capabilities are unused.

As a result, some of the production load might be distributed to the almost
idle standby nodes. In the case when a single standby is part of the
production system, and when some of the workload is routed to it, such as
batches and reporting queries and other analytics or at least read-only SQL
traffic, it looks like a good use of expensive resources.

Now, when the current primary becomes faulty, then the secondary is elected
and promoted as expected... and now as a single node it must be capable of
handling both the primary workload and also the reporting read-only standby
traffic.

In many cases that won't fly. If your application have an easy way to turn
off the read-only traffic for a while, the duration when all you have left
is a single Postgres node, then it might still be a good trade-off. It's not
often that I see that kind of sophistication in the wild though.

The approach I generally see is where monitoring and observability is used
to somehow “ensure” that a single node would be able to take on the entire
workload. Well okay then... but why handling the load-balancing complexities
in that case?

Finally, when using pg_auto_failover to manage your Postgres nodes, setting
the [candidate
priority](https://pg-auto-failover.readthedocs.io/en/master/architecture-multi-standby.html#candidate-priority)
of some of the standby nodes while still allowing them to participate in the
replication quorum might be a good trade-off. The nodes can now participate
in the data safety of your HA system while never being elected as the new
primary. See our [sample architecture with three standby nodes, one
async](https://pg-auto-failover.readthedocs.io/en/master/architecture-multi-standby.html#sample-architecture-with-three-standby-nodes-one-async)
documentation for more details.

### Application side HA and Connection Pooling

Another aspect of Postgres HA and failovers that needs to be part of this
article is the client-side aspect of HA. For an application to benefit from
server-side fault tolerance, it must be ready to lose its connections to the
current primary (when this one fails) and reconnect to the new primary (when
that one is ready).

The Postgres technology is named “Hot Standby” because at promotion time the
current connections and read-only traffic is allowed to continue. That's a
very nice feature which in this context means that the application may
connect early to the newly elected primary without causing too much trouble.

Then, in order to avoid the whole _connection lost_ thing, in some cases an
external connection pooler can be used. Those pieces of infrastructure can
usually be reloaded online with the new primary node as the connection
target.

There is a catch though: an SQL connection (or session really) is stateful.
SQL provides session level objects and features such as prepared statements,
temporary tables, cursors, or LISTEN/NOTIFY. So if you're using any of the
session level SQL features then reconnecting to the new primary node can not
be made transparently. The application is going to expect some kind of state
from its SQL connection, and will not find it again.

> I didn't provide an exhaustive list of the session level SQL features
> here. [pgbouncer features](https://www.pgbouncer.org/features.html)
> documentation page maintains one if you need to read it.

So the idea that for Postgres HA the roles are dynamic must find its way up
to your application code.

### Disaster Recovery Setup

Finally, implementing Postgres High Availability requires implementing
availability of the data whatever happens. This is achieved via the notion
of an “archive”, which is documented as a concept in Postgres itself.

While it is possible to implement archiving using the provided Postgres
hooks and some scripting around, please consider using one of the proven
robust implementations for archiving. The classic projects that people keep
saying some good things about even after running them in production for a
while are [WAL-G](https://wal-g.readthedocs.io), its predecessor
[WAL-E](https://github.com/wal-e/wal-e), then
[pgbackrest](https://pgbackrest.org), and then
[barman](https://pgbarman.org).

Using those projects it is possible to implement a Postgres archive either
on a local storage facility — maybe using the Open Source solution
[min.io](https://min.io) and its Cloud Storage compatible API — or on a
Cloud Storage of your choice.

When browsing the docs and prototyping your Disaster Recovery Setup, make
sure to understand what happens during and after a failover. In my
experience, the Open Source products I listed above to implement DR entirely
miss the point; at least in the docs. Again, the only thing that is known
for sure about the current primary node is that down the road, the primary
role is going to be assigned to another node in the system. 

Make sure your DR setup knows about this. They're not good at documenting
the steps that need to be implemented, if any, when this happens. That's
unfortunate, and I wish this would change. If you're reading this and
contribute to any of the DR systems I listed, can you think about how to
cover the point that any node that participates in some HA system must have
a dynamic role?

## Automated Failover and HA tooling

While it is possible to implement HA yourself from scratch, the planning for
a full failover with minimal (and controlled) data loss and fast enough
service failover is ridden with traps that are not easy to foresee, so it's
advised to use a software that's known to have taken care of the proper
steps for you.

As the main author and contributor to
[pg_auto_failover](https://pg-auto-failover.readthedocs.io/en/master/intro.html#single-standby-architecture),
that's the software I trust and recommend. 

The notion that roles are dynamic is a strong focus of the whole design of
pg_auto_failover. In particular the current primary is not assumed, it's an
information that is maintained through the life time of a formation — that's
[pg_auto_failover
glossary](https://pg-auto-failover.readthedocs.io/en/master/architecture.html#pg-auto-failover-glossary)
for what some people may be used to name a _cluster_.

Remember that the entire application stack must be designed for the node
roles to be dynamic in nature if you need some kind of Fault Tolerance.

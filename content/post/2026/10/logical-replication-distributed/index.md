+++
title     = "Postgres logical replication beyond one writer"
date      = "2026-11-03T14:50:00+0100"
tags      = ["PostgreSQL", "Replication", "Logical Decoding", "Architecture"]
categories = ["PostgreSQL", "Architecture"]
icon      = "🐘"
+++

In 2014, if you wanted Postgres servers on several continents, each of them
accepting writes and all of them sharing their rows, there was one serious
open source answer: BDR, from 2ndQuadrant. It needed a patched PostgreSQL 9.4
and a plugin, which means it needed a server that was not Postgres. Ten years
of work on logical replication have since moved most of what BDR needed into
the stock server, and in this article we build the same kind of system with
Postgres 19 release candidate 1 and pg_auto_failover, to see how far that
incremental progress has taken us, and what is still left to build.

This is the last article of a series on logical replication, and it should
read fine on its own. In [Part
1](/blog/2026/09/ten-years-of-postgres-logical-replication/) we built a
hub-and-workers system for write scalability, in [Part
2](/blog/2026/10/consolidating-databases-with-postgres-logical-replication/)
we consolidated several databases into one warehouse, and in [Part
3](/blog/2026/10/zero-downtime-postgres-upgrades-with-logical-replication/)
we did a zero-downtime major upgrade.

**TL;DR** We built five regions out of pg_auto_failover groups and core logical
replication, and tested them by killing primaries under load. Every
acknowledged row survived, every region ended with the same rows, and a fifth
region joined the running system with five statements and no pause. Core now
does the hard parts: row filters, loop prevention, failover-safe slots,
sequence synchronization, and it counts conflicts. It still doesn't replicate
DDL, so schema changes are a short pause, or pglogical or pgcopydb when a pause
isn't an option. It doesn't resolve conflicts or hand out unique ids to several
writers either, and we avoid both by construction.

<!--more-->

{{< lab >}}
The `compose/` directory next to this article is the test system: five
pg_auto_failover groups of a monitor and three nodes, a writer in every
region, and the failures we inject on purpose: `kill -9` on a primary,
standbys cut off from the primary's WAL, DDL under write load. After each run
we compare what every region holds with what its writers were told was
committed: rows missing, rows twice, rows that one side has and the other never
will. pg_auto_failover and pgcopydb are built from their `main` branches,
because the features we use are not in a release yet. The failover and cascade
drills run on Postgres 17, the conflict tests on Postgres 18 and on
`postgres:19rc1`. Every number in the article comes from these runs, three of
each unless we say otherwise.
{{< /lab >}}

<!--toc-->

## The system we want

Take a company with customers on four continents and a head office that wants
to see all of it. Each region's data is written close to its users and belongs
to that region. A region survives the loss of its primary without the others
noticing. The head office has a warehouse that sees everything. A fifth
region can be added next year without taking anything down. That's the system
BDR was designed for in 2014, and this is the shape we give it:

{{< image src="fig-region-map.svg" title="One writer per region: conflicts avoided by construction." >}}

Four regions own data: us-east in New York, us-west in San Francisco,
ap-south in Mumbai and ap-southeast in Sydney. A fifth site, a warehouse in
Amsterdam, owns nothing and holds a copy of everything. Each site is a
pg_auto_failover group of its own, a primary and two standbys, and the groups
share no failover. They only talk through logical replication, in both
directions: a region's own rows flow one way, and a read-only mirror of
everybody else's rows flows back.

That's five problems to solve, and we take them in this order:

  1. when the primary of a region dies, its change stream must not lose a
     row or invent one;
  2. two regions must never write the same row, so we have no conflict to
     resolve;
  3. a failover in one region must not break the links of the others;
  4. the schema changes everywhere, while the regions keep writing;
  5. a new region can be added to the running system, with a first copy of
     what the others hold, and no maintenance window.

Core Postgres replicates the rows. pg_auto_failover runs the failover of
each group and keeps the settings of the replication slots right while it
does. Core doesn't replicate DDL, and that's the one place where we look at
other tools, pglogical and pgcopydb. Here is what core learned since BDR, for
the parts of the system that use it:

| What we need | In Postgres since |
|---|---|
| Say what flows, in SQL | 10, publications and subscriptions |
| Say which rows and columns | 15, row filters, column lists |
| Replicate both ways without a loop | 16, `origin = none` |
| Keep the slot when the primary is lost | 17, failover slots and `synchronized_standby_slots` |
| See the conflicts | 18 logs and counts them, 19 `retain_dead_tuples` for `update_deleted` |
| Move the sequences | 19, `FOR ALL SEQUENCES` and `REFRESH SEQUENCES` |

What's missing from the table is what Postgres still doesn't do: replicate
DDL, resolve a conflict, and give unique sequence values to several writers.
We get back to each of them.

## A region is a group of three

When the primary of a region fails, a logical consumer is attached to it, and
we want it to get every row exactly once, or at least not to miss one. In our
system that consumer is another region's subscription.

Postgres 17 added `failover = true` for a replication slot, and the
`synchronized_standby_slots` setting. What they promise is this: a logical
walsender on a failover slot won't send a client any WAL past what every
standby listed in `synchronized_standby_slots` has already flushed. So when the
primary dies and one of those standbys is promoted, nothing the consumer has
received can be missing from the new primary. What can happen is duplicates,
because the position the consumer confirmed may be behind what it processed.
Never a hole.

{{< image src="fig-failover-slot.svg" title="A failover-safe logical slot, driven by pg_auto_failover's existing orchestration." >}}

That promise comes with conditions, and each one fails silently when you
miss it:

  - the slot must be created with `failover = true`, a plain slot gets no
    protection at all;
  - `synchronized_standby_slots` must list the standby that is going to be
    promoted, or the gate never applied to the new primary;
  - the standby needs `sync_replication_slots = on`, `hot_standby_feedback =
    on` and a `dbname` in its `primary_conninfo`.

A region doesn't have to run on pg_auto_failover. Patroni is the other widely
used way to run a group, and the high availability of a cloud provider's
managed service is a third. We use pg_auto_failover because we know it well,
and we could contribute failover slots support to it while preparing this
article. As far as the documentation says, Patroni keeps its own [permanent logical
slots](https://www.dbi-services.com/blog/surviving-a-patroni-failover-with-logical-replication/),
which it creates on the leader and copies to the standbys, advancing their
positions periodically: a mechanism older than the `failover` flag of Postgres
17. For RDS for PostgreSQL 17 and later, AWS documents [slot
synchronization](https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Appendix.PostgreSQL.CommonDBATasks.pglogical.slot.synchronization.html)
with `failover = true`,
`synchronized_standby_slots` and a database name in
`rds.logical_slot_sync_dbname`, and Azure [documents
that](https://learn.microsoft.com/azure/reliability/reliability-postgresql-flexible-server)
slots of a flexible server survive a failover with `sync_replication_slots` and
`hot_standby_feedback`. On Google Cloud SQL, it is
[documented](https://docs.cloud.google.com/sql/docs/postgres/advanced-dr-logical-failover-slot)
for PostgreSQL 17 and later on the Enterprise Plus edition, through its
cross-region disaster recovery replica, with `failover = true` on the
subscription and the replica as a synchronized standby; the pages about its
regular high availability instances don't say what happens to a logical slot
there. We haven't tested any of them. There are many more providers than we
could read, from Scaleway and OVHcloud to the large ones in the US, China,
Russia or India, and each has its own answer, so check the documentation of
what you run before relying on it.

In a pg_auto_failover group, listing the standbys in
`synchronized_standby_slots` (the second condition above) is the monitor's job.
It computes the list the same way it computes
`synchronous_standby_names`, as the physical slot of every failover candidate,
and `pg_autoctl` applies it on the primary and updates it when the candidates
change:

~~~
$ pg_autoctl show state
        Name |  Node |      Host:Port |       TLI: LSN |   Connection |      Reported State |      Assigned State
   ----------+-------+----------------+----------------+--------------+---------------------+--------------------
   us-east-a |     1 | us-east-a:5432 |   1: 0/5002AD8 |   read-write |             primary |             primary
   us-east-c |     2 | us-east-c:5432 |   1: 0/5002AD8 |    read-only |           secondary |           secondary
   us-east-b |     3 | us-east-b:5432 |   1: 0/5002AD8 |    read-only |           secondary |           secondary

$ pg_autoctl show synchronized-standby-slots
'pgautofailover_standby_2,pgautofailover_standby_3'
~~~

It doesn't create the logical slot though, that's for the consumer to do, and
there are three things the consumer must do on its side:

  1. create its slot with `failover = true` (for a subscription that's
     `create subscription ... with (failover = true)`);
  2. store its own position durably, as the last `end_lsn` it fully applied,
     and never restart from zero;
  3. at every reconnection, check the server it reaches before it streams.

The check is one replication command, which a consumer can send on the
connection it is about to stream from. Here it is on the new primary, after
`us-east-a` was killed and `us-east-c` promoted:

~~~
$ psql "dbname=app replication=database" -Atc "IDENTIFY_SYSTEM"
7703946002234535959|2|0/5049AD8|app
~~~

The columns are the system identifier, the timeline (2: the history forked
once), the position of the server's WAL (`xlogpos`) and the database. The
consumer compares them with what it stored, and refuses to stream if its
stored position is past `xlogpos`, or past the point where the history it
read from was left behind: a consumer ahead of the history it reads from is
the one case the slot can't protect. For the second half of that rule, the
operator's view of the same history is `pg_autoctl show timeline`, which lists
the timelines with the position where each one was forked from its parent
(the switchpoint), and each node's position against them:

~~~
$ pg_autoctl show timeline
     TLI | Parent TLI | Switchpoint LSN
---------+------------+----------------
       1 |          0 |             0/0
       2 |          1 |       0/50497C0

                Name | NodeId |  TLI |         LSN | Status
---------------------+--------+------+-------------+-----------------------------------------
           us-east-a |      1 |    1 |   0/50021D0 | ok, on accepted lineage
           us-east-c |      2 |    2 |   0/5049AD8 | ok, on accepted lineage
           us-east-b |      3 |    2 |   0/5049AD8 | ok, on accepted lineage
~~~

Timeline 2 forked from timeline 1 at `0/50497C0`. A consumer that had
received anything on timeline 1 past that position holds rows the new primary
doesn't have.

We tried it: a region is a monitor and three nodes on Postgres 17, and a
`pg_recvlogical` consumer follows a failover slot while a writer inserts
about 25 rows a second, one per transaction. Each scenario ran three times,
and the table gives the range. "Holes" are rows the new primary has that the
consumer never received, and "ahead" are rows the consumer received that the
new primary doesn't have.

| Scenario | Duplicates | Holes | Ahead | Writer blocked |
|---|---|---|---|---|
| planned `pg_autoctl perform failover` | 0 | 0 | 0 | 1.4 to 2.2 s |
| primary killed | 2 to 4 | 0 | 0 | 51 to 52 s |
| same, with `synchronized_standby_slots` emptied | 0 | 3 to 13 | 69 to 92 | 51 to 53 s |

The slot does what it says. The consumer is never ahead when the setting is
right, even when we cut the standbys off from the primary's WAL before the
kill: it just receives nothing from then on, because the walsender waits for
them. When we empty the setting, the consumer ends up with 69 to 92 rows the
new primary never had, and the new primary then writes at WAL positions the
consumer already passed, so it never sees 3 to 13 rows. That's exactly the case
the `IDENTIFY_SYSTEM` check is for, and nothing in Postgres raises it for you.

After the kill, `pg_autoctl` has promoted `us-east-c`, moved to timeline 2, and
shrunk the list to the one standby left:

~~~
$ pg_autoctl show synchronized-standby-slots
'pgautofailover_standby_3'
~~~

The slot isn't what costs time: a killed primary blocks the writer for about
51 seconds, roughly ten for the monitor to declare the node unhealthy and the
rest for pg_auto_failover's drain timeout. Those are defaults you can tune,
measured here on a local Docker network. They lean on the conservative side,
because they're chosen to protect against network partitions and split brain
rather than to fail over as fast as possible; the
[Failover and Fault Tolerance](https://pg-auto-failover.readthedocs.io/en/main/fault-tolerance.html)
page of the pg_auto_failover documentation details each timeout and what it
guards against, so read it before shortening any of them.

## Regions own their rows

We link sites with replication, not with consensus, so we avoid conflicts
instead of resolving them: the region is part of the key of every table.

~~~ sql
create table events (
  region     text   not null,
  event_id   bigint not null,
  payload    text,
  written_at timestamptz not null default clock_timestamp(),
  primary key (region, event_id)
);
~~~

No row can be written by two regions, because its key says who owns it. The
application picks the value of `region` and that's the only place where the
rule is enforced. Replication needs no rule at all, and each region can use
its own sequence for `event_id`: with a `(region, id)` key, we don't need a
global sequence. It costs a wider key, and that's all.

Each link is a publication with a row filter, one of the Postgres 15
features. The regions form a chain, and each hop carries only the rows the
other side doesn't have:

{{< image src="fig-publications.svg" title="Eight publications, one rule: a link carries only what the other side lacks." >}}

New York publishes two views of its table, one for Amsterdam and one for San
Francisco, and San Francisco only publishes its own rows back:

~~~ sql
-- on us-east
create publication pub_to_eu_west for table events
  where (region = 'us-east' or region = 'us-west');
create publication pub_to_us_west for table events
  where (region <> 'us-west');

-- on us-west
create publication pub_to_us_east for table events
  where (region = 'us-west');

-- on us-west again, the subscription to us-east
create subscription sub_us_west_from_us_east
  connection 'host=us-east-a,us-east-b,us-east-c port=5432 dbname=app user=postgres
              target_session_attrs=read-write'
  publication pub_to_us_west
  with (failover = true);
~~~

Two details in that subscription do the work of the previous section.
`failover = true` makes the slot it creates on the publisher a failover slot,
so the standbys keep a copy of it. And the connection string lists the three
nodes of the group with `target_session_attrs=read-write`, so the
subscription finds the primary wherever it is after a failover. The
subscription that a group runs itself needs nothing: it's in the catalogs,
which physical replication ships to the standbys, and an apply worker only
starts on a node that is a primary.

Why not `origin = none`, the Postgres 16 way to avoid loops? With it the
publisher doesn't send rows that carry a replication origin, meaning rows that
were themselves applied from somewhere else. It's all or nothing, though: on
San Francisco's subscription it would drop Amsterdam's mirror of Mumbai and
Sydney together with the loop, because those rows arrive carrying an origin too
once New York has applied them. A row filter looks at a value every row has,
whatever path it took. `origin = none` remains the tool when you don't have
such a column.

We ran this topology on Postgres 17, with two cascades, one warehouse and 2,000
concurrent writes in each region. Without the filters the writes echo around
the loop. With them, every node ends up with 8,000 rows, exactly 2,000 per
region, and no apply or sync error on any subscription:

~~~ sql
select region, count(*) from events group by region order by region;
-- on every primary and standby: 2,000 for each of the four regions
~~~

## When a region fails over, and when it's far away

The previous drill had one consumer. Here we have two regions subscribed to
each other, so what does a failover in one do to the replication with the
other, in both directions at once?

{{< image src="fig-region-failover.svg" title="One region fails over, with logical replication to a second region in both directions." >}}

We ran `us-east` and `us-west`, each a group with a monitor and three nodes,
linked both ways with the publications and subscriptions above and a writer
on each side. We killed the primary of one region under load, three times for
each, and waited for both sides to hold the same rows. The result is the same
in the six runs: every acknowledged row is on both sides, none is missing,
none is twice. In one run `us-east` had acknowledged 1,532 rows and `us-west`
2,552, and afterwards both held exactly those.

The two directions don't come back at the same time. The subscription that
the failed region runs resumes 21 to 23 seconds after the kill, from its
replication origin, as soon as a standby is promoted. The other region's
subscription can't connect until the new primary accepts writes, 52 to 54
seconds in, and it finds out by retrying every five seconds. It then resumes
from the synced slot, with no resync. All of this ran with the default
synchronous replication of pg_auto_failover. If a group's standbys are behind
when its primary dies, the other region's slot can be ahead of the promoted
node, which is a good reason to keep that default.

Two regions don't tell us what a cascade does. In the five-region system
`us-east` is a hub: `us-west` reads everything from it, the warehouse reads its
rows from it, and the rows of `ap-southeast` reach `us-west` through three
relays (`ap-south`, the warehouse, `us-east`). We ran a writer in each of the
four regions, about 25 rows a second each, and killed the primary of `us-east`,
three times.

{{< image src="fig-hub-failover.svg" title="Killing the hub: its own writer waits, the rows downstream of it stand still." >}}

The writer of `us-east` waits 51 to 52 seconds for its new primary, as before.
The three others never wait more than 0.2 seconds, because nothing they write
depends on `us-east`. What depends on it is the flow of rows: the `us-west`
rows stand still on the warehouse for 54 seconds, and the `ap-southeast` rows
on `us-west` for 54 to 56, until the subscriptions reconnect to the new
primary. Then they catch up, and two seconds after we pause the writers every
one of the five primaries holds every acknowledged row of every region, about
2,200 for `us-east` and 3,500 for each of the others, with identical ids. The
two subscriptions that read from `us-east` count one apply error each, the
connection that dropped, and no other subscription counts any.

A cascade is a dependency, then: the failover of a hub delays everything
downstream of it by the length of the failover, and loses nothing. The links
between our regions are asynchronous on purpose: a write in Sydney is
acknowledged without waiting for Amsterdam, and distance shows up as lag.

## Schema changes: the one gap

A table changes on one primary, and the others must follow, at the right place
in the stream. Inside a group the standbys get it from physical replication, so
there's nothing to do. Between groups, core doesn't replicate DDL. The apply
worker checks every column of an incoming row against the local table, and
fails when one is missing (an extra local column is fine):

~~~
ERROR:  logical replication target relation "public.events" is missing replicated column: "note_a"
~~~

So a column added on the publisher first stops the subscriber at the next
row, and the worker retries until the column exists. We ran `add column`
three ways with a writer in each region, three runs each, and lost nothing:

{{< image src="fig-ddl-rollout.svg" title="Three ways to add a column on regions that write to each other." >}}

| Rollout | Apply errors | Effect |
|---|---|---|
| `us-east` first, `us-west` ten seconds later | 4 on `us-west`, every run | `us-west` 90 to 178 rows behind after 10 s, caught up 15 to 16 s after the first `alter` |
| both primaries at the same moment | 0 in 8 runs of 9 | nothing visible, except one retry |
| writes paused, drained, both altered, writes resumed | 0 | writers blocked 0.9 to 1.5 s |

The second line is luck: the window is a few milliseconds and a busier writer
would find it. The first is what happens when you do one region then the
next. Adding a column wants the subscriber changed first, dropping one wants
the publisher first, and on a two-way link each region is both, so no order
is safe, only a short pause is. For the same reason a rename can't be a
rolling change here, and you do *expand, copy, switch, contract* instead.

A pause is three steps. The gate closes: writers stop starting transactions,
and the ones in flight finish. We wait for the links to drain, until every
region holds every row the others committed, so no change made on the old schema
is still on its way. Then we alter every primary, and reopen the gate. In the
lab the gate is a flag file that each writer checks between two transactions.
In production the usual gate is the pooler: PgBouncer's `PAUSE` waits for the
running queries to finish and holds the new ones, and `RESUME` lets them go,
so the application sees a latency bump and no error. We haven't run it behind
PgBouncer.

The pause is the baseline, and it scales to the five-region system. After the
failover of the previous section, we paused the four writers, ran the same
statement on the five primaries one after the other, and resumed them:

~~~ bash
# writers paused and drained, then, on each of the five primaries in turn:
for region in us-east us-west ap-south ap-southeast eu-west; do
  psql "host=$region-a,$region-b,$region-c dbname=app target_session_attrs=read-write" \
       -c "alter table events add column note text"
done
~~~

That took 0.4 seconds from the first `alter` to the last. Thirty seconds of
writes on the new schema later, all five regions hold every acknowledged row
with identical ids, the column exists on each, and no subscription counts a
new error. That's three runs.

### When a pause isn't an option

Two tools put the schema change in the stream instead. pglogical doesn't
replay DDL by itself, but it replays the statement you hand to
`pglogical.replicate_ddl_command()`, which runs it locally and sends it down the
same stream:

~~~ sql
select pglogical.replicate_ddl_command(
  $$ alter table events add column note text $$);
~~~

With two nodes subscribed to each other and both inserting, a plain `alter
table` on one node stops the other's apply worker at the first row with the new
column, and it comes back 187 seconds later because its restart delay grows with
each failure. Through `replicate_ddl_command` the other node has the column with
nothing run there, and no lag, a rename included. There is no event trigger
behind it: whoever changes the schema, a DBA or a migration tool, has to wrap
each statement in the call. The README warns that this isn't enough once the
subscriber keeps writing with the old structure, which is the case with two
writers on one table. Our test didn't hit that case.

pgcopydb captures DDL on its own. An event trigger on the source emits each
command with `pg_logical_emit_message` (Postgres 9.6, and `pgoutput` sends it
since 14), `pgcopydb follow` decodes the message, and the apply process runs it
on the target at the place it had in the stream. `pgcopydb stream event-trigger`
installs the trigger, which looks like this:

~~~ sql
create function pgcopydb.emit_ddl_message() returns event_trigger
  language plpgsql as $f$
declare
  payload json;
begin
  payload := json_build_object('type', 'ddl', 'body', current_query());
  perform pg_logical_emit_message(true, 'pgcopydb', payload::text);
end;
$f$;

create event trigger pgcopydb_emit_ddl_message on ddl_command_end
  when tag in ('CREATE TABLE', 'ALTER TABLE', 'DROP TABLE',
               'CREATE INDEX', 'DROP INDEX', 'CREATE SEQUENCE',
               'ALTER SEQUENCE', 'DROP SEQUENCE', 'CREATE VIEW',
               'ALTER VIEW', 'DROP VIEW')
  execute function pgcopydb.emit_ddl_message();
~~~

{{< image src="fig-pgcopydb-message.svg" title="DDL as a message, not a queue table." >}}

We replaced the native subscription from `us-east` to `us-west` with `pgcopydb
clone --follow` (a one-way link), with a writer inserting about 29 rows a
second, and ran `add column`, `rename column` and `drop column` on `us-east`
without touching `us-west`. Three runs: it keeps up, 27 to 30 rows a second
in and the same out, and each change reached `us-west` within a second, with
no lag at any point. The rename is the change that no ordering makes safe on
the native link. With `--enable-failover` the slot is `failover = true` on the
primary and `synced = true` on both standbys, and after we killed the `us-east`
primary `us-west` ended with every row, nothing left to catch up one second
after the writer stopped (three runs).

| | Core | pglogical | pgcopydb |
|---|---|---|---|
| Schema change in the stream | no | `replicate_ddl_command` | event trigger |
| Rename with writers on both sides | no | works, the README warns | one-way link only |
| Row filters | yes | yes | tables only |
| Failover slot | yes | after the fact | `--enable-failover` |
| Both directions | yes | yes | no |

None of them has every row of that table. The combination we want, native
filters and failover slots with the schema change in the stream, doesn't
exist off the shelf today. What works is the discipline of the first column:
additive, nullable changes, applied everywhere with a short pause, and
*expand and contract* for the rest.

## Adding a region

The system holds 600,000 rows by now, 75 MB, 200,000 in each of the three
regions that own data, with a writer in each of them. We bring up a fifth
group, `ap-southeast`, a monitor and three nodes, and join it with five
statements: its schema, a publication in each direction, and the two
subscriptions. `ap-south` is its neighbour, so it serves the first copy:

{{< image src="fig-add-region.svg" title="A fifth region joins the running system." >}}

~~~ sql
-- on ap-south
create publication pub_to_ap_southeast for table events
  where (region <> 'ap-southeast');

-- on ap-southeast, after creating the same table
create publication pub_to_ap_south for table events
  where (region = 'ap-southeast');
create subscription sub_ap_southeast_from_ap_south
  connection 'host=ap-south-a,ap-south-b,ap-south-c port=5432 dbname=app user=postgres
              target_session_attrs=read-write'
  publication pub_to_ap_southeast
  with (failover = true);

-- on ap-south
create subscription sub_ap_south_from_ap_southeast
  connection 'host=ap-southeast-a,ap-southeast-b,ap-southeast-c port=5432 dbname=app user=postgres
              target_session_attrs=read-write'
  publication pub_to_ap_south
  with (failover = true, copy_data = false);
~~~

`ap-south` holds its neighbours' rows as well as its own, so a single link
brings the new region everything. The rows of `ap-southeast` reach the others
through the publications they already have: `pub_to_eu_west` on `ap-south` lists
the region since the beginning, and the warehouse and the two US regions relay
it. Nothing changes on any of them.

We did it three times with the writers running, and compared what every
primary holds with what each writer was told was committed. The new group is
up 8 to 11 seconds after we ask for it, the first copy of 600,000 rows takes 2
seconds, and the source slot never kept more than 1 MB of WAL. No writer
waited more than 0.2 seconds between two rows at any point, joining included.
Two seconds after the writers stop, all five regions hold every acknowledged
row of every region, with identical ids, and there's no apply or sync error on
any subscription. That's the part core does without a maintenance window.

It's also a small database, and a first copy that takes two seconds isn't what
worries anyone. Core copies each table over one stream with `COPY`, a few
tables at a time, and starts a table again when the connection drops. A slot
keeps the WAL since its position until its consumer confirms it, so a long copy
makes a busy source keep its WAL: at 3 MB a second, a ten-hour copy keeps about
100 GB. `max_slot_wal_keep_size` and a look at `pg_replication_slots.wal_status`
belong in the plan of any long first copy. When the table is large and the link
long, or when the new region can't share a binary format with the others, we
reach for pgcopydb, which opens as many streams as we allow, can split one table
over several of them and remembers which tables are done. It runs between the
two servers, and writes every change twice, to a SQLite file and to the target,
so we'd keep it for the copy and not for the steady links.

Can it do the copy and hand the stream over to core, so that the source doesn't
keep the WAL of a long copy and the steady link stays in-core? It can, with
care. pgcopydb receives the changes while it copies and spools them to files on
its own host, so the source recycles its WAL during the copy. We ran it with a
writer inserting 50 rows a second: `pgcopydb clone --follow --plugin pgoutput`
copied 300,000 rows and followed, we stopped it at a marker with `pgcopydb
stream sentinel set endpos --current`, and a core subscription took over its
slot and its publication:

~~~ sql
-- on the target, once pgcopydb has stopped and applied what it had spooled, up to :lsn
create subscription takeover
  connection 'host=e-w1 dbname=app user=postgres'
  publication handover                     -- the publication pgcopydb created
  with (create_slot = false, slot_name = 'handover',
        copy_data = false, enabled = false);

select pg_replication_origin_advance('pg_' || oid, :'lsn')
  from pg_subscription where subname = 'takeover';

alter subscription takeover enable;
~~~

Two details decide whether rows survive. pgcopydb confirms to the source what
its receiver has spooled, not what its applier has written, so right after it
stops the slot is a few seconds ahead of the target. A second pass, `pgcopydb
follow --resume --not-consistent --endpos` with the slot's `confirmed_flush_lsn`,
applies that spool, and that position is the one to give
`pg_replication_origin_advance`. And the publication must be pgcopydb's:
`pgoutput` decides what to send with the publications as they were when the
change was made, so a publication created at the handover skips every row
committed before it existed (96 rows in the run that did it). With both in
place, three runs end with identical ids on the two servers and no apply error.

pglogical can't read that slot, because it speaks its own output plugin, so it
takes over another way. Its subscription creates its own slot as soon as it
exists, and we disable it right away, let pgcopydb stop at its marker, drop
pgcopydb's slot and enable it. The slot starts before pgcopydb stops, so the
apply worker, which starts at creation, fails on rows pgcopydb has already
applied, six duplicate-key errors in each run before we disable it. Once enabled
it replays from its slot, and three runs end with identical ids too.
`pglogical.replicate_ddl_command` then carries a schema change, with the table
name schema-qualified, which core can't.

The trade-off is a handover that has to be exact, which a first copy done by core
alone doesn't ask for. In exchange the copy runs in parallel with the
resumable streams of pgcopydb and the source doesn't hold its WAL for it, and
the steady link is core's, or pglogical's if we want DDL in the stream.

## When two writers do touch the same row

Not every schema can be split by owner. A reference table that every region
edits is the usual counterexample, and then conflicts are real. Here is what can
happen to one row:

{{< image src="fig-conflict.svg" title="Two writers, one row: what each policy leaves behind." >}}

We ran two
nodes subscribed to each other, with four clients per node updating the same
1,000 keys at 2,000 transactions a second per node for 15 seconds, about 60,000
updates, three runs each.

Core applies what it receives and counts the conflicts. Postgres 18 notices an
update that lands on a row last changed by another origin (the server needs
`track_commit_timestamp` for that), and `pg_stat_subscription_stats` counts it
by type:

~~~ sql
select key, value
  from pg_stat_subscription_stats s, jsonb_each_text(to_jsonb(s))
 where key like 'confl_%' and value::bigint > 0;
~~~

Each node counted 15,200 to 15,300 `update_origin_differs`, about half of the
updates it received. Both nodes ended with the same rows in all six runs, on
Postgres 18 and on 19rc1, and the sums say that 10 to 20 of the 60,000
increments were overwritten by a concurrent update of the same row. Nothing in
core picks a winner, though: it applies the change it receives, and that the
nodes converged is what this workload did, not a rule.

Postgres 19 adds one more thing to count. We ran the same keys again with
updates, deletes and upserts mixed (60, 20 and 20 percent). With
`retain_dead_tuples = on` on both subscriptions, the subscriber keeps the dead
rows long enough to tell an update that raced a delete from a row that was
never there, and counts it as `update_deleted`: 1 to 3 per node and run. Without
it the same race is counted as `update_missing`, 0 to 2 per run. The nodes
converged in all six of those runs too. Core counts, and it doesn't
resolve, so what we do with the counter is ours. pglogical does resolve, with
five policies: `last_update_wins` converged every time, though it keeps one row
and not both (8, 12 and 22 of about 60,000 increments lost), `keep_local` and
`first_update_wins` leave each node with its own value and nothing in the logs
says so, and `error` stops replicating until someone comes.

The other half of multi-writer is sequences. Postgres 19 has `create
publication ... for all sequences` and `alter subscription ... refresh
sequences`. We ran it on `postgres:19rc1`: after 1,000 rows with ids drawn from
a sequence on the publisher, the subscriber has the rows and its own sequence
still at 1, because rows stream and sequences don't. After `refresh sequences`
it's at 1,000, and the next `nextval` after a promotion would be 1,001. That's
good for a cutover. It's not a way for two writers to share a sequence, which
is why we use the `(region, id)` key. The *Sequence Access Methods* patch set
([commitfest #4677](https://commitfest.postgresql.org/52/4677/)) was returned
with feedback for Postgres 19, so it's not in. `galloc` and `snowflakeid` remain
features of EDB Postgres Distributed, the commercial successor of BDR.

## What BDR did, and what's left

BDR 1.0, a patched 9.4 and a plugin, has its last commit on the 1.0 branch in
November 2019. Its successors aren't open source, so the comparison is one of
designs, read from [its
documentation](https://github.com/2ndQuadrant/bdr/tree/bdr-plugin/REL1_0_STABLE/doc),
and we didn't run any of it.

| | BDR 1.0 | What we built |
|---|---|---|
| DDL | replicated, under a global lock: every node reachable and caught up, writes pause or fail meanwhile, a down node must be removed first | no lock and nothing automatic: a short pause, or the change in the stream |
| Sequences | global sequences allocated by a vote of the majority, deprecated in the manual in favor of step and offset | none: the key is `(region, id)` |
| A node is down | one slot per peer holds WAL on all the others until the node is parted | a node is a group of three, the slot follows through `failover = true` |

BDR 1.0 bought "every change reaches every node", rows, schema and keys, with
a coordination cost we refuse to pay here: no global lock, no majority vote for
a sequence, no pause of writes on a continent because one node is slow. And
its manual says nothing about what happens to a peer's slot when a node's own
server fails over, because on 9.4 the primitive didn't exist. The slot
synchronization of Postgres 17 is the piece BDR 1.x lacked.

{{< image src="fig-layers.svg" title="What does the work, and what is left." >}}

What's left is the part that's still a script, a procedure or a discipline:

  - **The consumer after a promotion.** Nothing checks that a consumer isn't
    ahead of the server it reconnects to, so that's the `IDENTIFY_SYSTEM` check
    of the first section, in your own code. It's the *Logical replication
    nodes* item of the pg_auto_failover
    [roadmap](https://oss.theartofpostgresql.com/roadmap/): first-class nodes
    for CDC consumers, next to the physical standbys, in the failover state
    machine. The project is [funded by the teams that run
    it](https://oss.theartofpostgresql.com/pg_auto_failover/).
  - **Schema changes.** Core carries none of them. A short pause of the writers
    does the job for additive changes, and pgcopydb or pglogical put the change
    in the stream when the pause isn't an option.
  - **Joining and leaving.** BDR had `join` and `part`. Adding a region to the
    system above is the five statements shown earlier, and retiring one is the
    same in reverse, by hand.
  - **Sequences and conflicts.** Postgres 19 copies sequence values on demand
    and counts conflicts. Nothing allocates ids across writers or resolves a
    conflict, and avoiding both by construction is still the only built-in
    answer.

None of this was possible ten years ago, and most of it is other people's
work. Logical decoding arrived in 9.4 with Andres Freund, and gave the WAL a
meaning outside of the server. pglogical and BDR, from Petr Jelinek, Craig
Ringer and their colleagues, showed what it was for, and the publications and
subscriptions of Postgres 10 made it part of core. The releases since have
been the long tail of making it usable in real systems, carefully reviewed on
the mailing list, one release at a time. That a primary can die without its
change stream losing or inventing a row, with the standbys following the slot
by themselves, comes from years of exactly that.

Every article in the series has a `compose/` directory, this one included.
Run them, break them, and tell us what we got wrong.

+++
title     = "Postgres logical replication beyond one writer"
date      = "2026-10-27T09:00:00+0100"
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
Postgres 19 release candidate 1, pg_auto_failover and pgcopydb, and see where
it still falls short.

This is the last article of a series on logical replication, and it should
read fine on its own. In [Part
1](/blog/2026/09/ten-years-of-postgres-logical-replication/) we built a
hub-and-workers system for write scalability, in [Part
2](/blog/2026/10/consolidating-databases-with-postgres-logical-replication/)
we consolidated several databases into one warehouse, and in [Part
3](/blog/2026/10/zero-downtime-postgres-upgrades-with-logical-replication/)
we did a zero-downtime major upgrade.

**TL;DR** Core Postgres now does the hard parts of multi-region replication:
row filters, loop prevention, failover-safe slots, sequence synchronization.
What it still doesn't do is replicate DDL, resolve conflicts, or hand out
unique ids to several writers. We avoid the conflicts by construction, and
we look at what pglogical and pgcopydb offer for the schema changes.

<!--more-->

{{< lab >}}
The demos are in the `compose/` directory next to this article. They build
pg_auto_failover and pgcopydb from source, because the features used here
live on their `main` branches and are not in a release yet. Failover,
cascading and schema demos run on Postgres 17, the comparison of the three
tools and the conflict tests on Postgres 18, and the sequence demo on
`postgres:19rc1`. For anything else about Postgres 19 I read the
`REL_19_RC1` source and release notes. Every number in the article comes from
a run of these demos.
{{< /lab >}}

<!--toc-->

## What we're building

{{< image src="fig-region-map.svg" title="One writer per region: conflicts avoided by construction." >}}

Four regions own data: us-east in New York, us-west in San Francisco,
ap-south in Mumbai and ap-southeast in Sydney. A fifth site, a warehouse in
Amsterdam, owns nothing and holds a copy of everything. Each site is a
pg_auto_failover group of its own, a primary and two standbys, and the groups
share no failover. They only talk through logical replication, in both
directions: a region's own rows flow one way, and a read-only mirror of
everybody else's rows flows back. New York and Mumbai link straight to
Amsterdam, and San Francisco and Sydney cascade through them instead of each
opening a long-haul connection of their own.

That's five problems to solve, and we'll take them in this order:

  1. when the primary of a region dies, its change stream must not lose a
     row or invent one;
  2. two regions must never write the same row, so we have no conflict to
     resolve;
  3. the links are bidirectional and some of them cascade, without a row
     ever going back where it came from;
  4. the schema changes everywhere, while the regions keep writing;
  5. a new region can be added, with a first copy of a large database that
     doesn't need a maintenance window.

Core Postgres replicates the rows. pg_auto_failover runs the failover of
each group and keeps the settings of the replication slots right while it
does. pgcopydb does the first copy, and can carry the schema changes in the
stream. pglogical is the older alternative to core, and still has a couple of
things core lacks.

## What Postgres learned in ten years

BDR needed a patched 9.4 because the pieces it relied on weren't in Postgres
yet. Here is what our system needs, and the release where Postgres got it. I
went through the release notes of each major version, from 9.4 to 19rc1, to
write this table.

| What we need | In Postgres since | Before that |
|---|---|---|
| Decode the WAL into row changes, `REPLICA IDENTITY` choosing the key | 9.4 (2014) | triggers and queue tables (Londiste, Slony) |
| Know where a change comes from, resume a stream by position | 9.5, replication origins | out-of-tree patches |
| Put something that's not a row in the stream | 9.6 `pg_logical_emit_message`, 14 for `pgoutput` | a table |
| Say what flows, in SQL | 10, publications and subscriptions | an extension on each side |
| `TRUNCATE`, and partitioned tables on the subscriber | 11, 13 | nothing |
| Don't wait for a huge transaction to commit | 14 streaming, 16 parallel apply, 18 `streaming = parallel` by default | waiting |
| Say which rows and columns | 15, row filters, column lists, `FOR TABLES IN SCHEMA` | replication sets in an extension |
| Survive a bad row | 15, `ALTER SUBSCRIPTION ... SKIP`, `disable_on_error` | editing the catalogs |
| Replicate both ways without a loop | 16, `origin = none` | a convention in the schema |
| Decode on a standby | 16 | reading the primary only |
| Keep the slot when the primary is lost | 17, failover slots and `synchronized_standby_slots` | a script, and a gap |
| Build a logical replica from a physical one, keep slots over `pg_upgrade` | 17, `pg_createsubscriber` | a full initial copy |
| See the conflicts | 18 logs and counts them, 19 `retain_dead_tuples` for `update_deleted` | reading the apply error |
| Move the sequences | 19, `FOR ALL SEQUENCES` and `REFRESH SEQUENCES` | a script |

What's missing from the table is what Postgres still doesn't do: replicate
DDL, resolve a conflict, and give unique sequence values to several writers.
We'll get back to them.

## A region that survives its primary

When the primary of a region fails, a consumer is attached to it, and we want
it to get every row exactly once, or at least not to miss one.

Postgres 17 added `failover = true` for a replication slot, and the
`synchronized_standby_slots` setting. I read the code at the `REL_19_RC1` tag
to see what they really promise: a logical walsender on a failover slot
won't send a client any WAL past what every standby listed in
`synchronized_standby_slots` has already flushed (see
`StandbySlotsHaveCaughtup` in `slot.c`, and `WalSndWaitForWal` in
`walsender.c`). So when the primary dies and one of those standbys is
promoted, nothing the consumer has received can be missing from the new
primary: the walsender didn't let it out before the standby had it. What can
happen is duplicates, because the position the consumer confirmed may be
behind what it processed. Never a hole.

{{< image src="fig-failover-slot.svg" title="A failover-safe logical slot, driven by pg_auto_failover's existing orchestration." >}}

That promise comes with conditions, and each one fails silently when you
miss it:

  - the slot must be created with `failover = true`, a plain slot gets no
    protection at all;
  - `synchronized_standby_slots` must list the standby that is going to be
    promoted, or the gate never applied to the new primary;
  - the standby needs `sync_replication_slots = on`, `hot_standby_feedback =
    on` and a `dbname` in its `primary_conninfo`;
  - a slot that stayed temporary on the standby, because it never caught up
    with the primary's position, is dropped on promotion and can't be
    created back in the past.

pg_auto_failover keeps those true for you. The monitor computes the list for
`synchronized_standby_slots` the same way it computes
`synchronous_standby_names`, as the physical slot of every failover candidate,
and `pg_autoctl` applies it on the primary and updates it when the candidates
change. The other settings are on by default for Postgres 17 and later. It
doesn't create the logical slot though, that's for the consumer to do, and
there are three things the consumer must do on its side:

  1. create its slot with `failover = true` (for a subscription that's
     `create subscription ... with (failover = true)`);
  2. store its own position durably, as the last `end_lsn` it fully applied,
     and never restart from zero;
  3. at every reconnection, run `IDENTIFY_SYSTEM` and refuse to stream if the
     server is behind the consumer's position, or if a timeline switch point
     is below it. A consumer that's ahead of the history it reads from is the
     one case the slot can't protect.

I tried it: a region is a monitor and three nodes on Postgres 17, and a
`pg_recvlogical` consumer follows a failover slot while a writer inserts
about 25 rows a second, one per transaction. Each scenario ran three times.
"Holes" are rows the new primary has that the consumer never received, and
"ahead" are rows the consumer received that the new primary doesn't have.

| Scenario | Duplicates | Holes | Ahead | Writer blocked |
|---|---|---|---|---|
| planned `pg_autoctl perform failover` | 0, 0, 0 | 0, 0, 0 | 0, 0, 0 | 1.4 to 2.2 s |
| primary killed | 3, 4, 2 | 0, 0, 0 | 0, 0, 0 | 51 to 52 s |
| standbys cut off, then primary killed | 0, 0, 0 | 0, 0, 0 | 0, 0, 0 | 51 to 53 s |
| same, with `synchronized_standby_slots` emptied | 0, 0, 0 | 4, 13, 3 | 92, 69, 80 | 51 to 53 s |

The slot does what it says. The consumer is never ahead when the setting is
right, even when the standbys were behind when the primary died (it just
receives nothing from then on, because the walsender waits for them). When we
empty the setting, the consumer ends up with 69 to 92 rows the new primary
never had, and the new primary then writes at WAL positions the consumer
already passed, so it never sees 3 to 13 rows. That's exactly the case the
`IDENTIFY_SYSTEM` check is for, and nothing in Postgres raises it for you.

The slot isn't what costs time: a killed primary blocks the writer for about
51 seconds, roughly ten for the monitor to declare the node unhealthy and the
rest for pg_auto_failover's drain timeout. Those are defaults you can tune,
measured here on a local Docker network.

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
features. New York publishes two views of its table, one for Amsterdam and
one for San Francisco, and San Francisco only publishes its own rows back:

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
were themselves applied from somewhere else (`pgoutput_origin_filter` in
`pgoutput.c`). It's all or nothing, though: on San Francisco's subscription it
would drop Amsterdam's mirror of Mumbai and Sydney together with the loop,
because those rows arrive carrying an origin too once New York has applied
them. A row filter looks at a value every row has, whatever path it took.
`origin = none` remains the tool when you don't have such a column.

I ran this topology on Postgres 17, with two cascades, one warehouse and 2,000
concurrent writes in each region. Without the filters the writes echo around
the loop. With them, every node ends up with 8,000 rows, exactly 2,000 per
region, and no apply or sync error on any subscription.

Cascading itself is nothing new, a subscriber re-publishing what it received
has worked since Postgres 10. Postgres 16 added a way to turn it off.

What cascading doesn't change is what logical replication never carried: DDL,
and the current value of sequences. For sequences, Postgres 19 now has
`create publication ... for all sequences` and `alter subscription ... refresh
sequences`. I ran it on `postgres:19rc1`: after 1,000 rows with ids drawn from
a sequence on the publisher, the subscriber has the rows and its own sequence
still at 1, because rows stream and sequences don't. After `refresh sequences`
it's at 1,000, and the next `nextval` after a promotion would be 1,001. That's
good for a cutover. It's not a way for two writers to share a sequence, which
is why we use the `(region, id)` key.

## When a region fails over, and when it's far away

The previous drill had one consumer. Here we have two regions subscribed to
each other, so what does a failover in one do to the replication with the
other, in both directions at once?

{{< image src="fig-region-failover.svg" title="One region fails over, with logical replication to a second region in both directions." >}}

I ran `us-east` and `us-west`, each a group with a monitor and three nodes,
linked both ways with the publications and subscriptions above and a writer
on each side. I killed the primary of one region under load, three times for
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

Moving a standby to another continent changes nothing in the guarantee, which
is about flushed positions, not distance. It changes what a synchronous commit
waits for. With a primary and a standby and a round trip time added with `tc
netem`, one client doing single-row inserts gets 29,661 commits a second with
`synchronous_commit = off` at 20 ms, and 45 with `remote_apply`; at 150 ms it's
27,282 and 6. Adding clients buys throughput back (about 1,450 tps with 32
clients at 20 ms) but never latency. `off` and `local` don't care about
distance, and they're also the two settings that can lose the last commits.
The links between our regions are asynchronous on purpose: a write in Sydney
is acknowledged without waiting for Amsterdam, and distance shows up as lag.

## Schema changes in five regions

A table changes on one primary, and the others must follow, at the right
place in the stream. Inside a group the standbys get it from physical
replication, so there's nothing to do. Between groups, here is what each tool
does.

### Core: the schema is yours

Postgres doesn't replicate DDL. The apply worker checks every column of an
incoming row against the local table, and fails when one is missing (an extra
local column is fine):

~~~
ERROR:  logical replication target relation "public.events" is missing replicated column: "note_a"
~~~

So a column added on the publisher first stops the subscriber at the next
row, and the worker retries until the column exists. I ran `add column` three
ways with a writer in each region, three runs each, and lost nothing:

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

### pgcopydb: DDL in the stream

pgcopydb can put the schema change in the same commit-ordered stream as the
rows, so it can't arrive before or after them. An event trigger on the source
emits each command with `pg_logical_emit_message` (Postgres 9.6, and
`pgoutput` sends it since 14), `pgcopydb follow` decodes the message, and the
apply process runs it on the target at the place it had in the stream.
`pgcopydb stream event-trigger` installs the trigger, which looks like this:

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

The JSON envelope has a `type` so that other kinds of messages can come later,
and `ddl` is the only one for now. Because it's a plain event trigger, a
multi-statement command travels as a single message, and the command runs on
the target with the role and `search_path` of the apply process, not
necessarily those of the original session.

I replaced the native subscription from `us-east` to `us-west` with `pgcopydb
clone --follow` (a one-way link), with a writer inserting about 29 rows a
second, and ran `add column`, `rename column` and `drop column` on `us-east`
without touching `us-west`. Three runs: it keeps up, 27 to 30 rows a second
in and the same out, and each change reached `us-west` within a second, with
no lag at any point. The rename is the change that no ordering makes safe on
the native link.

It doesn't replace the native link in our system though, for three reasons.
First, its slot isn't a failover slot by default. Postgres 17 accepts
`FAILOVER` on `CREATE_REPLICATION_SLOT` only in the parenthesized option
syntax, and the form older pgcopydb versions send can't carry it. On `main`,
`--enable-failover` sends it, and the same change makes `follow` restart its
sub-processes when one fails, a bit like an Erlang supervisor, because a
failover of the target is as ordinary as one of the source. With the flag the
slot is `failover=true` on the primary and `synced=true` on both standbys, and
after killing the primary `us-west` ends with every row, nothing left to catch
up one second after the writer stops (three runs). Without the flag the same
kill leaves `us-west` 1,306 to 1,450 rows behind: the restarts find no slot,
and after five attempts in 300 seconds `follow` gives up. Killing the primary
of the target region costs one restart of the apply process and no row (two
runs).

Second, it doesn't take a multi-host URI (`postgres://a,b,c/...` fails with
`could not match 3 port numbers to 5 hosts`), the keyword form with
`target_session_attrs=read-write` works. Third, it filters by table and not by
row, so it can carry the one-way links, like the warehouse mirror, but not the
two-way pairs.

### pglogical: replicate_ddl_command

pglogical doesn't replicate DDL either, its README says so, and gives us
`pglogical.replicate_ddl_command()` which runs the statement locally and queues
it in the stream. With two nodes subscribed to each other and both inserting,
a plain `alter table ... add column` on one of them stops the other's apply
worker at the first row, and it's only back 187 seconds after the first
`alter`, because its restart delay grows with each failure. Nothing is lost.
With `replicate_ddl_command` the other node has the column with nothing run
there, and no lag, rename included. The README warns that this isn't enough
once the subscriber writes with the old structure, which is the case of two
writers on one table, and my test didn't hit it.

Its slot has `failover = false`, because it uses the 2ndQuadrant syntax of
`CREATE_REPLICATION_SLOT`. You can flip it after the fact with Postgres 17's
`ALTER_REPLICATION_SLOT` on a replication connection while the slot is
inactive: disable the subscription, alter the slot, enable it. With that, I
killed a provider that had a physical standby, under a writer, and the
subscriber got the 200 rows written on the new provider one second after the
last insert. Without it, there's no slot on the standby and the 200 rows never
arrive.

### What the three add up to

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

## Core, pglogical and pgcopydb side by side

The tools differ in more than DDL: where the code runs, how fast they apply,
what they do with a first copy of a large database, and which Postgres
versions they connect.

{{< image src="fig-deployment.svg" title="Where the replication code runs: inside the servers, or between them." >}}

Core logical replication is Postgres itself. The walsender decodes the WAL
with `pgoutput`, and the subscriber's apply worker, a background process of the
server, applies each change as it arrives in one local transaction per source
transaction. Nothing is stored in between. pglogical has the same shape as an
extension that must be installed on both servers, with its own output plugin
and apply workers, which write to the heap directly, batch consecutive inserts
and commit asynchronously by default.

pgcopydb is another kind of tool, and logical replication isn't what it is
mostly for. It's for migrating to a new system, with new hardware, a new
architecture or a new major version: think `pg_dump -jN | pg_restore -jN`
between two running servers, with both parallelisms. It takes a snapshot,
copies the tables and builds the indexes in parallel, and to keep the cutover
short it follows the source from a slot created at the same snapshot, which it
reads while the copy runs. It's a separate program that runs on any host that
can reach both servers, with nothing installed on either. It writes every
change to a SQLite file, transforms them into SQL statements in a second one,
and replays those on the target through ordinary connections, which is what
lets it work with any server at any version. The price is that every change is
written twice and read twice.

### Order of magnitude

I ran the same table between two Postgres 18 servers on one machine, with a
trigger on the target that stamps the time a row is applied. Four workloads:
1,000 single-row transactions 50 ms apart, eight writers for 30 seconds, one
transaction of a million rows, and one `update` of a million rows.

| | Core | pglogical | pgcopydb |
|---|---|---|---|
| Latency of a single-row transaction, p50 / p99 | 0.3 / 1.7 ms | 0.2 / 2.0 ms | 0.7 / 7.8 ms |
| Rows a second under 8 writers | 80,000 (source commits 85,000) | 84,000 (85,000) | 8,900 loaded, 16,000 with the catch-up (source commits 67,000) |
| Lag once the 30 s load stops | 2 s | 0.2 s | 95 s |
| One transaction of 1M rows, after its commit | 4 to 6 s | 3 to 6 s | 5 to 6 s |
| One `update` of 1M rows that changes nothing (`REPLICA IDENTITY FULL`) | 3 to 5 s | 4 s | 6 to 8 s |

Core and pglogical keep up with the source, pgcopydb doesn't: it applies 9,000
to 16,000 rows a second and the rest queues, so the 95 seconds after a 30
second load are a backlog, not a stall. On a large transaction the gap closes,
because all three wait for the same thing, the source decoding at commit, and
pgcopydb transforms and applies the transaction while it's still receiving it.
The `update` changes no value, which pgcopydb notices when it has the whole old
row and sends nothing for; it still has to read a million messages, hence the
6 to 8 seconds. pglogical took 66 seconds for it in one run, after the
throughput test left 2.5 million rows in the table, and I haven't looked into
why.

Take these as orders of magnitude, from one machine and a handful of runs.
For a migration, 10,000 rows a second is more than most databases write. For a
replica behind a source that takes tens of thousands of writes a second, it's
not enough. Memory is the other number: pgcopydb works on a transaction in
passes while it receives it, so its apply process peaked at about 70 MB for
the million-row transaction and 230 MB for the million-row `update` with full
old rows. Core and pglogical apply a change as it arrives, and a million rows
cost them no more than one.

### What each supports

| | Core | pglogical | pgcopydb |
|---|---|---|---|
| Installed on | nothing | an extension on both servers | nothing on the servers, one program anywhere |
| Postgres versions | 10 and later, source and target may differ | 9.4 and later, one build per major version | source 9.6 and later (`wal2json` or `test_decoding` before 10), target anything `pg_restore` loads |
| First copy | one `COPY` per table, `max_sync_workers_per_subscription` at a time (2 by default) | `pg_dump`, then one `COPY` per table | `--table-jobs` tables at once, a big table split over several `COPY` streams, indexes built in parallel |
| Slow, unreliable link | a broken connection restarts the table | the same | many streams to fill the link, `--resume` skips finished tables |
| WAL kept on the source during the copy | from each table's sync slot until the table is copied and caught up | from the sync slot, for the whole copy | what receive hasn't spooled yet |
| Sequences | copied on demand from 19 | `replication_set_add_sequence` | copied at the end of the clone |
| Both directions, row filters | yes | yes | no |
| Conflicts | detected and counted (18) | five policies | none |

A single TCP stream rarely fills a long, slow link, and a copy that breaks
after nine hours of a ten-hour table makes for a bad evening. Core and
pglogical copy a table over one stream and start it again when the connection
drops. pgcopydb opens as many as you allow, can split one table over several
of them, and remembers which tables are done.

### The first copy and the source's WAL

A logical slot keeps the WAL since its position until its consumer confirms
it, and a first copy of a huge table is long, so what each tool does with that
position decides how much WAL a busy source has to keep.

Core gives each table its own synchronization worker. The worker creates a
permanent slot named `pg_<subscription>_sync_<table>_<system id>` inside a
`repeatable read` transaction on the publisher, copies the table, catches up
from its slot until it reaches the main apply worker, and drops the slot. The
source keeps the WAL from the creation of that slot until the table is copied
and caught up, and the snapshot holds back vacuum for as long as the `COPY`
runs. The subscription's own slot keeps being consumed meanwhile, so the bound
is one table's copy, which for a table of several terabytes is the whole copy.
pglogical takes one slot and one exported snapshot for the whole subscription:
structure, data, constraints, and only then the catch-up. The source keeps the
WAL and the snapshot for all of it.

pgcopydb creates its slot at the snapshot, copies with several workers, and
receives the changes into SQLite at the same time. The position it reports to
the source as flushed is what receive has committed to its SQLite file, so the
source recycles the WAL while the clone runs and the changes wait in files on
the pgcopydb host. The last release, 0.18, reports the position that apply
committed on the target instead, and apply only starts when the clone is done,
so with 0.18 the source keeps all of it.

To see it, I copied a table of 600,000 rows (261 MB with three expression
indexes) while a writer produced 3 to 4 MB of WAL a second on the source, and
sampled how far behind the current WAL position each slot's `restart_lsn` was,
every two seconds. The sizes are small, the arithmetic is not: at 3 MB a
second, a ten-hour copy keeps about 100 GB.

| | Core | pgcopydb (`main`) | pgcopydb 0.18 |
|---|---|---|---|
| During the copy | the table's sync slot: 52 MB after 2 s, 133 MB after 6 s | the slot: 127 MB after 2 s, 151 MB after 6 s | the slot: 122 MB after 2 s, rising |
| After the copy | the sync slot is gone, the main slot between 0 and 122 MB | between 46 and 202 MB, in steps | up to 208 MB for 24 s while apply replays, then 80 MB |

With the writer in another database of the same server, one that nothing
decodes, core's slot still advances (0 to 70 MB, in steps of about fifteen
seconds), pgcopydb `main` moves in steps too (57 to 186 MB), and pgcopydb 0.18
grew without end: 122 MB at the start, 647 MB after two minutes, and still
growing. It reported apply's position, and a quiet database on a busy server
commits nothing. In a real migration the sizes depend on the write rate of the
whole server, so `max_slot_wal_keep_size` and a look at
`pg_replication_slots.wal_status` belong in the plan for any long copy with
any of the three: a slot past the limit is invalidated, a core table
synchronization starts that table over, and a pgcopydb clone starts again.

### When the servers can't share a binary format

For pgcopydb speed isn't the question it was made for. A physical copy of a
cluster, from a base backup or a streaming standby, moves pages and WAL as
they are, so the two servers must agree on everything that shapes those
bytes. At startup the server compares its own settings with those recorded in
`pg_control` and refuses to run on a mismatch (`ReadControlFile()` in
`xlog.c`), and `pg_upgrade` does the same comparison between the old and the
new cluster (`check_control_data()`). That's the CPU architecture (word size, alignment, byte order), the block
size (8 kB unless built with `--with-blocksize`, from 1 to 32 kB), the WAL
block size, the relation and WAL segment sizes, `NAMEDATALEN`,
`INDEX_MAX_KEYS`, whether data checksums are on, and the catalog version,
which is why a physical copy also needs the same major version. Outside
`pg_control`, the C library of the operating system decides how text sorts, and
a physical copy carries indexes that are only right where the order is the
same.

| | Same major version | Architecture and build settings | Online |
|---|---|---|---|
| Physical standby, base backup | required | must match | yes |
| `pg_upgrade` | to a new major version | must match | no |
| Logical replication | no | free | yes, first copy and DDL are yours |
| `pg_dump` and `pg_restore` | no | free | no |
| pgcopydb | no | free | yes, after a parallel dump and restore |

pgcopydb lives in the last two rows, and its main use case is the second to
last: a dump and a restore made fast, with the catch-up of logical replication
at the end so that the cutover is short. It's for the migrations where the
first two rows are closed: from a 32 kB block size or an odd segment size to a
default build, which is what a hosted service runs; from one architecture to
another; to a server with checksums when the old one had none; or from a
database where nobody can install an extension. Logical replication ignores
the layout too, and leaves you the first copy, the schema and the sequences.
That's what pgcopydb packages, and it's what adding a region to our system
needs: a first copy of an already large database, over a long link, without
waiting for a maintenance window.

## When two writers do touch the same row

Not every schema can be split by owner. A reference table that every region
edits is the usual counterexample, and then conflicts are real. Core detects
and counts them (Postgres 18 logs each conflict and counts it by type, and 19
adds `retain_dead_tuples` so that an update that raced a delete can be told
from a missing row), and it can skip one with `alter subscription ... skip`.
It doesn't resolve them. pglogical has five policies for that, so I ran two
Postgres 18 nodes subscribed to each other, with four clients each updating
the same 1,000 keys at 2,000 transactions a second per node for 15 seconds,
three runs per policy:

| Policy | Rows that differ afterwards, of 1,000 | Replication |
|---|---|---|
| `last_update_wins` | 0, 0, 0 | healthy |
| `apply_remote` | 1, 0, 1 | healthy |
| `keep_local` | 1000, 1000, 1000 | healthy, no error |
| `first_update_wins` | 1000, 1000, 997 | healthy, no error |
| `error` | 1000, 1000, 1000 | both subscriptions `down` |

`last_update_wins` converges every time, though it keeps one row and not both:
of about 60,000 increments, 8, 12 and 22 were lost to a concurrent update of
the same row. `keep_local` and `first_update_wins` do what they say, each node
keeps its own value, and nothing in the logs tells you the nodes now
disagree. `error` stops replicating until someone comes.

The other half of multi-writer is sequences, and the *Sequence Access
Methods* patch set ([commitfest #4677](https://commitfest.postgresql.org/52/4677/))
was returned with feedback for Postgres 19, so it's not in. `galloc` and
`snowflakeid` remain features of EDB Postgres Distributed, the commercial
successor of BDR.

## What BDR did, and what's left

BDR 1.0, a patched 9.4 and a plugin, has its last commit on the 1.0 branch in
November 2019. Its successors aren't open source, so what follows is a
comparison of designs read from [its
documentation](https://github.com/2ndQuadrant/bdr/tree/bdr-plugin/REL1_0_STABLE/doc),
and I didn't run any of it.

| | BDR 1.0 | What we built |
|---|---|---|
| Topology | a full mesh, five nodes are 20 directed links | five groups, eight subscriptions, hubs and cascades chosen by hand |
| Conflicts | optimistic: the latest tuple by the origin's timestamp, or a user handler; some cases are "divergent" and need an operator | avoided with disjoint `region` keys |
| DDL | replicated, under a global lock: every node reachable and caught up, writes pause or fail meanwhile, a down node must be removed first | no lock and nothing automatic: a short pause, or the change in the stream |
| Sequences | global sequences allocated by a vote of the majority, deprecated in the manual in favor of step and offset | none: the key is `(region, id)` |
| A node is down | one slot per peer holds WAL on all the others until the node is parted | a node is a group of three, the slot follows through `failover = true` |

BDR 1.0 bought "every change reaches every node", rows, schema and keys, with
a coordination cost we refuse to pay here: no global lock, no majority vote for
a sequence, no pause of writes on a continent because one node is slow. And
its manual says nothing about what happens to a peer's slot when a node's own
server fails over, because on 9.4 the primitive didn't exist. The slot
synchronization of Postgres 17 is the piece BDR 1.x lacked.

What's left is concrete:

  - **DDL in the stream.** Core carries none of it. pgcopydb and pglogical each
    have part of the answer outside core, and a schema change that travels in
    the same commit order as the rows is what no core feature provides.
  - **Sequences.** Postgres 19 copies the values on demand, which is what a
    cutover needs. Nothing streams them as they advance, or allocates them
    across writers.
  - **Conflict resolution.** Postgres 18 counts conflicts and doesn't resolve
    them. Avoiding them by construction is still the only built-in answer.
  - **Joining and leaving.** BDR had `join` and `part`, and today adding a
    region or retiring one is a procedure you write by hand.
  - **The consumer as a node.** A logical consumer isn't in the state machine
    of anything that orchestrates the primary it reads from, so who creates the
    slot, checks its position after a promotion and raises the alarm when it's
    ahead is still a script. In pg_auto_failover, every routine that looks at
    the topology iterates the node table and would count a consumer as a
    standby. That's work for its [roadmap](https://oss.theartofpostgresql.com/roadmap/),
    which is [funded by the teams that run
    it](https://oss.theartofpostgresql.com/pg_auto_failover/).

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
Run them, break them, and tell me what I got wrong.

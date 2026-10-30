+++
title     = "Postgres logical replication beyond one writer"
date      = "2026-10-27T09:00:00+0100"
tags      = ["PostgreSQL", "Replication", "Logical Decoding", "Architecture"]
categories = ["PostgreSQL", "Architecture"]
icon      = "🐘"
+++

In 2014, if you wanted Postgres servers on several continents, each of them
accepting writes and all of them sharing their rows, there was one serious
open source answer: BDR, a patched PostgreSQL 9.4 and a plugin, from
2ndQuadrant. It needed a server that was not Postgres. Ten years of work on
logical replication in Postgres itself have since moved almost everything it
needed into the stock server. This post builds that system with what is
released today: Postgres 19 release candidate 1 for the newest features, two
open source tools around it, and a lab to measure each claim.

It is the last part of a series about logical replication, and it stands on
its own:
[Part 1](/blog/2026/09/ten-years-of-postgres-logical-replication/) builds a
hub-and-workers system for write scalability,
[Part 2](/blog/2026/10/consolidating-databases-with-postgres-logical-replication/)
consolidates several databases into one warehouse, and
[Part 3](/blog/2026/10/zero-downtime-postgres-upgrades-with-logical-replication/)
covers a zero-downtime major upgrade.

<!--more-->

{{< lab >}}
The demos for this post are in the `compose/` directory next to it. They
build pg_auto_failover and pgcopydb from source, because the features used
here are on their `main` branches and not in a tagged release yet. Failover,
cascading and schema demos run on Postgres 17, the comparison of the three
tools and the conflict policies on Postgres 18, and the sequence demo on
Postgres 19 release candidate 1 (`postgres:19rc1`). The other statements
about Postgres 19 were checked against the `REL_19_RC1` source tag and its
release notes. Statements marked **verified** were checked against the source of the
project they are about, or against a run of the demos, and every number
comes from a run.
{{< /lab >}}

<!--toc-->

---

## The system

{{< image src="fig-region-map.svg" title="One writer per region: conflicts avoided by construction." >}}

Four regions own data: us-east in New York, us-west in San Francisco,
ap-south in Mumbai and ap-southeast in Sydney. A fifth site, a warehouse in
Amsterdam, owns nothing and holds a copy of everything. Each site is a
group of three Postgres servers, a primary and two standbys, managed by
pg_auto_failover. The groups share no failover. They are linked by logical
replication only, in both directions: a region's own rows flow one way, and a
read-only mirror of everyone else's rows flows back. New York and Mumbai
link straight to Amsterdam, San Francisco and Sydney cascade through them
rather than each opening a long-haul connection of their own. Every site has
its application next to it, writing to its own region and reading the rest
from the mirror.

Five requirements make that a hard problem, and they are the outline of
this post, in the order they are met:

1. **A region keeps its change stream when its primary dies.** A consumer
   attached to a logical slot must not lose a row, or invent one, when a
   standby takes over.
2. **Rows belong to a region.** Two regions never write the same row, so
   there is no conflict to resolve.
3. **The links are bidirectional and some of them cascade**, without a row
   ever going back where it came from.
4. **The schema changes everywhere**, while the regions keep writing.
5. **A region can be added**, with a first copy that does not need a
   maintenance window, and that works even when the old and the new servers
   cannot share a binary format.

The tools are few, and each has one job. Core Postgres replicates the rows.
pg_auto_failover runs the failover of each group and keeps the slot's
settings correct through it (its `main` branch: the last release, v2.2,
predates the pieces used here). pgcopydb does the first copy and the
migrations that logical replication does not do alone, and can carry the
schema changes in the stream. pglogical is the alternative that predates
core and still has what core lacks: conflict policies and DDL replication.

## What Postgres learned in ten years

BDR 1.0 shipped on a patched 9.4 because the pieces it needed were not
in Postgres yet. This table lists what the system above needs, which
release put it in the server, and what it replaced. The releases and the
descriptions come from the release notes in each major version's source tree
(`REL9_4_0` to `REL_19_RC1`).

| What the system needs | In Postgres since | What it replaced |
|---|---|---|
| Decode the WAL into row changes, per table, with `REPLICA IDENTITY` choosing the key | 9.4 (2014): logical decoding, `pg_recvlogical`, `test_decoding` | Triggers and queue tables (Londiste, Slony) |
| Know which node a change came from, and resume a stream by position | 9.5: replication origins and progress tracking | Out-of-tree patches |
| Put a message in the stream that is not a row | 9.6: generic WAL messages (`pg_logical_emit_message`); 14: `pgoutput` can send them | Writing the message in a table |
| Say what flows, from SQL | 10: `CREATE PUBLICATION`, `CREATE SUBSCRIPTION`, with the initial table copy | An extension on each side (pglogical, BDR) |
| Replicate `TRUNCATE`; replicate into partitioned tables | 11; 13 | Nothing |
| Not wait for a huge transaction to commit before sending it | 14: streaming of in-progress transactions; 16: apply them in parallel; 18: `streaming = parallel` is the default | Wait for the commit |
| Say which rows and which columns flow | 15: row filters (`WHERE`), column lists, `FOR TABLES IN SCHEMA` | Replication sets, in an extension |
| Survive a bad row | 15: `ALTER SUBSCRIPTION ... SKIP`, `disable_on_error`, `pg_stat_subscription_stats` | Editing the catalogs |
| Two nodes that replicate each other without a loop | 16: `origin = none`; subscriptions run as the table owner | A convention in the schema |
| Decode on a standby | 16 | Only the primary could be read |
| Keep the slot when the primary is lost | 17: failover slots, `sync_replication_slots`, `synchronized_standby_slots` | A script, and the risk of a gap |
| Build a logical replica from a physical one; keep slots across `pg_upgrade` | 17: `pg_createsubscriber`, `pg_upgrade` keeps the slots | A full initial copy |
| See conflicts | 18: conflicts logged and counted per type; 19: `retain_dead_tuples` makes `update_deleted` conflicts detectable | Reading the apply error |
| Move the sequences | 19: `CREATE PUBLICATION ... FOR ALL SEQUENCES`, `ALTER SUBSCRIPTION ... REFRESH SEQUENCES` | A script that reads `last_value` |

Read the first column as the requirements of any multi-region system and the
second as the release that met each one. What BDR could not find in 9.4 is now
a SQL statement, or a setting. Three things are not on the list because
Postgres still does not do them: replicating DDL, resolving a conflict, and
handing out unique sequence values to several writers. They come back at the
end, with what the tools fill in meanwhile.

---

## 1. A region that keeps its change stream when its primary dies

Every subscription in this system points at a primary. When that primary
fails unplanned, mid-stream, a consumer is attached to it.

### What Postgres guarantees

**Verified**, against the `REL_19_RC1` tag, not inferred from the feature's
name. Postgres 17 added `failover = true` on a replication slot and the
`synchronized_standby_slots` setting. Together they change one thing,
exactly: a logical walsender on a failover slot will not send a client any
WAL past what every standby named in `synchronized_standby_slots` has already
flushed (`replication/slot.c:StandbySlotsHaveCaughtup`,
`replication/walsender.c:WalSndWaitForWal`). The consequence is the one that
matters for a change-data-capture consumer: if the primary dies and one of
those standbys is promoted, nothing the consumer already received can be
absent from the new primary's history, because the gate never let it out
until the standby had it too. What is left on the table is duplicates, not
holes: the consumer's `confirmed_flush` on the promoted standby's slot is
always at or behind what it last acknowledged, never ahead.

That guarantee has conditions, and each one is a way to lose it silently:

- The slot has to be `failover = true` from the start. A plain slot gets no
  protection at all.
- `synchronized_standby_slots` has to name the standby that ends up
  promoted. With two standbys and only one listed, the other one promoted,
  the gate never applied to the node that is now primary.
- The standby needs `sync_replication_slots = on`, `hot_standby_feedback =
  on`, and `dbname` in its `primary_conninfo`; slot state does not follow a
  plain streaming standby that is missing any of the three.
- A slot that stayed `temporary` on the standby, because it never caught up
  to the remote's `confirmed_flush` or never got a consistent snapshot, is
  dropped when that standby is promoted, and cannot be recreated in the
  past.

pg_auto_failover keeps those conditions true. A monitor function,
`pgautofailover.synchronized_standby_slots(formation, group)`, is computed the
way `synchronous_standby_names()` is, as every current failover candidate's
physical slot name, and `pg_autoctl` applies it on the primary through the
same path as the other settings, again whenever the set of candidates changes.
`sync_replication_slots` is on by default for Postgres 17 and later nodes,
next to `hot_standby_feedback`, and `wal_level` is `logical` on every node it
creates. A node that rejoins as a standby after an interrupted demotion drops
its stale failover slots instead of letting them block the slot-sync worker.
What it deliberately does not do is create the logical slot: that stays the
consumer's job, as a table's schema always did.

{{< image src="fig-failover-slot.svg" title="A failover-safe logical slot, driven by pg_auto_failover's existing orchestration." >}}

Physical replication keeps the standby current, including the slot's own state, once synchronized_standby_slots names it. After promotion the consumer reconnects to the same slot, now on the new primary, and resumes at its last confirmed LSN: duplicates are possible, a hole is not.

### What the consumer does, and what it costs

Only three things:

1. Create the slot with `failover = true` on the primary (for a core
   subscription that is `CREATE SUBSCRIPTION ... WITH (failover = true)`).
2. Persist its own position durably, keyed by the last `end_lsn` it has
   fully applied. Never resume from 0, never trust an in-memory position
   across a restart.
3. On every reconnect, `IDENTIFY_SYSTEM` before trusting the server: if its
   reported position is behind what the consumer already has, or a
   `TIMELINE_HISTORY` switchpoint sits below the consumer's stored LSN, stop
   and raise an alarm instead of streaming. A consumer that is ahead of the
   history it is about to read from is the one case the slot's own guarantee
   cannot protect.

**Verified**, by running it: a region is a pg_auto_failover group, a monitor and three
nodes (`us-east-a`, `us-east-b`, `us-east-c`) on Postgres 17, built from
pg_auto_failover `main` and left at its default timings. A `pg_recvlogical`
consumer (`test_decoding`, one-second feedback interval) follows a
`failover = true` slot while a writer inserts about 25 rows a second, one
row per transaction, with ids it chooses itself so a retry is recognisable.
Each scenario ran three times. "Holes" are rows the final primary has that the
consumer never received, "ahead" are rows the consumer received that the
final primary does not have.

| Scenario | Duplicates | Holes | Ahead | Writer blocked |
|---|---|---|---|---|
| `pg_autoctl perform failover` | 0, 0, 0 | 0, 0, 0 | 0, 0, 0 | 1.4 to 2.2 s |
| primary killed | 3, 4, 2 | 0, 0, 0 | 0, 0, 0 | 51 to 52 s |
| both standbys cut off, then primary killed, gate on | 0, 0, 0 | 0, 0, 0 | 0, 0, 0 | 51 to 53 s |
| same, `synchronized_standby_slots` emptied | 0, 0, 0 | 4, 13, 3 | 92, 69, 80 | 51 to 53 s |

- **The guarantee holds as described.** With the gate on, a planned failover
  produces no duplicate at all and a killed primary produces a handful,
  bounded by the consumer's feedback interval. Neither produces a hole. The
  consumer resumes by itself through a multi-host connection string, up to
  five seconds after the new primary accepts writes, which is
  `pg_recvlogical`'s own reconnect delay.
- **The gate does its job when the standbys are behind.** In the third
  scenario both standbys stop receiving WAL five seconds before the kill and
  the writer commits with `synchronous_commit = local`. The consumer
  receives nothing from that moment on, because the walsender will not send
  past what the standbys have flushed. Between 67 and 79 rows the writer had
  been told were committed are lost with the old primary, which is the cost
  of `synchronous_commit = local` and not the consumer's to carry, but none
  of them had been delivered downstream.
- **Misconfigured, the prediction comes true, and then some.** With
  `synchronized_standby_slots` empty, the same sequence delivers 69 to 92 rows
  to the consumer that the new primary never had. The new primary then writes
  new rows at WAL positions the consumer's confirmed position is already
  past, so the slot skips them: 3 to 13 rows written after the failover never
  reach the consumer at all. The consumer is ahead of the history it reads
  from, which is the case the `IDENTIFY_SYSTEM` check exists to catch, and
  nothing in Postgres raises it for you.
- **What dominates the downtime is not the slot.** With three nodes a planned
  failover blocks the writer for a second or two. A killed primary blocks it
  for about 51: roughly ten seconds for the monitor to mark the node
  unhealthy, and the rest in pg_auto_failover's drain timeout before the new
  primary is allowed to take writes. Those are defaults, they can be tuned,
  and they are measured here on a local Docker network, not a WAN.

The operator's view is the monitor's. Every node carries a free-form
`region` label, set with `pg_autoctl set node region`; no failover or quorum
decision reads it, but `pg_autoctl watch` shows it as a column. Here is a
frame of `pg_autoctl watch --group 0` taken 25 seconds after the primary was
killed, with the monitor part-way through the election: `us-east-a` is still
`primary` but assigned `demote_timeout`, `us-east-c` has been picked and is in
`stop_replication`, and `us-east-b` is following it (`join_secondary`):

```
Formation: default - Sync Standbys: 1                                       23:27:13

     Name  Node       Region Quorum Priority      Host:Port       TLI: LSN Last Check   Connection Last Report      Reported State      Assigned State
us-east-a     1      us-east    yes       50 us-east-a:5432   1: 0/5042190         2s read-write !         28s             primary      demote_timeout
us-east-c     2      us-east    yes       50 us-east-c:5432   2: 0/50424E8         1s    read-only          1s    stop_replication    stop_replication
us-east-b     3      us-east    yes       50 us-east-b:5432   1: 0/5042310         1s  read-only !          1s      join_secondary      join_secondary
```

`pg_autoctl show synchronized-standby-slots` prints both standbys' slots,
`'pgautofailover_standby_2,pgautofailover_standby_3'`, on a healthy group,
and one, `'pgautofailover_standby_3'`, once the primary has been killed and a
standby promoted: the gate shrinks to the standby that is left, and keeps
protecting the consumer, which a two-node group could not do.

---

## 2. Rows that belong to a region

The five sites are connected by replication, not by consensus, so the
architecture avoids conflicts instead of resolving them: the region is part
of every table's key.

```sql
create table events (
  region     text   not null,
  event_id   bigint not null,
  payload    text,
  written_at timestamptz not null default clock_timestamp(),
  primary key (region, event_id)
);
```

No row is ever a candidate for two regions to write, because the key says
which one owns it. The rule is enforced once, where the application picks the
value of `region`, and the replication layer needs no rule at all. A
continent is a reasonable unit of ownership, narrower if compliance needs it
narrower. It costs a wider key, and an `event_id` that each region mints for
itself: with a `(region, id)` key, a sequence on each node is enough, and no
global sequence is needed.

The groups are independent on purpose. They are connected by logical
replication, not meant as one failover unit, so each region has its own
monitor, and a failover in one never involves another.

---

## 3. Linking the regions

### One publication per link, filtered on the key

What moves on every link is the same shape. A region's own rows go one way, a
read-only mirror of everyone else's comes back the other way. Row filters
(Postgres 15) say which. New York publishes two different views of the same
table, one toward Amsterdam, one toward San Francisco, and San Francisco
publishes only its own rows back:

```sql
-- us-east
create publication pub_to_eu_west for table events
  where (region = 'us-east' or region = 'us-west');
create publication pub_to_us_west for table events
  where (region <> 'us-west');

-- us-west
create publication pub_to_us_east for table events
  where (region = 'us-west');

-- us-west, the subscriber side of the first link
create subscription sub_us_west_from_us_east
  connection 'host=us-east-a,us-east-b,us-east-c port=5432 dbname=app user=postgres
              target_session_attrs=read-write'
  publication pub_to_us_west
  with (failover = true);
```

Two things in that subscription do the work of section 1. `failover = true`
makes the slot that the subscription creates on the publisher a failover slot,
so the standbys keep a copy. The connection string lists every node of the
publisher's group with `target_session_attrs=read-write`, so the subscription
finds whichever one is the primary after a failover. The subscription a
group itself runs from another region needs nothing: it lives in the
catalogs, which physical replication carries to the standbys, and a
subscription's apply worker starts on a node only while that node is a
primary.

{{< image src="fig-region-node.svg" title="What runs at each site." >}}

Each region is a pg_auto_failover group of its own, and the two regions of the figure are the same group mirrored. The primary of `us-east` publishes `pub_us_east`, runs the subscription `sub_us_east_from_us_west` that reads `us-west`, and holds the failover-enabled slot `sub_us_west_from_us_east` that `us-west` reads. Both standbys keep a synced copy of that slot, and have the subscription in their catalogs without a worker. `us-west` is the same, with the roles reversed.

### Why a row filter, and not `origin = none`

Cascading, a subscriber re-publishing what it received to a further
subscriber, has worked since logical replication shipped in Postgres 10
(`doc/src/sgml/logical-replication.sgml`: "Subscribers pull data from the
publications they subscribe to and may subsequently re-publish data to allow
cascading replication"). What Postgres 16 added is a way to stop it on
purpose: `CREATE SUBSCRIPTION ... WITH (origin = none)` makes the publisher
send only the rows that originated there, not ones it is itself forwarding
from upstream. **Verified**, `pgoutput_origin_filter` in
`src/backend/replication/pgoutput/pgoutput.c`: with `origin = none`, any row
whose WAL record carries a non-invalid origin id is filtered out before it
reaches that subscriber. The id comes from `replorigin_session_origin`, which
the apply worker sets before writing anything it received.

For a region-keyed schema, `origin` is not the tool that closes the loop.
`origin` is a binary, `none` or `any`, not a per-origin allow list. On San
Francisco's subscription, `none` would drop Amsterdam's `ap-south` and
`ap-southeast` mirror along with the loop, because both arrive at San
Francisco carrying some non-invalid origin once New York's apply worker has
touched them. There is no built-in way to ask for "everything except what I
sent you". The row-filtered publication does it: each filter looks only at
a value every row carries, whatever hop the row arrived on, so a row can never
reach a link that would send it back to where it already lives. `origin =
none` stays the right tool when there is no such column to cut on.

**Verified**, by running it: four primaries, two of them cascading through the
other two, one warehouse, every mirror filtered, on Postgres 17, with 2,000
concurrent writes into each region. Without the filters the writes echo
around the loop. With them, the run ends with 8,000 rows on every node,
exactly 2,000 per region, and zero `apply_error_count` or `sync_error_count`
on any subscription.

### Cascading, one hop

San Francisco and Sydney are full primaries, not read-only leaves. What makes
them different from New York and Mumbai is routing: instead of opening a
fifth and sixth long-haul connection into Amsterdam, their rows cascade one
hop through New York and Mumbai first, and the combined view cascades back
down the same link. One intercontinental connection per hub instead of one
per spoke. A real build-out covering APAC would reach for more regions, and
past a certain distance a spoke earns its own direct link, but the mechanism,
one hop and the same row-filter discipline each direction, does not change
with a third or a fifth. Draw it once, repeat it per city.

Cascading changes nothing about what is not replicated, at any hop: DDL does
not replicate (section 6), and neither do sequence values as they advance.
Postgres 19 adds `CREATE PUBLICATION ... FOR ALL SEQUENCES` and `ALTER
SUBSCRIPTION ... REFRESH SEQUENCES`, which copy the current values on demand
from a publisher running 19 or later. **Verified**, on `postgres:19rc1`: after
1,000 rows with ids drawn from a sequence on the publisher, the subscriber
has the 1,000 rows and its own sequence still at 1, because rows stream and
sequences do not. After `ALTER SUBSCRIPTION ... REFRESH SEQUENCES` the
subscriber's sequence is at 1,000, and the first `nextval` it would hand out
after a promotion is 1,001. That helps a cutover. It does not let two writers
draw from one sequence, which is what the `(region, id)` key avoids needing.

---

## 4. When a region fails over, seen from the other regions

The drill of section 1 leaves one question open: what a failover in one
region does to the logical replication it takes part in with another, in both
directions at once.

{{< image src="fig-region-failover.svg" title="One region fails over, with logical replication to a second region in both directions." >}}

The primary is killed; the monitor elects and promotes a standby; that node's own subscription resumes from its replication origin at once, and the other region's subscription reconnects to the new primary as soon as it accepts writes, and resumes from the synced slot.

**Verified**, by running it: two regions, `us-east` and `us-west`, each a group
of its own with its own monitor and three nodes, each owning the rows of its
own region key, linked in both directions by native logical replication, with
a writer on each side. The primary of one region is killed under load, and the
test waits until both sides hold the same rows. Three runs killing `us-east-a`
and three killing `us-west-a`, with the same result every time:

- **Nothing lost, nothing duplicated, in either direction.** In one of the
  runs with `us-east-a` killed, `us-east` acknowledged 1,532 rows and
  `us-west` 2,552, and afterwards both regions hold exactly those rows. The
  other five runs end the same way: every acknowledged row is on both sides,
  none is missing, none is twice. The only apply error is one counted on the
  subscription that lost its connection.
- **The two directions come back at different times, and which one is which
  follows the dead region.** The failed region's own subscription resumes 21
  to 23 seconds after the kill, at the position its replication origin
  recorded, as soon as a standby is promoted. The other region's subscription
  cannot connect until the new primary accepts writes, 52 to 54 seconds in,
  and its worker finds that by retrying every five seconds: 55 seconds
  without new rows from the failed region in all six runs, against 21 to 23
  the other way.
- **The slot did its job.** The standbys held a synced copy of the failover
  slot before the kill, and the other region's subscription resumed on the new
  primary from it, without a resync.

This was run with pg_auto_failover's default synchronous replication, where a
commit on the primary waits for a standby. The case the test does not cover is
a group whose standbys are behind when the primary dies, for the
subscription the group runs from the other region: there the other side's slot
can be ahead of what the promoted node has, and it is the reason to keep the
synchronous setting for such a group.

---

## 5. Distance

Move the standby to another region and nothing above changes: the gate is
about flushed LSNs, not distance. What does change is how long a
`synchronous_commit = remote_apply` write waits for a cross-region round
trip, against how much data an asynchronous standby can lose in exchange for
not waiting at all.

A primary and a physical standby (`synchronous_standby_names = 'ANY 1
(standby)'`), a round-trip time added between them with `tc netem`, and
`pgbench` running single-row insert transactions under each setting, with one
client and with 32. The delay is emulated on one machine, so there is no
jitter and no loss; what it shows is the arithmetic of a commit that waits
for a round trip.

| RTT | `off` | `local` | `remote_write` | `on` | `remote_apply` |
|---|---|---|---|---|---|
| 0 ms | 29,475 tps | 17,075 | 610 | 3,795 | 5,811 |
| 20 ms | 29,661 | 17,266 | 45 | 44 | 45 |
| 70 ms | 28,630 | 17,856 | 13 | 13 | 12 |
| 150 ms | 27,282 | 18,467 | 6 | 6 | 6 |

One client, transactions a second. At 20 ms and beyond, the three synchronous
settings are the same number, one commit per round trip, and the average
commit takes the round trip (22 ms, 73 ms, 152 ms). With 32 clients the
commits overlap, and the throughput rises to about 1,450 tps at 20 ms and 205
at 150 ms: more clients buy back throughput, never latency. `off` and `local`
do not move with the distance; they are also the two that can lose the last
commits when the primary dies. The 0 ms row is not a latency result: with no
delay the numbers measure the standby's apply speed, and `remote_write` is
slower than `on` there for no reason the setting explains.

The logical links between the regions are asynchronous on purpose, so none of
this applies to them: a write in Sydney is acknowledged without waiting for
Amsterdam. Distance shows up there as lag, which section 7 measures for
the three tools.

---

## 6. Schema changes across regions

A table changes on one primary, and the others have to change with it, at the
right point in the stream. Five groups linked by eight subscriptions, two
pairs of them in both directions, make it a coordination problem: whoever
changes a table changes it in every region, while the writers keep writing.
Inside a group there is nothing to do, because the standbys get the change
through physical replication. Between groups, here is what each tool does,
measured on `us-east` and `us-west` as two groups linked as in section 4.

### Native logical replication: the schema is yours

Postgres does not replicate DDL. The apply worker checks every column of an
incoming row against the local table and fails if one is missing; an extra
local column is fine. So a column added on the publisher first stops the
subscriber at the next row, with this error, and the worker retries until the
column exists:

```
ERROR:  logical replication target relation "public.events" is missing replicated column: "note_a"
```

`ADD COLUMN` three ways, with a writer in each region, three runs each, nothing
lost in any of them:

| Rollout | Apply errors | Effect |
|---|---|---|
| `us-east` first, `us-west` ten seconds later | 4 on `us-west`, every run | `us-west` is 90 to 178 rows behind after 10 s and caught up 15 to 16 s after the first `ALTER` |
| both primaries at the same moment (3 attempts per run) | 0 in 8 of 9, 2 on `us-east` in the other | none visible at this write rate, except for that one retry |
| writes paused, drained, both `ALTER`ed, writes resumed | 0 | writers blocked 0.9 to 1.5 s |

The second line is luck, not design: the window is a few milliseconds, and a
busier writer would find it. The first line is the honest default for an
operator who does one region and then the next. Adding a column wants the
subscriber changed first; dropping one wants the publisher changed first; and
on a link that runs in both directions each region is both, so no order is
safe, only a short pause is. That is also why a rename cannot be a rolling
change here: expand, copy, switch, contract, each step additive or removing
something nobody reads.

### pgcopydb: DDL carried in the stream

pgcopydb can put a schema change in the same commit-ordered stream as the
rows, so it cannot arrive before or after them. An event trigger on the
source emits each command as a logical message (`pg_logical_emit_message`,
the 9.6 primitive, which `pgoutput` sends since Postgres 14), `pgcopydb
follow` decodes it, and the apply process runs it on the target in the
position it had in the stream. `pgcopydb stream event-trigger` installs the
trigger:

```sql
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
```

{{< image src="fig-pgcopydb-message.svg" title="DDL as a message, not a queue table." >}}

An Event Trigger emits a JSON message, prefix pgcopydb, type ddl, body the command text. pgcopydb follow decodes the Message frame, filters on the prefix, parses the JSON, and dispatches on type: the ddl handler executes the body on the same connection the rest of the apply worker already uses, inside the same commit-ordered stream as every other change.

The envelope `{"type": "ddl", "body": "..."}` has `type` as an open registry:
`ddl` is the only one defined, and a second kind (a sequence synchronization,
say) would be a new `type` value and a new entry in the dispatch table, not a
second wire convention. Two edges come with the source side being a plain
event trigger: `current_query()` captures a multi-statement command whole, not
split, so it lands in the target's replay file as a single DDL message; and a
replayed command runs with the apply worker's own role and `search_path`, not
necessarily the one the original session had.

`pgcopydb clone --follow` replaced the native subscription from `us-east` to
`us-west` (a one-way link; `clone` also created the table on `us-west`
itself), with a writer inserting about 29 rows a second in single-row
transactions, and three changes ran on `us-east` with nothing run on
`us-west`. Three runs, on pgcopydb `main` (d02e591):

- It keeps up: 27 to 30 rows a second written, the same arriving, a largest
  lag of 0 rows.
- `ADD COLUMN`, `RENAME COLUMN` and `DROP COLUMN` each reached `us-west`
  within one second, with a largest lag of 0 rows over the next ten seconds,
  writer running. The rename is the change that no order makes safe on the
  native link. The row inserted with the new column lands with it.

Three things keep it from replacing the native link in this system:

- **Its slot is not a failover slot by default.** Without `--enable-failover`,
  pgcopydb issues `CREATE_REPLICATION_SLOT ... LOGICAL` without the `FAILOVER`
  option. After `us-east-a` is killed, the new primary has no such slot and
  the stream cannot be resumed (`replication slot "pgcopydb" does not
  exist`): the rows written afterwards never reach `us-west`, and re-creating
  the link needs a new slot and a new clone. Postgres 17 accepts `FAILOVER`
  on that same command, but only in the parenthesized option syntax
  (`CREATE_REPLICATION_SLOT "pgcopydb" LOGICAL "pgoutput" (FAILOVER true)`),
  which the bare form older pgcopydb versions sent cannot carry. pgcopydb
  `main` has an `--enable-failover` option that sends it on Postgres 17 and
  later, and makes `follow` restart its sub-processes when one fails, at most
  `--max-restarts` times in `--restart-window` seconds, Erlang supervisor
  style, because a failover of the target is as ordinary as one of the
  source. With both, the slot is `failover=true` on the primary and `synced=true`
  on both standbys before the kill; after the kill the new primary has it,
  and `us-west` ends with every row `us-east` acknowledged, with nothing left
  to catch up one second after the writer stops (three runs). Without the
  flag, the same kill leaves `us-west` 1,306 to 1,450 rows behind: the
  restarts find no slot, and after five attempts in 300 seconds `follow` logs
  `giving up` and exits. Killing the primary of the target region instead
  costs one restart of the apply process, which comes back by itself, and no
  row is missing (two runs). The standbys still need `sync_replication_slots`
  and the primary `synchronized_standby_slots`, the Postgres-side settings of
  section 1.
- **It does not take a multi-host URI.** A `postgres://a:5432,b:5432,...`
  string makes its replication connection fail with `could not match 3 port
  numbers to 5 hosts`; the keyword form `host=a,b,c port=5432 ...
  target_session_attrs=read-write` works and is what the demo uses.
- **It filters by table, not by row.** The `where (region = ...)`
  publications that keep the bidirectional links from echoing have no
  equivalent, so it can carry one-way links only, such as the warehouse
  mirror, not the pairs.

### pglogical: `replicate_ddl_command`

pglogical does not replicate DDL either; its README says so, and offers
`pglogical.replicate_ddl_command()`, which runs the statement locally and
queues it for the subscribers of a replication set (`ddl_sql` by default), in
the stream. Two PostgreSQL 18 nodes subscribed to each other, both inserting,
twice:

- A plain `ALTER TABLE ... ADD COLUMN` on `w1` only: `w2`'s apply worker fails
  on the first row (`unknown column name plain_col`), the subscription goes
  `down`, and 290 rows pile up in ten seconds. The same `ALTER` by hand on
  `w2` ten seconds later fixes it, but the worker is only back 187 seconds
  after the first `ALTER`, both runs: its restart delay grows with each
  failure. Nothing was lost.
- `replicate_ddl_command('alter table ... add column ...')` on `w1`: `w2` had
  the column with nothing run there, lag zero, both writers running. The same
  with `rename column`: renamed on `w2`, lag zero.
- pglogical's own README says this will not be enough once the subscriber
  also writes with the old structure, and that is the situation of two
  writers on one table. The test did not hit it, which says nothing about the
  window.
- The slot pglogical creates has `failover = false`. It issues
  `CREATE_REPLICATION_SLOT ... LOGICAL pglogical_output FAILOVER`, the
  2ndQuadrant syntax, and only on 2ndQuadrant's PostgreSQL 9.6 and 11 builds;
  community Postgres 17 and later spell the option `(FAILOVER true)`, so a
  promotion loses the slot the way pgcopydb's is lost.

It can still be made a failover slot, without changing pglogical. The slot is
created once, when the subscription is, and nothing touches its flag
afterwards, so Postgres 17's `ALTER_REPLICATION_SLOT` does it from a
replication connection (`psql "dbname=app replication=database" -c
'ALTER_REPLICATION_SLOT "pgl_app_provider_sub" ( FAILOVER true )'`) while the
slot is inactive: disable the subscription with
`pglogical.alter_subscription_disable`, run the command, enable it again.
Creating the slot by hand first does not work: pglogical drops an inactive
slot of that name and creates its own. On a provider with a physical standby
(`sync_replication_slots = on`, the provider's `synchronized_standby_slots`
naming the standby's slot) and a subscriber whose provider DSN lists both
hosts with `target_session_attrs=read-write`:

- the slot flips to `failover=true`, and the standby has a `synced=true` copy
  within seconds;
- the provider is killed under a writer and the standby promoted; the apply
  worker reconnects to it by itself and the 200 rows written on the new
  provider reach the subscriber one second after the last insert, with no row
  missing;
- the same run without the `ALTER_REPLICATION_SLOT` step has no slot on the
  standby, the worker dies on `START_REPLICATION SLOT ... does not exist`, the
  subscription stays `down`, and the 200 rows never arrive.

One trap is the demo's, not pglogical's: the promoted standby inherits
`synchronized_standby_slots = 'e_s1_slot'`, a slot it does not have, and
logical decoding then waits for it forever. The script resets it by hand; in
this system, pg_auto_failover computes that list. One run of each.

### What the three add up to

| | Core | pglogical | pgcopydb |
|---|---|---|---|
| Schema change in the stream, in order with the rows | no | yes, through `replicate_ddl_command` | yes, with the event trigger |
| Survives a rename with writers on both sides | no | works, the README warns of the window | one-way link only |
| Row filters | yes | yes (`row_filter`) | no (tables only) |
| Failover-enabled slot | yes | not as shipped; `ALTER_REPLICATION_SLOT` after creation does it | with `--enable-failover` |
| Link in both directions | yes | yes | no |

None of the three has every column of that table. The combination the system
needs (native filters and failover slots, with the schema change in the
stream) is not available off the shelf today. What works is the discipline of
the native column: additive, nullable changes, applied everywhere with a short
pause, and the rest as expand and contract. Speed, first copy and version
support come next, side by side.

---

## 7. Choosing the tool, and adding a region

The schema comparison looked at one axis. The choice between the three has
several, and they pull in different directions: where the code runs, how fast
it applies changes, what it does with the first copy of a large database,
whether it carries DDL, and which Postgres versions it can connect.

### Three architectures

{{< image src="fig-deployment.svg" title="Where the replication code runs: inside the servers, or between them." >}}

**Core logical replication** is part of Postgres. The publisher's walsender
decodes the WAL with the built-in `pgoutput` plugin and streams the changes.
On the subscriber, an apply worker, a background process of the server
itself, applies each change as it arrives, in one local transaction per
source transaction, and commits when the COMMIT message comes in. Nothing is
stored in between (`replication/logical/worker.c`). The target is the one who
pulls: it opens the connection to the source.

**pglogical** has the same shape, as an extension that has to be installed on
both servers (`shared_preload_libraries`, one build per major version). It
brings its own output plugin and its own apply workers. They write to the heap
directly, turn runs of consecutive inserts into one `heap_multi_insert` call,
and commit asynchronously by default (`pglogical_apply.c`, settings
`pglogical.batch_inserts` and `pglogical.synchronous_commit`).

**pgcopydb** is another kind of tool, and logical replication is not its main
use case. Its documentation puts it as "migration to a new Postgres system,
either for new hardware, new architecture, or new Postgres major version":
`pg_dump -jN | pg_restore -jN` between two running servers, with the
parallelism of both and the streaming that the two programs cannot do
together. It takes one snapshot, copies the tables and builds the indexes in
parallel, and, to keep the cutover short, follows the changes of the source
from a slot created at that same snapshot, which it reads all the time the
copy runs: the source can forget the WAL as soon as it is spooled on the
pgcopydb host, however long the copy takes. That last part is logical
replication, used as the way to catch up. It is a separate program, run on any
host that can reach both servers, and it needs nothing installed on either.
It reads the slot, writes every change to a SQLite file, transforms the
changes into SQL statements kept in a second SQLite file, and replays those on
the target through ordinary connections with prepared statements. SQL over
plain connections works against any server, managed or not, at any version,
which is the reach a migration tool needs. The price is that every change is
written twice and read twice before the target sees it.

### Order of magnitude

The same table on two Postgres 18 servers on one machine, over a one-way link,
with a trigger on the target that stamps the time a row is applied. Four
workloads: 1,000 single-row transactions 50 ms apart (the stamp minus the
source's `clock_timestamp()` is the latency), eight concurrent writers for 30
seconds, one transaction of a million rows, and one `UPDATE` statement of a
million rows.

| | Core | pglogical | pgcopydb |
|---|---|---|---|
| Latency of a single-row transaction, p50 / p99 | 0.3 / 1.7 ms | 0.2 / 2.0 ms | 0.7 / 7.8 ms |
| Rows a second under 8 writers | 80,000 (the source commits 85,000) | 84,000 (85,000) | 8,900 while loaded, 16,000 over the run with the catch-up (the source commits 67,000) |
| Lag once the 30 s load stops | 2 s | 0.2 s | 95 s |
| One transaction of 1M rows, seconds after its commit | 4 to 6 | 3 to 6 | 5 to 6 |
| One `UPDATE` of 1M rows that changes nothing, `REPLICA IDENTITY FULL` | 3 to 5 s | 4 s | 6 to 8 s |

The source commits 67,000 to 85,000 single-row transactions a second in the
third workload (all three run on one machine, and the run with pgcopydb is the
one where the source committed fewer). Core and pglogical keep up with it. pgcopydb applies 9,000 to 16,000 rows a second and the rest queues:
the 95 seconds it needs to catch up after a 30 second load are a backlog, not
a stall. On an idle link a change reaches the target in a fraction of a
millisecond with the two in-server tools, and in under a millisecond to a few
milliseconds with pgcopydb. A single large transaction narrows the gap to
nothing much: the three wait first for the same thing, because the source
decodes a transaction when it commits and needs a second or two to hand out
the last change, and pgcopydb transforms and applies the transaction while it
is still being received. The `UPDATE` changes no value, which pgcopydb
recognises when it has the whole old row (`REPLICA IDENTITY FULL`) and sends
nothing for, where core and pglogical apply every row; it still has to receive
and read back a million messages, which is what the 6 to 8 seconds are.
pglogical took 66 seconds for that statement in one run, after the throughput
workload had left 2.5 million rows in the table, and 4 seconds in the two runs
on a clean table; that was not investigated.

Read these as orders of magnitude: one machine, a handful of runs, and the
runs of one tool differ by a factor of two on the large transactions. "About
the same", "a few times slower" and "ten times slower" are the claims, not the
digits. For a migration, 10,000 rows a second is more than most databases
write; for a replica behind a source that takes tens of thousands of writes a
second, it is not enough.

One more number is memory. pgcopydb transforms and applies a transaction in
passes while it is received, so what it holds is what arrived between two
passes, not the transaction: its apply process peaked at about 70 MB for the
transaction of one million inserted rows, and at about 230 MB for the `UPDATE`
of one million rows with their whole old rows. A pgcopydb that held the whole
transaction would need about 340 MB and 1.4 GB for the same two. Core and
pglogical apply a change as it arrives, and a million rows cost them no more
memory than one.

### What each one is for

- **Core logical replication** is the default when both servers run Postgres
  10 or later and you may configure them: the lowest latency, the highest
  throughput, nothing to install, and the only one of the three that is part
  of the server's own release process. It carries the region links of this
  system. Its gaps are the ones listed above: no DDL, no help with a first
  copy that is large or interrupted, and some features need a recent version
  on both sides.
- **pglogical** is for what core cannot do: servers older than 10 down to 9.4,
  per-table replication sets, five conflict policies when two nodes write the
  same row, and a `replicate_ddl_command()` that puts a schema change in the
  stream. It needs the extension on both servers, so it is out on hosting that
  does not offer it.
- **pgcopydb** is for migrating a database, in particular a large one, to a
  server that cannot be a physical copy of the first: another CPU
  architecture or operating system, another build of Postgres, another major
  version, a hosted service. It is dump and restore with the parallelism, the
  resume and the filtering that `pg_dump | pg_restore` lack, and then a short
  cutover: the changes captured from the first second, DDL carried in the
  stream, and nothing installed on either side. It is not the tool to leave
  running behind a source that takes tens of thousands of writes a second: the
  table above says why.

### What each supports

| | Core | pglogical | pgcopydb |
|---|---|---|---|
| Installed on | nothing | an extension on the source and on the target | nothing on the servers; one program on any host |
| Postgres versions | 10 and later; source and target may be different major versions | 9.4 and later on both, one build per major version | source 9.6 and later (`wal2json` or `test_decoding` before 10, `pgoutput` from 10); target: a server `pg_restore` can load; `pg_dump` must be at least as recent as the source |
| DDL | not replicated | `replicate_ddl_command()`, called by hand, in the stream | an event trigger emits it as a message, in the stream, once `pgcopydb stream event-trigger` has installed it |
| First copy | one table synchronization worker per table (`max_sync_workers_per_subscription`, 2 by default): each table is one `COPY` | structure with `pg_dump` and `pg_restore`, then one `COPY` per table through sync workers | `--table-jobs` tables at once, a large table split across several `COPY` streams (`--split-tables-larger-than`), indexes built in parallel |
| A long, slow link | a broken connection starts the table over | the same | many streams to fill the link; `--resume` does not resend the tables already copied (the one that was in flight starts over) |
| WAL kept on the source during the copy | from the sync slot of each table, until that table is copied and caught up | from the sync slot, for the whole data copy | what receive has not yet spooled |
| Memory for a large transaction | constant: a change is applied as it arrives | constant | bounded by what arrives between two passes: about 70 MB for a million inserted rows, 230 MB for a million updated rows with their old rows |
| Sequences | values copied on demand from Postgres 19 (`REFRESH SEQUENCES`) | `replication_set_add_sequence` | copied at the end of the clone |
| Row filters, columns | yes (15 and later) | yes | tables only |
| Both directions | yes (`origin = none`, 16 and later) | yes | no |
| Conflict handling | detects and counts (18), can skip | five policies | none |
| Failover slot | yes (17 and later) | not as shipped | `--enable-failover` |

Read the first-copy rows with the network in mind. A single TCP stream rarely
fills a long, slow link, and a copy that breaks after nine hours of a ten hour
table is a bad evening. Core and pglogical copy one table over one stream and
begin that table again if the connection drops; pgcopydb opens as many as you
allow, can split one table across several of them (on an integer key, or on
`ctid` when there is none), and remembers which tables are done. None of the
three has a compression option; that is the network's job. These rows come from
the documentation and the source of each tool; the lab measures speed, not
copy times.

### The first copy and the source's WAL

A first copy of a huge table is long, and a logical slot keeps the WAL the
source has written since the slot's position until its consumer confirms it.
What each tool does with that position during the copy decides how much WAL a
busy source has to keep.

**Core** gives each table its own synchronization worker
(`replication/logical/tablesync.c`). The worker opens `BEGIN READ ONLY
ISOLATION LEVEL REPEATABLE READ` on the publisher, creates a permanent logical
slot named `pg_<subscription>_sync_<table>_<system id>` inside that
transaction, copies the table, and then reads its slot to apply the changes
made since the snapshot until it reaches the position of the subscription's
apply worker; the slot is dropped after that. What the source keeps for a
table is the WAL since its sync slot was created, until the table has been
copied and has caught up, and the snapshot of the copy holds back vacuum on the
publisher for as long as the `COPY` runs. The source comment says why it is
done per table: the synchronization "does not have to hold the xid and LSN
for the time it takes to copy data of all tables". The subscription's own slot
keeps being consumed meanwhile, the apply worker skipping the changes of the
tables still being copied. The bound is one table's copy, times the workers
running at once, and for a table of several terabytes that is the whole copy.

**pglogical** takes one slot and one exported snapshot for the whole
subscription (`pglogical_sync_subscription`): the structure with `pg_dump
--snapshot`, then the data of the tables of the replication sets, then the
constraints, and only then the catch-up from the slot. The source keeps the
WAL and the snapshot for all of it.

**pgcopydb** takes a snapshot and creates its slot at the same point, copies
with several workers, and receives the changes into SQLite at the same time.
The position it tells the source it has flushed is the one receive has
committed and synced to its SQLite file, so the source can recycle the WAL
while the clone runs, and the changes wait in files on the pgcopydb host
instead of in the source's `pg_wal`. Version 0.18, the last release, reports
the position apply has committed on the target instead, and apply is only
enabled when the clone is done, data, indexes and post-data included: with
0.18 the source keeps the WAL since the slot was created for the whole clone,
and until apply has caught up. What the concurrent capture buys on top is that
the changes are already decoded when the copy ends, so the catch-up is a local
replay, where core has the source decode that backlog after the copy.

**Verified**, by running it: a table of 600,000 rows (261 MB, with three
expression indexes) is copied while a writer produces 3 to 4 MB of WAL a
second on the source, and the lab samples how far behind the current WAL
position each slot's `restart_lsn` is, every two seconds. The sizes are small,
the arithmetic is not: at 3 MB a second, a ten hour copy keeps about 100 GB.

| | Core | pgcopydb (`main`) | pgcopydb 0.18 |
|---|---|---|---|
| During the copy | the sync slot of the table: 52 MB after 2 s, 133 MB after 6 s | the slot: 127 MB after 2 s, 151 MB after 6 s | the slot: 122 MB after 2 s, rising |
| After the copy | the sync slot is gone; the subscription's slot sits between 0 and 122 MB | between 46 and 202 MB, in steps | rising to 208 MB for 24 s, while apply replays what was stored, then 80 MB |

A second run shows the edge. With the writer in another database of the same
server, which nothing decodes, core's slot still advances. Its slot sits between 0 and 70 MB, in steps of about fifteen seconds. pgcopydb
0.18's grew without end: 122 MB at the start, 647 MB after two minutes, and it
was still growing, because it reported apply's position, apply only moves it
when it commits a transaction of the database, and a quiet database on a busy
server commits none. On `main` it moves in steps too, between 57 and 186 MB. The
sizes in a real migration depend on the write rate of the whole server, not
only of the database being copied, so `max_slot_wal_keep_size` and a look at
`pg_replication_slots.wal_status` belong in the plan of any long copy with any
of the three: a slot that goes past the limit is invalidated, a core table
synchronization starts that table over, and a pgcopydb clone has to start
again.

### When the two servers cannot share a binary format

The tools above are compared on speed, and for pgcopydb that is not the
question it was made for. A physical copy of a cluster (a base backup, a
streaming standby, a copy of the files) moves pages and WAL as they are, so
the two servers have to agree on everything that shapes those bytes. At
startup the server compares its own settings to the ones recorded in
`pg_control` and refuses to run on a mismatch ("database files are
incompatible with server", `ReadControlFile()` in `access/transam/xlog.c`),
and `pg_upgrade` makes the same comparison between the old and the new cluster
(`check_control_data()` in `bin/pg_upgrade/controldata.c`). What has to match:

- **The CPU architecture**: word size, maximum alignment (`MAXALIGN`) and byte
  order. The documentation is blunt about it: "the hardware architecture must
  be the same — shipping from, say, a 32-bit to a 64-bit system will not
  work."
- **The relation block size**, 8 kB unless built with `--with-blocksize` (the
  `blocksize` option of the meson build), from 1 to 32 kB. It fixes the TOAST
  chunk size and the large-object chunk size too.
- **The WAL block size**, 8 kB, `--with-wal-blocksize`, from 1 to 64 kB.
- **The relation segment size**, 1 GB, `--with-segsize` or `segsize_blocks`.
- **The WAL segment size**, 16 MB, chosen by `initdb --wal-segsize`.
- **The maximum identifier length** (`NAMEDATALEN`, 64), **the maximum number
  of columns of an index** (`INDEX_MAX_KEYS`, 32) and **the SLRU segment size**
  (`SLRU_PAGES_PER_SEGMENT`, 32): constants of `pg_config_manual.h`, each with
  the comment "changing this requires an initdb".
- **Data checksums**, set by `initdb --data-checksums`. `pg_upgrade` refuses a
  mismatch in either direction.
- **The catalog version and the control file version**, which change with the
  major version: this is why a physical copy needs the same major version.

Outside `pg_control`, the operating system and its C library decide how text
sorts, and a physical copy carries indexes that are only correct where that
order is the same.

| | Same major version | Architecture and build settings | Online |
|---|---|---|---|
| Physical standby, base backup, file copy | required | must match | yes |
| `pg_upgrade` | new major version | must match | no |
| Logical replication, core or pglogical | no | free; "Linux to Windows" is in the list of uses in the documentation | yes, but the first copy and DDL are yours |
| `pg_dump` and `pg_restore` | no | free: "the only method that will work when transferring a database to a different machine architecture" | no |
| pgcopydb | no | free | yes, after a parallel dump and restore |

pgcopydb belongs to the last two rows, and its main use case is the second to
last: a dump and a restore, made fast, with the catch-up of logical
replication added at the end so that the cutover is short. The migrations it is
for are the ones where the first two rows are closed: from a server built with
a 32 kB block size, a custom segment size or an unusual `NAMEDATALEN` to a
default build, which is what a hosted service runs; from one CPU architecture
or operating system to another; to a server with data checksums when the old
one had none; or from a database nobody can install an extension on. Logical
replication ignores the layout too, and it can do the same moves for the tables
it carries; it leaves the first copy, the schema and the sequences to you, and
that is the part of the work pgcopydb packages. A new region of the system
built here is exactly that case: a first copy of a database that is already
large, over a long link, that must not wait for a maintenance window.

---

## 8. When two writers touch the same row

Not every schema can be partitioned by owner. A reference table that every
region edits is the honest counterexample, and then conflicts are real. Core
**detects and counts** them (Postgres 18 logs each conflict and counts it by
type in `pg_stat_subscription_stats`; Postgres 19 adds `retain_dead_tuples`,
which keeps what the subscriber needs to tell an update that raced a delete
from a missing row, and counts those as `update_deleted`) and can **skip** one
(Postgres 15, `ALTER SUBSCRIPTION ... SKIP`). It does not resolve one.
pglogical's five policies, `error`, `apply_remote`, `keep_local`,
`last_update_wins` and `first_update_wins`, are the floor.

**Verified**, by running it: pglogical 2.4.8 (the PGDG package) on Postgres
18.6, two nodes subscribed to each other, four clients each updating the
same 1,000 keys at 2,000 transactions per second per node for 15 seconds
(about 60,000 updates), three runs of each policy:

| Policy | Rows that differ afterwards, of 1,000 (3 runs) | Replication |
|---|---|---|
| `last_update_wins` | 0, 0, 0 | healthy |
| `apply_remote` | 1, 0, 1 | healthy |
| `keep_local` | 1000, 1000, 1000 | healthy, no error |
| `first_update_wins` | 1000, 1000, 997 | healthy, no error |
| `error` | 1000, 1000, 1000 | both subscriptions `down` |

`last_update_wins` converged every time, though it keeps one row, not both: of
the roughly 60,000 increments, 8, 12 and 22 were overwritten by a concurrent
update to the same row. `apply_remote` usually converges and sometimes leaves
one row different on the two nodes. `keep_local` and `first_update_wins` do
what they say on the tin, each node keeps its own value, and nothing in the
logs says the nodes now disagree. `error` stops replicating until someone
intervenes.

Two more findings from the same lab. Replicating into a **partitioned parent**
with pglogical copies the initial rows, then the first replicated `INSERT`
kills the apply worker with `signal 11`; the postmaster restarts every backend,
and the subscription crash-loops (about 900 crashes in the 30 seconds before
the demo drops it). Core replicates into partitioned tables since Postgres 13.
And **moving a customer between two row filters** reaches neither subscriber
correctly: worker 1 keeps the stale row, worker 2 never gets it, and the hub
has the new value.

The sequence gap is the other half of multi-writer. **Verified** against the
commitfest entry: Michael Paquier's "Sequence Access Methods, round two"
([commitfest #4677](https://commitfest.postgresql.org/52/4677/)) was returned
with feedback in PG19-Final on 2026-02-27, after more than two years moving
between commitfests without landing. It is not in Postgres 19.
`galloc` and `snowflakeid` remain features of EDB Postgres Distributed, the
commercial successor of BDR, and not of core. What Postgres 19 did commit is
sequence synchronization, which helps a cutover and does not let two writers
draw from one sequence.

---

## 9. What BDR did, and what is left

BDR 1.0 was the open source multi-master of 2ndQuadrant: a patched PostgreSQL
9.4 and a plugin, whose last commit on the 1.0 stable branch is from November
2019. It is not maintained, and its successors are not open source, so this is
a comparison of designs, read from the documentation in its repository
([`doc/`](https://github.com/2ndQuadrant/bdr/tree/bdr-plugin/REL1_0_STABLE/doc)),
and nothing here was run.

| | BDR 1.0, from its manual | The system built here |
|---|---|---|
| Topology | A full mesh: every node connects to every other, and a joining node configures all existing ones to connect to it ([node management](https://github.com/2ndQuadrant/bdr/blob/bdr-plugin/REL1_0_STABLE/doc/manual-node-management.sgml)). Five nodes are 20 directed links. | Five groups, eight subscriptions: hubs and cascades chosen by hand. |
| Conflicts | Optimistic. Row conflicts resolve to the most recent tuple by the originating host's timestamp, or a user handler; a primary-key update or an insert violating several unique constraints is "divergent" and needs an operator ([conflicts](https://github.com/2ndQuadrant/bdr/blob/bdr-plugin/REL1_0_STABLE/doc/manual-conflicts.sgml)). | Avoided: disjoint `region` keys. A conflict is an error to see, not a rule to apply. |
| DDL | Replicated automatically, under a global DDL lock: every node must be reachable and caught up, writes on every node pause or fail meanwhile, DDL is refused while the lock is held elsewhere, and a down node must be removed first and cannot come back ([DDL replication](https://github.com/2ndQuadrant/bdr/blob/bdr-plugin/REL1_0_STABLE/doc/manual-ddl-replication.sgml)). | No lock, and nothing automatic: a short pause, or the change travels in the stream. |
| What DDL may do | A list of refused statements, among them `ADD COLUMN ... DEFAULT` and anything that rewrites a table. | Whatever Postgres accepts; the replicas must keep up. |
| Sequences | Global sequences (`CREATE SEQUENCE ... USING bdr`), allocated in chunks by a vote of the majority, unusable on two nodes; the manual calls them deprecated and recommends step and offset sequences ([global sequences](https://github.com/2ndQuadrant/bdr/blob/bdr-plugin/REL1_0_STABLE/doc/manual-global-sequences.sgml)). | No shared sequence: the key is `(region, id)`. |
| What replicates | Replication sets per table; a table's DDL goes to all nodes whatever its sets ([replication sets](https://github.com/2ndQuadrant/bdr/blob/bdr-plugin/REL1_0_STABLE/doc/manual-replication-sets.sgml)). | Row filters per publication; DDL is outside them. |
| A node is down | One slot per peer holds WAL on every other node until the node is parted, with the disk-full outcome the manual warns about; a parted node cannot rejoin. | A node is a group of three; failover is pg_auto_failover's, and the slot follows through `failover = true`. |

Two readings follow. BDR 1.0 bought "every change reaches every node" (rows,
schema, keys) with a coordination cost this system refuses to pay: no global
lock, no majority vote for a sequence, no write pause on a continent because
one node is slow. What it pays instead is the operator's discipline in the
table above. And what BDR 1.0 left out is the problem the first section is
about: its manual does not discuss the failover of a node's own server or
what happens to a peer's slot when that server's standby is promoted, because
in 2019 on PostgreSQL 9.4 the primitive did not exist. The slot
synchronization of Postgres 17 is the piece BDR 1.x lacked, and
`synchronized_standby_slots` is the guard it would have needed.

### What is left

- **DDL in the stream.** Core replicates none of it. pgcopydb and pglogical
  each carry part of the answer outside core, and a schema change that
  travels in the same commit order as the rows is the piece no core feature
  provides.
- **Sequences.** Postgres 19 can copy their values on demand
  (`REFRESH SEQUENCES`, publisher 19 or later), which is what a cutover
  needs. Nothing streams them as they advance, and nothing allocates them
  across several writers.
- **Conflict resolution.** Postgres 18 counts conflicts; it does not resolve
  them. Avoiding them by construction, as the architecture here does, is
  still the only built-in answer.
- **Joining and leaving a topology.** BDR had `join` and `part`; today adding
  a region or retiring one is a procedure written by hand.
- **The consumer as a first-class node.** A logical consumer is not in the
  state machine of anything that orchestrates the primary it reads from, so
  who creates the slot, who checks its position after a promotion and who
  raises the alarm when it is ahead is still a script. Every topology routine
  of pg_auto_failover, from `synchronous_standby_names()` to `formation_uri()`,
  iterates the node table and would miscount a consumer registered as a
  standby, and there is no state machine the monitor drives for it. That is
  development on the
  [pg_auto_failover roadmap](https://oss.theartofpostgresql.com/roadmap/)
  (first-class logical replication nodes, cascading replication for
  multi-data-centre topologies, disaster recovery with archiving), a project
  maintained by one person and
  [funded by the teams that run it](https://oss.theartofpostgresql.com/pg_auto_failover/).

None of this was possible ten years ago, and most of it is other people's
work. Logical decoding arrived in 9.4 (Andres Freund, with 2ndQuadrant) and
gave the WAL a meaning outside the server. pglogical and BDR, the work of
Petr Jelinek, Craig Ringer and their colleagues, showed what it was for, and
the publications and subscriptions of Postgres 10 made it part of core. The
releases since have been the long tail of making it usable for real systems,
and a primary that can die without its change stream losing or inventing a
row, with the standbys following the slot by themselves, is the result of
years of unglamorous, carefully reviewed work, merged one release at a time.

Every part of the series has a `compose/` directory of its own, this one
included. Run them, break them, and tell me what I got wrong.

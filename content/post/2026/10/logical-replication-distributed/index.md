+++
title     = "Postgres logical replication beyond one writer"
date      = "2026-10-27T09:00:00+0100"
tags      = ["PostgreSQL", "Replication", "Logical Decoding", "Architecture"]
categories = ["PostgreSQL", "Architecture"]
icon      = "🐘"
+++

This is part 4, the last one, of a series about Postgres logical replication
use-cases, and about how the feature set has evolved over the past ten
years and ten releases.
[Part 1](/blog/2026/09/ten-years-of-postgres-logical-replication/) built a
hub-and-workers system for write scalability.
[Part 2](/blog/2026/10/consolidating-databases-with-postgres-logical-replication/)
consolidated several application databases into one warehouse.
[Part 3](/blog/2026/10/zero-downtime-postgres-upgrades-with-logical-replication/)
covered a zero-downtime major upgrade, with a way back. All three assumed
one writer, one region, and a table-shaped buffer for change data capture.
This post is what changes once each of those assumptions breaks, in the
order they tend to break in practice: the primary itself fails over, the
servers are no longer close together, more than one of them writes, the
schema has to change on all of them at once, and the CDC consumer has to
be a real one instead of a demo.

Like the rest of the series, everything built here is open source:
Postgres itself, pg_auto_failover, pgcopydb, and pglogical where Part 1
used it. BDR, the multi-master that was open source until 2019, and the
commercial systems that followed come up as a reference point, in the
conflict section and in a comparison near the end, rather than something
to build against.

<!--more-->

{{< lab >}}
The demos for this post are in the `compose/` directory next to it. They
build pg_auto_failover and pgcopydb from source (the features used here
are on their `main` branches and not in a tagged release yet). Statements
marked **verified** were checked against the Postgres, pg_auto_failover,
pglogical or pgcopydb source, or against a run of the demos, and every
number comes from such a run.
{{< /lab >}}

<!--toc-->

---

## The scenario

{{< image src="fig-region-map.svg" title="One writer per region: conflicts avoided by construction." >}}

Four equal primaries, four disjoint region keys — us-east in New York, us-west in San Francisco, ap-south in Mumbai, ap-southeast in Sydney — so no two of them ever mint the same row. Every site, the eu-west warehouse in Amsterdam included, is a pg_auto_failover group of its own: a monitor and three nodes, one primary and two standbys, not a single server. The groups share no failover: they are linked by logical replication only. Every link is the same bidirectional logical replication: a region's own writes flow one way, a read-only mirror of everyone else's data flows back the other way. New York and Mumbai link straight to one global warehouse in Amsterdam; San Francisco and Sydney cascade through them instead of each opening its own long-haul connection. Every site has its own front desk, read-write against its own region and read-only against the mirror of the rest.

Five things, each a post of its own in a shorter series, covered here in
one, in the order a team actually runs into them: the primary dies and a
standby takes over — does the CDC consumer notice; the servers are a
continent apart — does that change the answer; two of them accept writes
on purpose — what happens when they touch the same row; a table changes
on all of them — which tool carries the schema; and the thing
reading the change stream has to be a real program, not three sections
of a [Part 2](/blog/2026/10/consolidating-databases-with-postgres-logical-replication/)
demo that stopped on purpose before finishing one.

## Publisher failover, and what it costs across a region

Every subscription in this series so far pointed at one primary and
stayed there.
[Part 3](/blog/2026/10/zero-downtime-postgres-upgrades-with-logical-replication/)'s
own reverse-subscription trick survives a planned cutover; it says
nothing about the primary dying unplanned, mid-stream,
with a consumer attached to it.

### What Postgres actually guarantees

**Verified**, against the `REL_19_RC1` tag, not inferred from the feature's
name. Postgres 17 added `failover = true` on a replication slot and the
`synchronized_standby_slots` GUC. Together they change one thing, exactly:
a logical walsender gating on a failover slot will not send a client any
WAL past what every standby named in `synchronized_standby_slots` has
already flushed (`replication/slot.c:StandbySlotsHaveCaughtup`,
`replication/walsender.c:WalSndWaitForWal`). The consequence is the one
that matters for a CDC consumer: if the primary dies and one of those
standbys is promoted, nothing the consumer already received can be absent
from the new primary's own history, because the gate never let it out
until the standby had it too. The failure mode left on the table is
duplicates, not holes — the consumer's `confirmed_flush` on the promoted
standby's slot is always at or behind what it last acknowledged, never
ahead.

That guarantee has conditions, and each one is a way to lose it
silently, not loudly:

- The slot has to be `failover = true` from the start — a plain slot
  gets no protection at all.
- `synchronized_standby_slots` has to actually name the standby that
  ends up promoted. Two standbys, only one listed, the other one
  promoted: the gate never applied to the node that's now primary.
- The standby needs `sync_replication_slots = on`, `hot_standby_feedback
  = on`, and `dbname` in its `primary_conninfo` — slot state does not
  follow a plain streaming standby that's missing any of the three.
- A slot that stayed `temporary` on the standby — because it never
  caught up to the remote's `confirmed_flush`, or never got a consistent
  snapshot — is dropped the moment that standby is promoted, and cannot
  be recreated in the past.

pg_auto_failover (`main`; the last tagged release, v2.2, predates it)
does that part. A monitor function,
`pgautofailover.synchronized_standby_slots(formation, group)`, is
computed the way `synchronous_standby_names()` is, every current failover
candidate's physical slot name, and `pg_autoctl` fetches it and applies it
on the primary through the same `fsm_apply_settings()` path. It re-applies
it whenever the set of candidates changes. `sync_replication_slots` is on
by default for Postgres 17+ nodes, next to `hot_standby_feedback`, and
`wal_level` is `logical` on every node it creates (`--wal-level replica`
opts out). A node that rejoins as a standby after an interrupted demotion
drops its stale failover slots instead of letting them block the
slot-sync worker. What it deliberately does not do is create or manage
the logical slot itself: that stays the CDC tool's job, the way a table's
schema or a `pg_dump` object list always was.

The monitor still gets you the orchestration (who is primary, when a
promotion happens, a connection string that survives one), and the GUCs
that make a failover-safe slot mean what its name says, kept correct
across every role change.

{{< image src="fig-failover-slot.svg" title="A failover-safe logical slot, driven by pg_auto_failover's existing orchestration." >}}

Physical replication keeps the standby current, including the slot's own state, once synchronized_standby_slots names it. After promotion the consumer reconnects to the same slot, now on the new primary, and resumes at its last confirmed LSN: duplicates are possible, a hole is not.

### Building it against pg_auto_failover

Not a new `pg_autoctl` verb (that is a real feature, scoped separately
below), only three things for the consumer to do:

1. Create the slot with `failover = true` on the primary. The slot is
   the consumer's to manage; pg_auto_failover keeps the GUCs around it
   correct.
2. Persist the consumer's own position durably, keyed by the last
   `end_lsn` it has fully applied — never resume from 0, never trust an
   in-memory position across a restart.
3. On every reconnect, `IDENTIFY_SYSTEM` before trusting the server: if
   its reported position is behind what the consumer already has, or a
   `TIMELINE_HISTORY` switchpoint sits below the consumer's stored LSN,
   stop and raise an alarm instead of streaming — a consumer that is
   ahead of the history it is about to read from is the one case the
   slot's own guarantee cannot protect.

**Verified**, by running it: profile `a` in `compose/` is the `us-east`
region, a pg_auto_failover group of its own, a monitor and three nodes
(`us-east-a`, `us-east-b`, `us-east-c`) on Postgres 17, built from
pg_auto_failover `main` and left at its default timings. A
`pg_recvlogical` consumer (`test_decoding`, one-second feedback interval)
follows a `failover = true` slot while a writer inserts about 25 rows a
second, one row per transaction, with ids it chooses itself so a retry
is recognisable. Each scenario ran three times; "holes" are rows the
final primary has that the consumer never received, "ahead" are rows
the consumer received that the final primary does not have.

| Scenario | Duplicates | Holes | Ahead | Writer blocked |
|---|---|---|---|---|
| `pg_autoctl perform failover` | 0, 0, 0 | 0, 0, 0 | 0, 0, 0 | 1.4 to 2.2 s |
| primary killed | 3, 4, 2 | 0, 0, 0 | 0, 0, 0 | 51 to 52 s |
| both standbys cut off, then primary killed, gate on | 0, 0, 0 | 0, 0, 0 | 0, 0, 0 | 51 to 53 s |
| same, `synchronized_standby_slots` emptied | 0, 0, 0 | 4, 13, 3 | 92, 69, 80 | 51 to 53 s |

- **The guarantee holds as described.** With the gate on, a planned
  failover produces no duplicate at all and a killed primary produces
  a handful, bounded by the consumer's feedback interval, and neither
  produces a hole. The consumer resumes by itself through a multi-host
  connection string, up to five seconds after the new primary accepts
  writes, which is `pg_recvlogical`'s own reconnect delay.
- **The gate does its job when the standbys are behind.** In the third
  scenario both standbys stop receiving WAL five seconds before the kill
  and the writer commits with `synchronous_commit = local`. The consumer
  receives nothing from that moment on, because the walsender will not
  send past what the standbys have flushed. Between 67 and 79 rows the writer had
  been told were committed are lost with the old primary — that is the
  cost of `synchronous_commit = local`, and not the consumer's to carry —
  but none of them had been delivered downstream.
- **Misconfigured, the prediction comes true, and then some.** With
  `synchronized_standby_slots` empty, the same sequence delivers 69 to
  92 rows to the consumer that the new primary never had. The new
  primary then writes new rows at WAL positions the consumer's
  confirmed position is already past, so the slot skips them: 3 to 13
  rows written after the failover never reach the consumer at all. The
  consumer is ahead of the history it reads from, which is the case the
  `IDENTIFY_SYSTEM` check in step 3 exists to catch, and nothing in
  Postgres raises it for you.
- **What dominates the downtime is not the slot.** With three nodes a
  planned failover blocks the writer for a second or two. A killed primary
  blocks it for about 51: roughly ten seconds for the monitor to mark the
  node unhealthy, and the rest in pg_auto_failover's drain timeout before
  the new primary is allowed to take writes. Those are defaults, they can
  be tuned, and they are measured here on a local Docker network, not a
  WAN.

What the operator sees is the monitor's own view. The nodes carry a
free-form `region` label, set with `pg_autoctl set node region`; it is
purely informational and no failover or quorum decision reads it, but
`pg_autoctl watch` shows it as a column. It is there for what
pg_auto_failover does not do yet: handling cascading physical
replication across data centres, and several archiving nodes. Neither
exists today, and neither would change anything in this post, where the
links between regions are logical replication, not a standby of a
standby. The labels used in this post are the site names of the map.
Every region is a pg_auto_failover group of its own, with its own
monitor and three nodes named `<region>-a`, `<region>-b` and
`<region>-c`, and the three nodes carry the same label: `us-east` for
New York, `us-west` for San Francisco, `ap-south` for Mumbai,
`ap-southeast` for Sydney, and `eu-west` for the Amsterdam warehouse.
The monitors carry none. For the four sites that own rows, the label is
also the value of the `region` column, so a row's key and its node's
label name the same place. Here is a frame of `pg_autoctl watch --group 0` taken 25 seconds after the
primary was killed, with the monitor part-way through the election:
`us-east-a` is still `primary` but assigned `demote_timeout`, `us-east-c`
has been picked and is in `stop_replication`, and `us-east-b` is
following it (`join_secondary`):

```
Formation: default - Sync Standbys: 1                                       23:27:13

     Name  Node       Region Quorum Priority      Host:Port       TLI: LSN Last Check   Connection Last Report      Reported State      Assigned State
us-east-a     1      us-east    yes       50 us-east-a:5432   1: 0/5042190         2s read-write !         28s             primary      demote_timeout
us-east-c     2      us-east    yes       50 us-east-c:5432   2: 0/50424E8         1s    read-only          1s    stop_replication    stop_replication
us-east-b     3      us-east    yes       50 us-east-b:5432   1: 0/5042310         1s  read-only !          1s      join_secondary      join_secondary
```

`pg_autoctl show synchronized-standby-slots` prints both standbys' slots,
`'pgautofailover_standby_2,pgautofailover_standby_3'`, on a healthy
group, and one, `'pgautofailover_standby_3'`, once the primary has been
killed and a standby promoted: the gate shrinks to the standby that is
left, and keeps protecting the consumer, which a two-node group could not
do.


### What a region's failover looks like from the other regions

The scenario of this post is not one group: it is several, replicating to
each other. So the question the drill above leaves open is what a
failover in one region does to the logical replication it takes part in
with another, in both directions at once.

{{< image src="fig-region-node.svg" title="What runs at each site." >}}

Each region is a pg_auto_failover group of its own, a monitor and three nodes, and the two regions of the figure are the same group mirrored. The primary of `us-east` publishes `pub_us_east`, runs the subscription `sub_us_east_from_us_west` that reads `us-west`, and holds the failover-enabled slot `sub_us_west_from_us_east` that `us-west` reads. Both standbys keep a synced copy of that slot, and have the subscription in their catalogs without a worker. `us-west` is the same, with the roles reversed.

Two things have to be set up once, and neither is automatic. The
subscription that reads from a group is created with `failover = true`,
so that the slot it creates on the primary is the kind the standbys
synchronize, and its connection string lists every node of the group
with `target_session_attrs=read-write`, so it finds whichever one is the
primary. The subscription the group itself runs from the other region
needs nothing: it lives in the catalogs, which physical replication
carries to the standbys, and a subscription's apply worker starts on a
node only while that node is a primary.

{{< image src="fig-region-failover.svg" title="One region fails over, with logical replication to a second region in both directions." >}}

The primary is killed; the monitor elects and promotes a standby; that node's own subscription resumes from its replication origin at once, and the other region's subscription reconnects to the new primary as soon as it accepts writes, and resumes from the synced slot.

**Verified**, by running it: profile `f` in `compose/` is two regions,
`us-east` and `us-west`, each a group of its own with its own monitor
and three nodes, each owning the rows of its own region key, linked in
both directions by native logical replication, with a writer on each
side. The primary of one region is killed under load, and the test
waits until both sides hold the same rows. Three runs killing
`us-east-a` and three killing `us-west-a`, with the same result every
time:

- **Nothing lost, nothing duplicated, in either direction.** In
  one of the runs with `us-east-a` killed, `us-east` acknowledged 1,532
  rows and `us-west` 2,552, and afterwards both regions hold exactly those
  rows. The other five runs end the same way: every acknowledged row is
  on both sides, none is missing, none is twice. The only apply error is
  one counted on the subscription that lost its connection.
- **The two directions come back at different times, and which one is
  which follows the dead region.** The failed region's own subscription
  resumes 21 to 23 seconds after the kill, at the position its
  replication origin recorded, as soon as a standby is promoted. The
  other region's subscription cannot connect until the new primary
  accepts writes, 52 to 54 seconds in, and its worker finds that by
  retrying every five seconds: 55 seconds without new rows from the
  failed region in all six runs, against 21 to 23 the other way.
- **The slot did its job.** The standbys held a synced copy of the
  failover slot before the kill, and the other region's subscription
  resumed on the new primary (`us-east-b` or `us-west-b` or `-c`,
  whichever the election picked) from it, without a resync.

This was run with pg_auto_failover's default synchronous replication, where
a commit on the primary waits for a standby. The case the test does not
cover is a group whose standbys are behind when the primary dies, for the
subscription the group runs from the other region: that is the case where
the other side's slot can be ahead of what the promoted node has, and it
is the reason to keep the synchronous setting for such a group.

### Geo-replication is the same mechanism, read for latency instead of correctness

Move the standby to another region and nothing above changes — the gate
is about flushed LSNs, not distance. What does change is the one number
the correctness question doesn't ask about: how long a `synchronous_commit
= remote_apply` write waits for a cross-region round trip, versus how
much RPO an async standby gives back in exchange for not waiting at all.
That trade is the maintenance-window trade from Part 3's RTO/RPO section,
with a continent's worth of latency standing in for the window.

Profile `b` measures it: a primary and a physical standby
(`synchronous_standby_names = 'ANY 1 (standby)'`), a round-trip time added
between them with `tc netem`, and `pgbench` running single-row insert
transactions under each setting, with one client and with 32. The delay is
emulated on one machine, so there is no jitter and no loss; what it shows
is the arithmetic of a commit that waits for a round trip.

| RTT | `off` | `local` | `remote_write` | `on` | `remote_apply` |
|---|---|---|---|---|---|
| 0 ms | 29,475 tps | 17,075 | 610 | 3,795 | 5,811 |
| 20 ms | 29,661 | 17,266 | 45 | 44 | 45 |
| 70 ms | 28,630 | 17,856 | 13 | 13 | 12 |
| 150 ms | 27,282 | 18,467 | 6 | 6 | 6 |

One client, transactions a second. At 20 ms and beyond, the three
synchronous settings are the same number, one commit per round trip, and
the average commit takes the round trip (22 ms, 73 ms, 152 ms). With 32
clients the commits overlap, and the throughput rises to about 1,450 tps
at 20 ms and 205 at 150 ms: more clients buy back throughput, never
latency. `off` and `local` do not move with the distance; they are also
the two that can lose the last commits when the primary dies, which is
the RPO side of the trade. The 0 ms row is not a latency result: with no
delay the numbers measure the standby's apply speed, and `remote_write`
is slower than `on` there for no reason the setting explains.

### What a real `pg_autoctl create proxy` would still need

Everything above is still a hand-run script wired to existing
primitives, not an orchestrated component of its own. A logical consumer
is still not a node: every existing topology routine, from
`synchronous_standby_names()` to `formation_uri()`, iterates the node
table and would miscount one as a standby if it were registered as such,
and there is no state machine the monitor drives the way it drives the
keeper's. That is real pg_auto_failover development, not a config change:
the project's `docs/orchestrated-operations.rst` names roughly this shape,
a generalized monitor/client primitive for a whole-cluster, multi-step,
resumable operation, and calls it a design exploration, not an
implementation. That component is named here, scoped, and left where
[Part 3](/blog/2026/10/zero-downtime-postgres-upgrades-with-logical-replication/#what-is-still-on-you)
left whole-cluster orchestration: on the roadmap, not shipped.

That roadmap is public, and it is where this component belongs.
pg_auto_failover's [roadmap](https://oss.theartofpostgresql.com/roadmap/)
lists first-class logical replication nodes in the state machine,
cascading replication for multi-data-centre topologies, which is what
the `region` label is for, and disaster recovery with archiving built
in. The project is maintained by one person, and its maintenance is
funded by the teams that run it: the
[pg_auto_failover funding campaign](https://oss.theartofpostgresql.com/pg_auto_failover/)
is how that work gets paid for.

---

## Conflict, avoided before it's resolved

Every architecture in this series up to now worked by making conflicts
impossible by construction — disjoint key spaces in Part 1, one schema
per source in Part 2, one writer at a time in Part 3. The map at the top
of this post is the same discipline at a coarser grain, and it is worth
building deliberately before reaching for anything that resolves a
conflict after the fact.

### The architecture that makes the question moot

One Postgres primary per region — a continent is a reasonable unit,
narrower if compliance needs it narrower, narrower still if latency
does — with the region itself part of every table's key, the same
`(worker_id, event_id)` discipline Part 1 used for workers. The map
draws four: `us-east` in New York, `us-west` in San Francisco,
`ap-south` in Mumbai, `ap-southeast` in Sydney. No row is ever a candidate for two of them to
write, because the key says which one owns it, so there is no conflict
left for anything downstream to detect, let alone resolve. Every site on
the map, the warehouse included, is drawn as the three overlapping
circles the legend calls "Primary + 2 standbys", next to a small marker
for its monitor: the Part 3 architecture in full, a pg_auto_failover
group of its own with two standbys and its own failover story, not a
single server with a single point of failure. The groups are
independent on purpose. They are connected by logical replication, not
meant as one failover unit, so each region has its own monitor, and a
failover in one never involves another.
The failover-slot mechanism two sections up is what keeps a CDC
consumer attached to one of these groups correct across a promotion;
nothing about having four of them, or a warehouse-side group with no
region of its own, changes that story per site, it just runs it four
times over instead of once. Replication between sites is the one job
Part 2 already built: an asynchronous, read-only roll-up, run in both
directions at once. New York and Mumbai each link directly to one
global warehouse in Amsterdam; San Francisco and Sydney link one hop
further out, through New York and Mumbai, instead of each opening its
own long-haul connection across an ocean neither needs a second one of.

What moves on every one of those links is the same shape: a region's
own writes go one way, a read-only mirror of everyone else's data comes
back the other way. New York's mirror from Amsterdam excludes
`region in ('us-east', 'us-west')` — it already has both, one from its
own writes, one cascaded up from San Francisco a moment earlier — and
holds only `'ap-south'` and `'ap-southeast'`; Mumbai's excludes
`('ap-south', 'ap-southeast')` the same
way, holding only the two American regions. Amsterdam never originates
a write of its own; every row it holds arrived from one of the four
primaries first, and what it sends back out is filtered, never new.
Nothing a region already owns is ever re-applied to itself, so nothing
here is a write path back in the sense that would matter — it is the
identical one-way, read-only flow Part 2 built, running in both
directions on the same connection instead of one. Getting the writing
half right costs exactly what Part 1's worker-key discipline cost: a
wider key, and a rule about where a row is allowed to be written,
enforced once, at the application layer, instead of resolved forever at
the replication layer.

The map's other symbol is the same discipline at its point of contact
with a person. Every site has a front desk, and every desk is
read-write against its own region's primary and read-only against the
mirror of everywhere else — four symmetric desks, not a tiered set.
Amsterdam's own desk is read-only, full stop, for the one reason it's
the one site with no region of its own: nothing is allowed to
originate there, so there is nothing for a desk to write. None of this
is a new mechanism — a desk is just an application connecting to
Postgres, same as a terminal in Part 1 or Part 2 — the only thing worth
drawing is which half of each connection is read-write and which is
read-only, because that split is the whole architecture in miniature.

### Cascading, one hop downstream

San Francisco and Sydney are full primaries, not read-only leaves —
`us-west` and `ap-southeast` are real, independently written regions,
exactly like `us-east` and `ap-south`. What makes them different from New York and
Mumbai is routing, not role: instead of opening a fifth and sixth
long-haul connection into Amsterdam, their own writes cascade one hop
through New York and Mumbai first, and the combined view — the hub's
own writes, its spoke's writes alongside them, and Amsterdam's mirror
of the other two regions — cascades back down that same link. One
expensive intercontinental connection per hub instead of one per spoke;
the short local hop between two cities a few hundred kilometers apart
absorbs the extra traffic for free.

That cascading itself — a subscriber re-publishing what it received to
a further subscriber — is not a new capability. **Verified**: a subscriber re-publishing
what it received to a further subscriber has worked since logical
replication shipped in Postgres 10 — the documentation has said so,
unchanged, across every release since
(`doc/src/sgml/logical-replication.sgml`: "Subscribers pull data from
the publications they subscribe to and may subsequently re-publish
data to allow cascading replication"). Nothing in `pgoutput`'s
origin-filter callback ever blocked it by default; through Postgres 15
that callback was a stub returning `false` unconditionally — forward
everything, no exceptions.

What Postgres 16 added is not cascading itself, it is a way to stop it
on purpose. `CREATE SUBSCRIPTION ... WITH (origin = none)` tells the
upstream node to send only rows that originated there, not ones it is
itself forwarding from further upstream — the opposite of the default,
`origin = any`, which forwards regardless of where a row has already
been. **Verified**, `pgoutput_origin_filter` in
`src/backend/replication/pgoutput/pgoutput.c`: with `origin = none`,
any row whose WAL record carries a non-invalid origin id is filtered
out before it reaches that subscriber. The id comes from
`replorigin_session_origin`, which the apply worker sets before writing
anything it received, so a row is tagged the moment it has crossed one
hop, and the next hop can choose not to forward it any further.

The documentation alone does not show this: for a disjoint,
region-keyed schema like this one, `origin` is not the tool that closes
the loop — row-filtered publications are, and once they are in place,
`origin` has nothing left to do.

The naive version would give San Francisco an unfiltered subscription
to everything New York publishes. New York's publication carries
`us-east`'s own writes plus Amsterdam's mirror of `ap-south` and `ap-southeast`, which
San Francisco genuinely needs — the whole point of a four-way mirror is
that a hop away from the primary still sees the rest of the world — but
it would also carry San Francisco's own writes reflected back the
moment San Francisco's own publication toward New York is unfiltered
too. `origin = none` looks like the fix: it worked cleanly one hop up,
in the two-node `pgoutput`/`pgoutput_origin_filter` case `origin`
was designed for. It does not work here, because `origin` is a binary
— `none` or `any`, not a per-origin allow list — and `none` on San
Francisco's subscription would drop Amsterdam's `ap-south`/`ap-southeast` mirror along
with the loop, since both arrive at San Francisco carrying *some*
non-invalid origin once New York's apply worker has touched them. There
is no built-in way to ask for "everything except what I sent you."

What actually closes the loop, cleanly, is the same tool already doing
the conflict-avoidance work: a **row-filtered publication** on the
`region` column itself. New York publishes two different views instead
of one — `WHERE (region = 'us-east' OR region = 'us-west')` toward
Amsterdam, and `WHERE (region <> 'us-west')` toward San Francisco —
and San Francisco's own publication back toward New York is
`WHERE (region = 'us-west')`, nothing else, ever. An unfiltered
publication toward Amsterdam would send Amsterdam's own mirror straight
back to it through New York. Neither filter cares
what hop a row arrived on or what it did or didn't already touch; each
one only looks at a value every row already carries. With that in
place, a row can never reach a link that would send it back to where it
already lives, by construction, the identical guarantee the primary-key
discipline already gives the write side. `origin = none` stays real and
useful — it is still the right tool the day a `region` column is not
the thing a loop needs to be cut on — but it is not what this
particular architecture needs.

What cascading does not change is what was already true one hop up:
DDL does not replicate, at any hop, for the same reason Part 2 found at
the first one, and neither do sequence values as the writes happen. Postgres
19 adds `ALTER SUBSCRIPTION ... REFRESH SEQUENCES`, which copies the
current values on demand from a publisher running 19 or later; nothing
streams them.

**Verified**, by running it: profile `c` in `compose/` builds exactly
this topology on Postgres 17 — four primaries, two of them cascading
through the other two, one warehouse, every mirror filtered — and drives
2,000 concurrent writes into each region. Without the filters the writes echo
around the loop; with them in place, the run ends with
8,000 rows on every node, exactly 2,000 per region, and zero
`apply_error_count` or `sync_error_count` on any subscription. (The
per-conflict-type counters, `confl_*`, only exist from Postgres 18, so
this run reads the two error counters Postgres 17 has.)

Real deployments covering APAC rarely stop at one extra region.
Singapore, Tokyo, and Hong Kong each show up as their own region in
every major cloud's catalog, for a geographic reason as much as a
commercial one — Mumbai to Sydney is already something like 9,500 km,
and Tokyo to Sydney is not much shorter, so a single APAC hub cascading
every city in the region through itself buys consistency at the cost of
exactly the latency this architecture exists to avoid; past a certain
distance a spoke earns its own direct link to the warehouse instead.
China is its own case regardless: the major clouds' China regions run
under separate local operators, not as another node on the same global
account, which makes it a business and compliance decision before it
is a replication one. A real build-out would reach for several of
these; this post draws two cascades, New York to San Francisco and
Mumbai to Sydney, because the mechanism — one hop, same row-filter
discipline each direction — does not change with a third or a fifth.
Draw it once, repeat it per city.

### When two writers genuinely touch the same row

Not every architecture can be partitioned this way — a shared
reference table that every region edits is the honest counterexample —
and that is where Parts 1 and 3's own findings become the starting
point instead of a footnote. Core **detects and counts** conflicts
(Postgres 18's `pg_stat_subscription_stats` columns, Part 1) and can
**skip** one (Postgres 15's `ALTER SUBSCRIPTION ... SKIP`). It does not
resolve one. pglogical's five policies — `error`, `apply_remote`,
`keep_local`, `last_update_wins`, `first_update_wins` — are the floor,
re-tested here against genuinely concurrent writers on both sides
instead of the one-writer-per-table discipline the rest of the series
designed around on purpose:

**Verified**, by running it: profile `e` in `compose/` is pglogical 2.4.8
(the PGDG package) on Postgres 18.6, the same pglogical release Part 1
used on Postgres 14.

- **The partitioned-parent crash is unchanged.** Replicating into a
  partitioned parent copies the initial rows, then the first replicated
  `INSERT` kills the apply worker with `signal 11`; the postmaster
  restarts every backend, and the subscription crash-loops — about 900
  crashes in the 30 seconds before the demo drops it.
- **The filter move is unchanged.** Moving a customer from worker 1's
  row filter to worker 2's reaches neither: worker 1 keeps the stale
  row, worker 2 never gets it, and the hub has the new value.
- **`keep_local` diverges for real under concurrent writers.** Two
  nodes subscribed to each other, four clients each updating the same
  1,000 keys at 2,000 transactions per second per node for 15 seconds
  (about 60,000 updates), three runs of each policy:

| Policy | Rows that differ afterwards, of 1,000 (3 runs) | Replication |
|---|---|---|
| `last_update_wins` | 0, 0, 0 | healthy |
| `apply_remote` | 1, 0, 1 | healthy |
| `keep_local` | 1000, 1000, 1000 | healthy, no error |
| `first_update_wins` | 1000, 1000, 997 | healthy, no error |
| `error` | 1000, 1000, 1000 | both subscriptions `down` |

`last_update_wins` converged every time, though it keeps one row, not
both: of the roughly 60,000 increments, 8, 12 and 22 were overwritten by
a concurrent update to the same row. `apply_remote` usually converges
and sometimes leaves one row different on the two nodes. `keep_local`
and `first_update_wins` do what they say on the tin — each node keeps
its own value — and nothing in the logs says the nodes now disagree.
`error` stops replicating until someone intervenes.

The sequence gap
[Part 1](/blog/2026/09/ten-years-of-postgres-logical-replication/#minting-ids-on-the-workers)
was left open as "not yet committed"; it now has a firmer answer.
**Verified** against the commitfest entry itself:
Michael Paquier's "Sequence Access Methods, round two"
([commitfest #4677](https://commitfest.postgresql.org/52/4677/)) was
returned with feedback in PG19-Final on 2026-02-27, after more than two
years moving between commitfests without landing. It will not be in
Postgres 19. `galloc` and `snowflakeid` remain EDB Postgres Distributed
features, not core ones, and stay that way for at least one more
release.

What Postgres 19 did commit is a smaller thing, sequence synchronization:
a publication `FOR ALL SEQUENCES` and `ALTER SUBSCRIPTION ... REFRESH
SEQUENCES` copy the current values to the subscriber when asked. That
helps a cutover. It does not let two writers draw from one sequence.

Where does real multi-master conflict handling live today? In BDR and
its successor, EDB Postgres Distributed: BDR 1.x was open source and
reached its end of life in December 2019; every version since is
commercial. It is a reference point for this section, the same way RDS
and Aurora are in Part 3, and nothing in this series builds against it.

---

## Schema changes across regions

Parts 1 and 2 met DDL as a single stream's problem: a table changes on
the publisher, and the subscriber has to change with it, at the right
point in the stream. Five groups linked by eight subscriptions, two pairs
of them in both directions, make it a coordination problem: whoever
changes a table changes it in every region, while the writers keep
writing. Inside a group there is nothing to do, because the standbys get
the change through physical replication (the demo checks: the column
added on `us-east-a` is on `us-east-b` with no command of ours). Between
groups, here is what each tool does, measured on `us-east` and `us-west`
as two groups linked as in profile `f`.

### Native logical replication: the schema is yours

Postgres does not replicate DDL. The apply worker checks every column of
an incoming row against the local table and fails if one is missing; an
extra local column is fine. So a column added on the publisher first
stops the subscriber at the next row, with this error, and the worker
retries until the column exists:

```
ERROR:  logical replication target relation "public.events" is missing replicated column: "note_a"
```

Profile `g` (`./run-g.sh core`) runs `ADD COLUMN` three ways with a writer
in each region, three runs each, nothing lost in any of them:

| Rollout | Apply errors | Effect |
|---|---|---|
| `us-east` first, `us-west` ten seconds later | 4 on `us-west`, every run | `us-west` is 90 to 178 rows behind after 10 s and caught up 15 to 16 s after the first `ALTER` |
| both primaries at the same moment (3 attempts per run) | 0 in 8 of 9, 2 on `us-east` in the other | none visible at this write rate, except for that one retry |
| writes paused, drained, both `ALTER`ed, writes resumed | 0 | writers blocked 0.9 to 1.5 s |

The second line is luck, not design: the window is a few milliseconds, and
a busier writer would find it. The first line is the honest default for
an operator who does one region and then the next. Adding a column wants
the subscriber changed first; dropping one wants the publisher changed
first; and on a link that runs in both directions each region is both,
so no order is safe, only a short pause is. That is also why a rename
cannot be a rolling change here: expand, copy, switch, contract, each
step additive or removing something nobody reads.

### pgcopydb: DDL carried in the stream

The design from the previous section applies: with the event trigger
installed, a schema change is a message in the same commit-ordered stream
as the rows, so it cannot arrive before or after them. `./run-g.sh
pgcopydb` replaces the native subscription from `us-east` to `us-west`
with `pgcopydb clone --follow` (a one-way link; `clone` also created the
table on `us-west` itself), with a writer inserting about 29 rows a second
in single-row transactions, and runs three changes on `us-east` with
nothing run on `us-west`. Three runs, on pgcopydb `main` as of late October
2026:

- It keeps up: 28 to 29 rows a second written, the same arriving, a
  largest lag of 0 rows.
- `ADD COLUMN`, `RENAME COLUMN` and `DROP COLUMN` each reached `us-west`
  within one or two seconds, with at most one row of lag, writer running.
  The rename is the change that no order makes safe on the native link.
  The row inserted with the new column lands with it.

Three things keep it from replacing the native link in this scenario:

- **Its slot is not a failover slot by default.** Without
  `--enable-failover`, pgcopydb issues
  `CREATE_REPLICATION_SLOT ... LOGICAL` without the `FAILOVER` option.
  After `us-east-a` is killed, the new primary has no such slot and the
  stream cannot be resumed (`replication slot "pgcopydb" does not
  exist`): the rows written afterwards never reach `us-west`, and
  re-creating the link needs a new slot and a new clone.
- **It does not take a multi-host URI.** A `postgres://a:5432,b:5432,...`
  string makes its replication connection fail with `could not match 3 port
  numbers to 5 hosts`; the keyword form
  `host=a,b,c port=5432 ... target_session_attrs=read-write` works and is
  what the demo uses.
- **It filters by table, not by row.** The `where (region = ...)`
  publications that keep the bidirectional links from echoing have no
  equivalent, so it can carry one-way links only, such as the warehouse
  mirror, not the pairs.

The first is the one that matters, and the fix is small. Postgres 17
accepts `FAILOVER` on that same command, but only in the parenthesized
option syntax (`CREATE_REPLICATION_SLOT "pgcopydb" LOGICAL "pgoutput"
(FAILOVER true)`), which the bare form older pgcopydb versions sent cannot
carry. pgcopydb `main` has an `--enable-failover` option
([#1073](https://github.com/dimitri/pgcopydb/pull/1073)) that sends it on
Postgres 17 and later. The same change makes `follow` restart its
sub-processes when one fails (the receive process already reconnects by
itself, the apply process did not), at most `--max-restarts` times in
`--restart-window` seconds, Erlang supervisor style, because a failover of
the target is as ordinary as one of the source. With both, `./run-g.sh
pgcopydb-failover` shows the slot as `failover=true` on the primary and
`synced=true` on both standbys before the kill; after the kill the new
primary has it, and `us-west` ends with every row `us-east` acknowledged,
with nothing left to catch up one second after the writer stops (three
runs). Without the flag, the same kill leaves `us-west` 1,234 to 1,339 rows
behind: the restarts find no slot, and after five attempts in 300 seconds
`follow` logs `giving up` and exits. Killing the primary of the target region instead costs one restart of the apply
process, which comes back by itself, and no row is missing (two runs). The standbys still need
`sync_replication_slots` and the primary `synchronized_standby_slots`, the
Postgres-side settings the first section covers.

### pglogical: `replicate_ddl_command`

pglogical does not replicate DDL either; its README says so, and offers
`pglogical.replicate_ddl_command()`, which runs the statement locally and
queues it for the subscribers of a replication set (`ddl_sql` by default),
in the stream. `./run-e.sh ddl` runs two PostgreSQL 18 nodes subscribed to
each other, both inserting, twice:

- A plain `ALTER TABLE ... ADD COLUMN` on `w1` only: `w2`'s apply worker
  fails on the first row (`unknown column name plain_col`), the
  subscription goes `down`, and 290 rows pile up in ten seconds. The same
  `ALTER` by hand on `w2` ten seconds later fixes it, but the worker is
  only back 187 seconds after the first `ALTER`, both runs: its restart
  delay grows with each failure. Nothing was lost.
- `replicate_ddl_command('alter table ... add column ...')` on `w1`:
  `w2` had the column with nothing run there, lag zero, both writers
  running. The same with `rename column`: renamed on `w2`, lag zero.
- pglogical's own README says this will not be enough once the
  subscriber also writes with the old structure, and that is the situation
  of two writers on one table. The test did not hit it, which says
  nothing about the window.
- The slot pglogical creates has `failover = false`. It issues
  `CREATE_REPLICATION_SLOT ... LOGICAL pglogical_output FAILOVER`, the
  2ndQuadrant syntax, and only on 2ndQuadrant's PostgreSQL 9.6 and 11
  builds; community Postgres 17 and later spell the option `(FAILOVER
  true)`, so a promotion loses the slot the way pgcopydb's is lost.

It can still be made a failover slot, without changing pglogical. The
slot is created once, when the subscription is, and nothing touches its
flag afterwards, so Postgres 17's `ALTER_REPLICATION_SLOT` does it from a
replication connection (`psql "dbname=app replication=database" -c
'ALTER_REPLICATION_SLOT "pgl_app_provider_sub" ( FAILOVER true )'`) while
the slot is inactive: disable the subscription with
`pglogical.alter_subscription_disable`, run the command, enable it again.
Creating the slot by hand first does not work: pglogical drops an inactive
slot of that name and creates its own. `./run-e.sh failover` runs it on a
provider with a physical standby (`sync_replication_slots = on`, the
provider's `synchronized_standby_slots` naming the standby's slot) and a
subscriber whose provider DSN lists both hosts with
`target_session_attrs=read-write`:

- the slot flips to `failover=true`, and the standby has a `synced=true`
  copy within seconds;
- the provider is killed under a writer and the standby promoted; the
  apply worker reconnects to it by itself and the 200 rows written on the
  new provider reach the subscriber one second after the last insert, with
  no row missing (`results/e-failover-1.out`);
- the same run without the `ALTER_REPLICATION_SLOT` step has no slot on the
  standby, the worker dies on `START_REPLICATION SLOT ... does not exist`,
  the subscription stays `down`, and the 200 rows never arrive
  (`results/e-failover-0.out`).

One trap that is the demo's, not pglogical's: the promoted standby inherits
`synchronized_standby_slots = 'e_s1_slot'`, a slot it does not have, and
logical decoding then waits for it forever. The script resets it by hand;
in the post's setup, pg_auto_failover computes that list. One run of each.

### What the three add up to

| | Core | pglogical | pgcopydb |
|---|---|---|---|
| Schema change in the stream, in order with the rows | no | yes, through `replicate_ddl_command` | yes, with the event trigger |
| Survives a rename with writers on both sides | no | works, the README warns of the window | one-way link only |
| Row filters | yes | yes (`row_filter`) | no (tables only) |
| Failover-enabled slot | yes | not as shipped; `ALTER_REPLICATION_SLOT` after creation does it | with `--enable-failover` |
| Link in both directions | yes | yes | no |

None of the three has every column of that table. The combination the
scenario needs (native filters and failover slots, with the schema change in
the stream) is not available off the shelf today. What works today is the
discipline of the native column: additive, nullable changes, applied
everywhere with a short pause, and the rest as expand and contract. Speed,
first copy and version support come next, side by side.

## Core, pglogical and pgcopydb side by side

The schema section compared the three tools on one axis. The choice between
them has several, and they pull in different directions: where the code
runs, how fast it applies changes, what it does with the first copy of a
large database, whether it carries DDL, and which Postgres versions it can
connect.

### Three architectures

{{< image src="fig-deployment.svg" title="Where the replication code runs: inside the servers, or between them." >}}

**Core logical replication** is part of Postgres. The publisher's walsender
decodes the WAL with the built-in `pgoutput` plugin and streams the changes.
On the subscriber, an apply worker, a background process of the server
itself, applies each change as it arrives, in one local transaction per
source transaction, and commits when the COMMIT message comes in. Nothing is
stored in between (`replication/logical/worker.c`). The target is the one
who pulls: it opens the connection to the source.

**pglogical** has the same shape, as an extension that has to be installed
on both servers (`shared_preload_libraries`, one build per major version).
It brings its own output plugin and its own apply workers. They write to the
heap directly, turn runs of consecutive inserts into one `heap_multi_insert`
call, and commit asynchronously by default (`pglogical_apply.c`, settings
`pglogical.batch_inserts` and `pglogical.synchronous_commit`).

**pgcopydb** is another kind of tool, and logical replication is not its
main use case. Its documentation puts it as "migration to a new Postgres
system, either for new hardware, new architecture, or new Postgres major
version": `pg_dump -jN | pg_restore -jN` between two running servers, with
the parallelism of both and the streaming that the two programs cannot do
together. It takes one snapshot, copies the tables and builds the indexes in
parallel, and, to keep the cutover short, follows the changes of the source
from a slot created at that same snapshot, which it reads all the time the
copy runs: the source can forget the WAL as soon as it is spooled on the
pgcopydb host, however long the copy takes. That last part is logical
replication, used as the way to catch up. It is a separate program, run on
any host that can reach both servers, and it needs nothing installed on
either. It reads the slot, writes every change to a SQLite file, transforms
the changes into SQL statements kept in a second SQLite file, and replays
those on the target through ordinary connections with prepared statements.
SQL over plain connections works against any server, managed or not, at any
version, which is the reach a migration tool needs. The price is that every
change is written twice and read twice before the target sees it.

### Order of magnitude

The same table on two Postgres 18 servers on one machine, over a one-way link, with a trigger on the target that stamps the time a row is
applied. `./run-e.sh bench <method>` runs four workloads: 1,000 single-row
transactions 50 ms apart (the stamp minus the source's `clock_timestamp()` is
the latency), eight concurrent writers for 30 seconds, one transaction of a
million rows, and one `UPDATE` statement of a million rows.

| | Core | pglogical | pgcopydb |
|---|---|---|---|
| Latency of a single-row transaction, p50 / p99 | 0.3 / 1.7 ms | 0.2 / 2.0 ms | 0.6 / 4.2 ms |
| Rows a second under 8 writers (the source commits 85,000) | 80,000 | 84,000 | 8,000 to 10,000 while loaded, 17,000 to 18,000 with the catch-up |
| Lag once the 30 s load stops | 2 s | 0.2 s | 75 to 110 s |
| One transaction of 1M rows, seconds after its commit | 4 to 6 | 3 to 6 | 7 to 9 |
| One `UPDATE` of 1M rows that changes nothing, `REPLICA IDENTITY FULL` | 3 to 5 s | 4 s | 9 to 18 s |

The source commits about 85,000 single-row transactions a second in the
third workload, whichever tool is behind it. Core and pglogical keep up with
it. pgcopydb applies 8,000 to 18,000 rows a second and the rest queues: the
75 to 110 seconds it needs to catch up after a 30 second load are a backlog,
not a stall. On an idle link a change reaches the target in a fraction of a
millisecond with the two in-server tools, and in a few milliseconds with
pgcopydb. A single large transaction narrows the gap to about a factor of
two, because the three wait first for the same thing: the source decodes a
transaction when it commits, and needs a second or two to hand out the last
change. The `UPDATE` changes no value, which pgcopydb recognises when it has
the whole old row (`REPLICA IDENTITY FULL`) and sends nothing for, where core
and pglogical apply every row; it still has to receive and read back a million
messages, which is what the 9 to 18 seconds are. pglogical took 66 seconds
for that statement in one run, after the throughput workload had left 2.5
million rows in the table, and 4 seconds in the two runs on a clean table;
that was not investigated.

Read these as orders of magnitude: one machine, a handful of runs, and the
runs of one tool differ by a factor of two on the large transactions. "About
the same", "a few times slower" and "ten times slower" are the claims, not the
digits. For a migration, 10,000 rows a second is more than most databases
write; for a replica behind a source that takes tens of thousands of writes a
second, it is not enough.

One limit the table does not show is memory. pgcopydb builds a whole
transaction in memory before it writes it down: its apply process peaked at
340 MB for the transaction of one million inserted rows, and at 1.4 GB for the
`UPDATE` of one million rows with their whole old rows, about 1.4 kB a row.
Core and pglogical apply a change as it arrives, and a million rows cost them
no more memory than one. A transaction a few times that size gets the process
killed for lack of memory, and `follow` restarts it up to `--max-restarts`
times before it gives up.

### What each one is for

- **Core logical replication** is the default when both servers run
  Postgres 10 or later and you may configure them: the lowest latency, the
  highest throughput, nothing to install, and the only one of the three that
  is part of the server's own release process. It is what Parts 2 and 3 and
  the region links of this post use. Its gaps are the ones listed above: no
  DDL, no help with a first copy that is large or interrupted, and some
  features need a recent version on both sides.
- **pglogical** is for what core cannot do: servers older than 10 down to
  9.4, per-table replication sets, five conflict policies when two nodes
  write the same row, and a `replicate_ddl_command()` that puts a schema
  change in the stream. It needs the extension on both servers, so it is out
  on hosting that does not offer it. Part 1's worker hub is built on it.
- **pgcopydb** is for migrating a database, in particular a large one, to a
  server that cannot be a physical copy of the first: another CPU
  architecture or operating system, another build of Postgres, another major
  version, a hosted service. It is dump and restore with the parallelism,
  the resume and the filtering that `pg_dump | pg_restore` lack, and then a
  short cutover: the changes captured from the first second, DDL carried in
  the stream, and nothing installed on either side. Part 3's upgrade is
  built on it. It is not the tool to leave running behind a source that takes
  tens of thousands of writes a second: the table above says why.

### What each supports

| | Core | pglogical | pgcopydb |
|---|---|---|---|
| Installed on | nothing | an extension on the source and on the target | nothing on the servers; one program on any host |
| Postgres versions | 10 and later; source and target may be different major versions | 9.4 and later on both, one build per major version | source 9.6 and later (`wal2json` or `test_decoding` before 10, `pgoutput` from 10); target: a server `pg_restore` can load; `pg_dump` must be at least as recent as the source |
| DDL | not replicated | `replicate_ddl_command()`, called by hand, in the stream | an event trigger emits it as a message, in the stream, once `pgcopydb stream event-trigger` has installed it |
| First copy | one table synchronization worker per table (`max_sync_workers_per_subscription`, 2 by default): each table is one `COPY` | structure with `pg_dump` and `pg_restore`, then one `COPY` per table through sync workers | `--table-jobs` tables at once, a large table split across several `COPY` streams (`--split-tables-larger-than`), indexes built in parallel |
| A long, slow link | a broken connection starts the table over | the same | many streams to fill the link; `--resume` does not resend the tables already copied (the one that was in flight starts over) |
| WAL kept on the source during the copy | from the sync slot of each table, until that table is copied and caught up | from the sync slot, for the whole data copy | what receive has not yet spooled (on `main`; 0.18 kept it all until apply had caught up) |
| Memory for a large transaction | constant: a change is applied as it arrives | constant | proportional to the transaction: 340 MB for a million inserted rows, 1.4 GB for a million updated rows with their old rows |
| Sequences | values copied on demand from Postgres 19 (`REFRESH SEQUENCES`) | `replication_set_add_sequence` | copied at the end of the clone |
| Row filters, columns | yes (15 and later) | yes | tables only |
| Both directions | yes (`origin = none`, 16 and later) | yes | no |
| Conflict handling | detects and counts (18), can skip | five policies | none |
| Failover slot | yes (17 and later) | not as shipped | `--enable-failover` |

Read the first-copy rows with the network in mind. A single TCP stream rarely
fills a long, slow link, and a copy that breaks after nine hours of a ten
hour table is a bad evening. Core and pglogical copy one table over one
stream and begin that table again if the connection drops; pgcopydb opens as
many as you allow, can split one table across several of them (on an integer
key, or on `ctid` when there is none), and remembers which tables are done.
None of the three has a compression option; that is the network's job. These
rows come from the documentation and the source of each tool; the lab
measures speed, not copy times.

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
copied and has caught up, and the snapshot of the copy holds back vacuum on
the publisher for as long as the `COPY` runs. The source comment says why it
is done per table: the synchronization "does not have to hold the xid and LSN
for the time it takes to copy data of all tables". The subscription's own
slot keeps being consumed meanwhile, the apply worker skipping the changes of
the tables still being copied. The bound is one table's copy, times the
workers running at once, and for a table of several terabytes that is the
whole copy.

**pglogical** takes one slot and one exported snapshot for the whole
subscription (`pglogical_sync_subscription`): the structure with
`pg_dump --snapshot`, then the data of the tables of the replication sets,
then the constraints, and only then the catch-up from the slot. The source
keeps the WAL and the snapshot for all of it.

**pgcopydb** takes a snapshot and creates its slot at the same point, copies
with several workers, and receives the changes into SQLite at the same time.
The position it tells the source it has flushed is the one receive has
committed and synced to its SQLite file, so the source can recycle the WAL
while the clone runs, and the changes wait in files on the pgcopydb host
instead of in the source's `pg_wal`. That is how it works on `main`
([#1077](https://github.com/dimitri/pgcopydb/pull/1077)). Version 0.18
reported the position apply had committed on the target instead
(`stream_sync_sentinel` in `ld_stream.c`), and apply is only enabled when the
clone is done, data, indexes and post-data included (`cli_clone_follow.c`):
the source kept the WAL since the slot was created for the whole clone, and
until apply had caught up. What the concurrent capture buys on top is that
the changes are already decoded when the copy ends, so the catch-up is a local
replay, where core has the source decode that backlog after the copy.

**Verified**, by running it: `./run-e.sh walretention <core|pgcopydb>` copies
a table of 600,000 rows (261 MB, with three expression indexes) while a
writer produces 3 to 4 MB of WAL a second on the source, and samples how far
behind the current WAL position each slot's `restart_lsn` is, every two
seconds. The sizes are small, the arithmetic is not: at 3 MB a second, a ten
hour copy keeps about 100 GB.

| | Core | pgcopydb 0.18 | pgcopydb `main` |
|---|---|---|---|
| During the copy | the sync slot of the table: 52 MB after 2 s, 133 MB after 6 s | the slot: 122 MB after 2 s, rising | the slot: 127 MB after 2 s, 144 MB after 6 s |
| After the copy | the sync slot is gone; the subscription's slot sits between 0 and 90 MB | rising to 208 MB for 24 s, while apply replays what was stored, then 80 MB | between 79 and 232 MB, in steps |

A second run shows the edge. With the writer in another database of the same
server, which nothing decodes, core's slot still advances (between 0 and 100
MB, in steps of about fifteen seconds). pgcopydb 0.18's grew without end: 122
MB at the start, 647 MB after two minutes, and it was still growing, because
it reported apply's position, apply only moves it when it commits a
transaction of the database, and a quiet database on a busy server commits
none. On `main` it moves in steps too, between 76 and 293 MB. The sizes in a
real migration depend on the write rate of the whole server, not only of the
database being copied, so `max_slot_wal_keep_size` and a look at
`pg_replication_slots.wal_status` belong in the plan of any long copy with
any of the three: a slot that goes past the limit is invalidated, a core
table synchronization starts that table over, and a pgcopydb clone has to
start again.

### When the two servers cannot share a binary format

The tools above are compared on speed, and for pgcopydb that is not the
question it was made for. A physical copy of a cluster (a base backup, a
streaming standby, a copy of the files) moves pages and WAL as they are, so
the two servers have to agree on everything that shapes those bytes. At
startup the server compares its own settings to the ones recorded in
`pg_control` and refuses to run on a mismatch ("database files are
incompatible with server", `ReadControlFile()` in `access/transam/xlog.c`),
and `pg_upgrade` makes the same comparison between the old and the new
cluster (`check_control_data()` in `bin/pg_upgrade/controldata.c`). What has
to match:

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
- **Data checksums**, set by `initdb --data-checksums`. `pg_upgrade` refuses
  a mismatch in either direction.
- **The catalog version and the control file version**, which change with
  the major version: this is why a physical copy needs the same major version.

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

pgcopydb belongs to the last two rows, and its main use case is the second
to last: a dump and a restore, made fast, with the catch-up of logical
replication added at the end so that the cutover is short. The migrations it
is for are the ones where the first two rows are closed: from a server built
with a 32 kB block size, a custom segment size or an unusual `NAMEDATALEN` to
a default build, which is what a hosted service runs; from one CPU
architecture or operating system to another; to a server with data checksums
when the old one had none; or from a database nobody can install an extension
on. Logical replication ignores the layout too, and it can do the same moves
for the tables it carries; it leaves the first copy, the schema and the
sequences to you, and that is the part of the work pgcopydb packages.

## Finishing the CDC consumer: DDL as a message, not a table

[Part 2](/blog/2026/10/consolidating-databases-with-postgres-logical-replication/)
built the honest baseline — a queue table, an Event Trigger, a
replica-side trigger that replays what it finds — and named exactly
where it stops being a demo: "a component that merges and splits change
streams without writing tables... a
[story for another article](/blog/2026/10/consolidating-databases-with-postgres-logical-replication/#sequences-dont-replicate-either),"
and a *general* consumer that also decodes `pgoutput`'s binary row
changes, called
["a real project, not a demo"](/blog/2026/10/consolidating-databases-with-postgres-logical-replication/#what-pg_logical_emit_message-actually-sends)
and left unbuilt. This section is that project, scoped down to the part
that's actually missing.

### `pgcopydb follow` is most of the way there already

**Verified** by reading the source: `pgcopydb`'s own pgoutput decoder
(`ld_pgoutput.c`) correctly handles
`BEGIN`, `COMMIT`, relation caching, and every DML message type — it's
the same tool Part 3 built the whole upgrade path on, and that code
path is already correct. What it does not do, confirmed by reading the
switch statement itself, is anything with a Message frame:

```c
default:
{
    log_debug("pgoutput: unknown message type '%c' (0x%02x), filtering out",
              msgtype, (unsigned char) msgtype);
    metadata->filterOut = true;
    break;
}
```

A `pg_logical_emit_message()` call — the primitive Part 2 built its own
DDL replay on — arrived on the wire and was logged as unknown and
dropped. That was the entire gap before the change described below. The
machinery on the other side of it was already built and waiting:
`STREAM_ACTION_MESSAGE` was already in `pgcopydb`'s action enum, the
JSON round-trip for it was already wired, and the SQLite-backed storage
layer already had a generic insert path for it. Nothing downstream of
the decoder needed to change shape — it needed one new case in one
switch statement, and something to actually do once that case is
reached.

The gap is not a hypothetical one. On the `wal2json` plugin path, a
Message frame with no `schema` or `table` property (PeerDB's
`peerdb_heartbeat`, for one) used to be routed into the DML branch,
fail to parse as a row, and make apply retry the same transaction
forever. A CDC consumer cannot afford a frame that it neither handles
nor ignores.

{{< image src="fig-pgcopydb-message.svg" title="DDL as a message, not a queue table." >}}

An Event Trigger emits a JSON message, prefix pgcopydb, type ddl, body the command text. pgcopydb follow decodes the Message frame, filters on the prefix, parses the JSON, and dispatches on type — the ddl handler executes the body on the same connection the rest of the apply worker already uses, inside the same commit-ordered stream as every other change.

### The design

The design follows from a line-by-line read of both the pgcopydb source
and Postgres core's own wire format for the Message frame (`proto.c:
logicalrep_write_message`). A JSON envelope, `{"type": "ddl", "body":
"..."}`, with `type` as an open
registry — `ddl` is the only one defined so far, and the envelope
exists specifically so a second kind, a sequence-sync message being the
obvious next candidate, is a new `type` value and a new dispatch-table
entry, not a second wire convention. The source side reuses Part 2's own
Event Trigger unchanged, pointed at `pg_logical_emit_message` instead of
an insert; the apply side reuses the same connection and the same
commit-ordered stream every other change already goes through — no
second pipeline, no barrier to keep two readers in sync, the exact
complexity Part 2 weighed and rejected when it considered a second,
independent DDL-message slot.

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

This design is implemented in pgcopydb: the Message frame is decoded
and stored, dispatched by `type`, and a `pgcopydb stream event-trigger`
subcommand installs the trigger above instead of asking a reader to
write it by hand. It is on `main`, pending the next release. **Verified** by running it:
with the trigger installed mid-stream, an `ALTER TABLE ... ADD COLUMN`
on the source reaches the target two seconds later with zero manual
statements there, the rows updated afterward follow, and so does
ordinary traffic issued after it (`compose/results/d-ddl-message.out`).

Two edges follow from the source side being unchanged: `current_query()` captures a multi-statement command whole, not
split — the demo showed a multi-statement command landing in `replay.db`
as a single DDL message — and a replayed command runs with the apply worker's own role
and `search_path`, not necessarily the one the original session had.
Both are documentation, the same as they were in Part 2, not new code
to write.

### No pre-existing open-source tool already does this

PeerDB (AGPLv3) and Sequin both stream one Postgres source out to many sinks, with routing
and filtering on the way out — neither merges more than one Postgres
source into a single stream, and neither claims anything about
surviving a promotion. The actual prior art for the *merge* direction,
and for splitting one stream into several by predicate, is exactly
where this series' Part 1 started: PgQ and Londiste's tick-and-cascade
model, which already solved per-source ordering and already named what
it never guaranteed — a total order across merged sources, or atomicity
across one distributed transaction's pieces. A fuller proxy built on
that lineage — merge N sources, split by predicate, stay continuous
across the failover this post opened with — is real, scoped,
pg_auto_failover-shaped future work, and not built here.

---

## What BDR 1.x did about the same problems, on paper

BDR 1.0 was the open source multi-master of 2ndQuadrant: a patched
PostgreSQL 9.4 and a plugin, whose last commit on the 1.0 stable branch
is from November 2019. It is not maintained, and its successors are not
open source, so this is a comparison of designs, read from the
documentation in its repository
([`doc/`](https://github.com/2ndQuadrant/bdr/tree/bdr-plugin/REL1_0_STABLE/doc)),
and nothing here was run.

| | BDR 1.0, from its manual | This post |
|---|---|---|
| Topology | A full mesh: every node connects to every other, and a joining node configures all existing ones to connect to it ([node management](https://github.com/2ndQuadrant/bdr/blob/bdr-plugin/REL1_0_STABLE/doc/manual-node-management.sgml)). Five nodes are 20 directed links. | Five groups, eight subscriptions: hubs and cascades chosen by hand. |
| Conflicts | Optimistic. Row conflicts resolve to the most recent tuple by the originating host's timestamp, or a user handler; a primary-key update or an insert violating several unique constraints is "divergent" and needs an operator ([conflicts](https://github.com/2ndQuadrant/bdr/blob/bdr-plugin/REL1_0_STABLE/doc/manual-conflicts.sgml)). | Avoided: disjoint `region` keys; a conflict is an error to see, not a rule to apply. |
| DDL | Replicated automatically, under a global DDL lock: every node must be reachable and caught up, writes on every node pause or fail meanwhile, DDL is refused while the lock is held elsewhere, and a down node must be removed first and cannot come back ([DDL replication](https://github.com/2ndQuadrant/bdr/blob/bdr-plugin/REL1_0_STABLE/doc/manual-ddl-replication.sgml)). | No lock, and nothing automatic: the stall above unless the operator pauses writes, or the change travels in the stream. |
| What DDL may do | A list of refused statements, among them `ADD COLUMN ... DEFAULT` and anything that rewrites a table. | Whatever Postgres accepts; the replicas must keep up. |
| Sequences | Global sequences (`CREATE SEQUENCE ... USING bdr`), allocated in chunks by a vote of the majority, unusable on two nodes; the manual calls them deprecated and recommends step and offset sequences ([global sequences](https://github.com/2ndQuadrant/bdr/blob/bdr-plugin/REL1_0_STABLE/doc/manual-global-sequences.sgml)). | No shared sequence: the key is `(region, id)`. |
| What replicates | Replication sets per table; a table's DDL goes to all nodes whatever its sets ([replication sets](https://github.com/2ndQuadrant/bdr/blob/bdr-plugin/REL1_0_STABLE/doc/manual-replication-sets.sgml)). | Row filters per publication; DDL is outside them. |
| A node is down | One slot per peer holds WAL on every other node until the node is parted, with the disk-full outcome the manual warns about; a parted node cannot rejoin. | A node is a group of three; failover is pg_auto_failover's, and the slot follows through `failover = true`. |

Two readings follow. BDR 1.0 bought "every change reaches every node"
(rows, schema, keys) with a coordination cost this post's setup
refuses to pay: no global lock, no majority vote for a sequence, no
write pause on a continent because one node is slow. What it pays instead
is the operator's discipline in the table above. And what BDR 1.0 left
out is the problem the first half of this post is about: its manual
does not discuss the failover of a node's own server or what happens to
a peer's slot when that server's standby is promoted, because in 2019 on
PostgreSQL 9.4 the primitive did not exist. The slot-synchronization of
Postgres 17 is the piece BDR 1.x lacked, and `synchronized_standby_slots`
is the guard it would have needed.

---

## What this post adds to the ladder

Six things, and each one is additive to what Parts 1 through 3 already
measured, not a replacement for any of it:

- **A failover-safe slot** turns Part 3's "the publisher never moves"
  assumption into a named, sourced mechanism instead of an unstated
  one — and the real orchestration for it is scoped, not built.
- **Geography** turns out not to need a new mechanism, only the same
  one read for latency instead of correctness.
- **Avoidance before resolution** is this series' own founding
  discipline — disjoint key spaces — applied at the size of a
  continent instead of a worker, with real multi-master conflict
  handling named honestly as commercial, not core.
- **Schema changes** get measured on three tools instead of assumed: the
  native link leaves them to the operator, pgcopydb and pglogical carry
  them in the stream, and none of the three has row filters, failover
  slots and both directions at once.
- **A side-by-side of the three tools** puts numbers on what the
  architectures cost (latency of a few milliseconds against a fraction of
  one, ten thousand rows a second against eighty thousand) and says what
  each is for: core by default, pglogical for what core lacks, pgcopydb for
  migrating a large or old database.
- **A finished CDC consumer** closes the one gap Part 2 explicitly
  left open, and does it as a real change merged into a tool the series
  already relies on, not a bigger demo.

---

## Ten years of logical replication, and what is left

None of this was possible ten years ago, and most of it is other people's
work. Logical decoding arrived in 9.4 (Andres Freund, with 2ndQuadrant)
and gave the WAL a meaning outside the server. pglogical and BDR, the
work of Petr Jelinek, Craig Ringer and their colleagues, showed what it
was for, and the publications and subscriptions of Postgres 10 made it
part of core. The releases since have been the long tail of making it
usable for real systems: `TRUNCATE`, partitioned tables, streaming of
large in-progress transactions, two-phase commit, row filters and column
lists, parallel apply, origin filtering that makes a two-way link
possible at all, logical decoding on a standby, and then the pieces this
post leans on the most: failover slots and `synchronized_standby_slots`,
`pg_createsubscriber`, slots that survive `pg_upgrade`, the conflict
counters of Postgres 18. Every one of them went through review on the
mailing list, and a primary that can die without its change stream losing
or inventing a row, with the standbys following the slot by themselves, is
the result of years of exactly that.
[Part 1](/blog/2026/09/ten-years-of-postgres-logical-replication/) has the
long version of that history.

What is left is concrete:

- **DDL in the stream.** Core replicates none of it. pgcopydb and
  pglogical each carry part of the answer outside core, and a schema
  change that travels in the same commit order as the rows is the piece no
  core feature provides.
- **Sequences.** Postgres 19 can copy their values on demand
  (`REFRESH SEQUENCES`, publisher 19 or later), which is what a cutover
  needs; nothing streams them as they advance, and nothing allocates
  them across several writers.
- **Conflict resolution.** Postgres 18 counts conflicts; it does not
  resolve them. Avoiding them by construction, as the architecture here
  does, is still the only built-in answer.
- **Joining and leaving a topology.** BDR had `join` and `part`; today
  adding a region or retiring one is a procedure written by hand.
- **The consumer as a first-class node.** A logical consumer is not in
  the state machine of anything that orchestrates the primary it reads
  from, so who creates the slot, who checks its position after a
  promotion, and who raises the alarm when it is ahead is still a script.

Postgres has a decade of foundations for all of these. Most of the
remaining work is of the same kind as the work already done: unglamorous,
carefully reviewed, and merged one release at a time.

Every part has a `compose/` directory of its own, this one included.
Run them, break them, and tell me what I got wrong.

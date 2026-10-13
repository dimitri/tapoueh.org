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
servers are no longer close together, more than one of them writes, and
the CDC consumer has to be a real one instead of a demo.

Scope stays where the rest of the series drew it: open source only.
pglogical and Citus are both OSS and stay in scope, the same as Part 1
left them. BDR is named once, correctly dated — version 1.x was open
source and reached end of life in December 2019; everything since,
including EDB Postgres Distributed, is commercial — and RDS/Aurora are
named once as the managed reference point. Neither is built against here.

<!--more-->

{{< lab >}}
This post's own `compose/` directory is not built yet. Sections below are
in one of two states, marked as they come up: **verified** means checked
against the actual Postgres, pg_auto_failover, or pgcopydb source — file
and line, not recollection — and stated as fact. **To measure** means the
mechanism is real and sourced, but the number that goes with it needs an
actual run, and is left as a named placeholder rather than invented. Parts
1 through 3 only ever quoted numbers from a real `make run`; this one will
too, once it has one.
{{< /lab >}}

<!--toc-->

---

## The scenario

{{< image src="fig-region-map.svg" title="One writer per region: conflicts avoided by construction. Each region's primary key includes the region, so no two regions ever mint the same row. New York and Mumbai replicate both ways with the Amsterdam warehouse: their own writes flow up, a row-filtered, read-only mirror of the other region flows back down — never a write path back, since neither side's mirror is ever of its own data. San Francisco and Sydney are not separate regions: each cascades its logical replication stream from the region's own primary, one hop downstream, same key discipline, no write of its own. Every site has a front desk; New York and Mumbai's are read-write at home and read-only to the mirror, the other three are read-only everywhere." >}}

Four things, each a post of its own in a shorter series, covered here in
one, in the order a team actually runs into them: the primary dies and a
standby takes over — does the CDC consumer notice; the servers are a
continent apart — does that change the answer; two of them accept writes
on purpose — what happens when they touch the same row; and the thing
reading the change stream has to be a real program, not three sections of
a Part 2 demo that stopped on purpose before finishing one.

## Publisher failover, and what it costs across a region

Every subscription in this series so far pointed at one primary and
stayed there. Part 3's own reverse-subscription trick survives a planned
cutover; it says nothing about the primary dying unplanned, mid-stream,
with a consumer attached to it.

### What Postgres actually guarantees

**Verified**, against `REL_19_STABLE`, not inferred from the feature's
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

None of that is pg_auto_failover's job today. **Verified** against the
same source tree this post's own `pg_auto_failover` checkout uses: a
`grep` for `sync_replication_slots` or `synchronized_standby_slots`
across `src/` returns exactly one hit, a comment in `pgctl.c` noting the
GUC exists — no code sets it, no code creates a failover slot, no code
checks one before deciding who gets promoted. The monitor gets you the
orchestration — who's primary, when a promotion happens, a connection
string that survives one — for free. It gets you none of the mechanism
above.

{{< image src="fig-failover-slot.svg" title="A failover-safe logical slot, driven by pg_auto_failover's existing orchestration. Physical replication keeps the standby current, including the slot's own state, once synchronized_standby_slots names it. After promotion the consumer reconnects to the same slot, now on the new primary, and resumes at its last confirmed LSN: duplicates are possible, a hole is not." >}}

### Building it against pg_auto_failover as it is today

Not a new `pg_autoctl` verb — that's a real feature, scoped separately
below — but a relay script against the monitor and the primary exactly
as they already exist:

1. Create the slot with `failover = true` on the primary.
2. Set `synchronized_standby_slots` to the standby's physical slot name
   by hand (the same name `pg_auto_failover` already assigns,
   `pgautofailover_standby_<id>`), and confirm `sync_replication_slots`,
   `hot_standby_feedback`, and `dbname` are set on the standby — none of
   this is automatic yet, all of it is one `ALTER SYSTEM` / config-file
   edit away.
3. Persist the consumer's own position durably, keyed by the last
   `end_lsn` it has fully applied — never resume from 0, never trust an
   in-memory position across a restart.
4. On every reconnect, `IDENTIFY_SYSTEM` before trusting the server: if
   its reported position is behind what the consumer already has, or a
   `TIMELINE_HISTORY` switchpoint sits below the consumer's stored LSN,
   stop and raise an alarm instead of streaming — a consumer that is
   ahead of the history it is about to read from is the one case the
   slot's own guarantee cannot protect.

`[TODO: build this against a real pg_auto_failover-managed primary and
standby pair, force a promotion with pg_autoctl perform failover, and
measure: time from promotion to the consumer's first post-failover
message; whether any duplicate arrives, and how many; whether a
deliberately misconfigured synchronized_standby_slots produces the
predicted hole.]`

### Geo-replication is the same mechanism, read for latency instead of correctness

Move the standby to another region and nothing above changes — the gate
is about flushed LSNs, not distance. What does change is the one number
the correctness question doesn't ask about: how long a `synchronous_commit
= remote_apply` write waits for a cross-region round trip, versus how
much RPO an async standby gives back in exchange for not waiting at all.
That trade is the maintenance-window trade from Part 3's RTO/RPO section,
with a continent's worth of latency standing in for the window.

`[TODO: measure synchronous_commit = on, remote_write, remote_apply, and
off, same demo schema, same traffic generator, across a real cross-
region link rather than localhost — the number that actually matters
here cannot come from a laptop.]`

### What a real `pg_autoctl create proxy` would still need

Everything above is a hand-run script, not an orchestrated component.
Turning it into one is real pg_auto_failover development, not a demo
change: a new catalog table (a logical consumer is not a node — every
existing topology routine, from `synchronous_standby_names()` to
`formation_uri()`, iterates the node table and would miscount one as a
standby if it were registered as such), a new state machine the monitor
drives the same way it drives the keeper's, and keeper changes in the
handful of places that already almost do this — `pgctl.c`'s recovery
settings already write `dbname` into `primary_conninfo` with a comment
about `pg_sync_replication_slots()`, half-built and never finished. That
component is named here, scoped, and left where Part 3 left whole-cluster
orchestration: on the roadmap, not shipped.

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
narrower if compliance needs it narrower — with the region itself part
of every table's key, the same `(worker_id, event_id)` discipline Part 1
used for workers, drawn at a scale where "worker" means "everything the
EU's data-residency rules apply to." No row is ever a candidate for two
regions to write, because the key says which one owns it, so there is no
conflict left for anything downstream to detect, let alone resolve.
Each region's primary is the Part 3 architecture in full — its own
standbys, its own failover story, the mechanism two sections up — and
replication's only job is the one Part 2 already built: an asynchronous,
read-only roll-up into one global warehouse. Amsterdam never originates
a write of its own — every row it holds came from New York or Mumbai
first — but the map shows it fanning the combined view back out, to
each site, as a row-filtered copy: New York's mirror excludes
`region = 'americas'`, since it already has that natively, and holds
only Mumbai's rows; Mumbai's excludes `region = 'apac'` the same way.
Nothing either site already owns is ever re-applied to itself, so nothing
here is a write path back in the sense that would matter — it is the
identical one-way, read-only flow Part 2 built, just run twice, once per
direction, between two partitions neither side was ever going to write
to anyway. Getting the writing half right costs exactly what Part 1's
worker-key discipline cost: a wider key, and a rule about where a row is
allowed to be written, enforced once, at the application layer, instead
of resolved forever at the replication layer.

The map's other symbol is the same discipline drawn at its point of
contact with a person. Every site has a front desk, and what it can do
there follows directly from what the site under it is. At New York and
Mumbai, the desk is read-write against its own region's primary and
read-only against the mirror of the other region arriving from
Amsterdam — two disjoint partitions, one writable, one not, on the same
connection. At San Francisco, Sydney, and Amsterdam itself, the desk is
read-only, full stop, for the same reason those three nodes never take
a write of their own: a cascade one hop downstream and a pure
consolidator are not where this series' write discipline says a row is
allowed to be minted. None of this is a new mechanism — a desk is just
an application connecting to Postgres, same as a terminal in Part 1 or
Part 2 — the only thing worth drawing is which half of each connection
is read-write and which is read-only, because that split is the whole
architecture in miniature.

`[TODO: build the two-region demo — two primaries, one warehouse, each
side's row-filtered mirror flowing back, a traffic generator that
writes to each primary independently — and confirm zero conflict
counters move no matter how concurrently it runs.]`

### Cascading, one hop downstream

San Francisco and Sydney in the map above are not a third and fourth
region. Each is a logical replication subscriber of its region's own
primary, built the same way every subscriber in this series has been —
`CREATE SUBSCRIPTION` against one upstream. The only difference is what
each one does next: it is also a publisher, exposing what it just
applied, and a further consumer subscribes to that instead of going
back to New York or Mumbai directly.

That is not a new capability. **Verified**: a subscriber re-publishing
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

For a one-way cascade like San Francisco or Sydney — leaf nodes,
nothing subscribes to them — `origin` changes nothing today. It
matters the day a subscription goes back toward New York, or a third
hop goes out from San Francisco: that is exactly the shape `origin =
none` exists to cut off, and setting it on a leaf costs nothing before
there is a loop to have. What cascading does not change is what was
already true one hop up: DDL and sequence state still do not
replicate, at any hop, for the same reason Part 2 found at the first
one.

Real deployments covering APAC rarely stop at one extra node. Singapore,
Tokyo, and Hong Kong each show up as their own region in every major
cloud's catalog, for a geographic reason as much as a commercial one —
Mumbai to Sydney is already something like 9,500 km, and Tokyo to
Sydney is not much shorter, so one APAC primary cascading to every city
in the region buys consistency at the cost of exactly the latency this
architecture exists to avoid. China is its own case regardless: the
major clouds' China regions run under separate local operators, not as
another node on the same global account, which makes it a business and
compliance decision before it is a replication one. A real build-out
would reach for several of these; this post draws two cascades, New
York to San Francisco and Mumbai to Sydney, because the mechanism — one
hop, same `origin` discipline — does not change with a third or a
fifth. Draw it once, repeat it per city.

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

`[TODO: re-run Part 1's three pglogical findings — the partitioned-
parent crash, the filter-move silently dropped on both sides, keep_local's
undetectable divergence — against a current PG18 build, concurrent
writers this time, and confirm whether any of the three has changed.]`

The sequence gap from Part 1 is still open as of this post: no sequence
access method landed in the Postgres 19 source tree, `galloc` and
`snowflakeid` remain EDB Postgres Distributed features, not core ones.
`[TODO: check the mailing list again before publishing — late-2025
activity was the last data point Part 1 had.]`

BDR/PGD is the honest answer to "where does real multi-master
conflict handling live today," named once, for the record: version
1.x was open source and reached its end of life in December 2019;
every version since is commercial, under EDB. It is a reference point
for this section, the same way RDS and Aurora are a reference point in
Part 3 — never something built against here.

---

## Finishing the CDC consumer: DDL as a message, not a table

Part 2 built the honest baseline — a queue table, an Event Trigger, a
replica-side trigger that replays what it finds — and named exactly
where it stops being a demo: "a component that merges and splits change
streams without writing tables... a story for another article," and a
*general* consumer that also decodes `pgoutput`'s binary row changes,
called "a real project, not a demo" and left unbuilt. This section is
that project, scoped down to the part that's actually missing.

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
DDL replay on — arrives on the wire and is logged as unknown and
dropped. That is the entire gap. The taxonomy on the other side of it
is already built and waiting: `STREAM_ACTION_MESSAGE` has been in
`pgcopydb`'s own action enum since before this post, the JSON
round-trip for it is already wired, and the SQLite-backed storage layer
already has a generic insert path for it. Nothing downstream of the
decoder needs to change shape — it needs one new case in one switch
statement, and something to actually do once that case is reached.

{{< image src="fig-pgcopydb-message.svg" title="DDL as a message, not a queue table. An Event Trigger emits a JSON message, prefix pgcopydb, type ddl, body the command text. pgcopydb follow decodes the Message frame, filters on the prefix, parses the JSON, and dispatches on type — the ddl handler executes the body on the same connection the rest of the apply worker already uses, inside the same commit-ordered stream as every other change." >}}

### The design

Full design in a separate note, `pgcopydb-cdc-messages.md`, built on a
line-by-line read of both the pgcopydb source and Postgres core's own
wire format for the Message frame (`proto.c:
logicalrep_write_message`). A JSON envelope, `{"type": "ddl", "body":
"..."}`, with `type` as an open
registry — `ddl` is the only one this post ships, and the envelope
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
create function shop.emit_ddl_message() returns event_trigger
  language plpgsql as $f$
declare
  payload json;
begin
  payload := json_build_object('type', 'ddl', 'body', current_query());
  perform pg_logical_emit_message(true, 'pgcopydb', payload::text);
end;
$f$;

create event trigger shop_emit_ddl on ddl_command_end
  when tag in ('CREATE TABLE', 'ALTER TABLE')
  execute function shop.emit_ddl_message();
```

`[TODO: implement the case 'M': decode, the STREAM_ACTION_MESSAGE
dispatch in ld_apply.c, and the --messages flag on pgcopydb follow;
re-run Part 2's own shop/warehouse demo with this trigger instead of
the queue table, and confirm the warehouse gets the column with zero
manual alter table statements.]`

Two edges carry over unchanged, because the source side is unchanged:
`current_query()` captures a multi-statement command whole, not split,
and a replayed command runs with the apply worker's own role and
`search_path`, not necessarily the one the original session had. Both
are documentation, the same as they were in Part 2, not new code to
write.

### No pre-existing open-source tool already does this

PeerDB (AGPLv3, confirmed still open after its ClickHouse integration) and
Sequin both stream one Postgres source out to many sinks, with routing
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
pg_auto_failover-shaped future work, the same honest "not built here"
this post has already used twice.

---

## What this post adds to the ladder

Four things, and each one is additive to what Parts 1 through 3 already
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
- **A finished CDC consumer** closes the one gap Part 2 explicitly
  left open, and does it as a real, scoped change to a tool this site
  already maintains, not a bigger demo.

The three `[TODO]` markers through this post are not a hedge — they are
the same discipline Part 3 closed on: every number anywhere in this
series came from an actual rehearsal, and this post does not get to
skip that just because it is the last one.

The demo for this post, once it exists, will be in a `compose/`
directory of its own, the same as every other part. Until then: run the
other three, break them, and tell me what I got wrong — this one included.

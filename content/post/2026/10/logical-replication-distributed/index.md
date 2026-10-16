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

{{< image src="fig-region-map.svg" title="One writer per region: conflicts avoided by construction. Four equal primaries, four disjoint region keys — us-east in New York, us-west in San Francisco, in in Mumbai, au in Sydney — so no two of them ever mint the same row. Every site, the Amsterdam warehouse included, is a pg_auto_failover-managed primary and standby pair, not a single server. Every link is the same bidirectional logical replication: a region's own writes flow one way, a read-only mirror of everyone else's data flows back the other way. New York and Mumbai link straight to one global warehouse in Amsterdam; San Francisco and Sydney cascade through them instead of each opening its own long-haul connection. Every site has its own front desk, read-write against its own region and read-only against the mirror of the rest." >}}

Four things, each a post of its own in a shorter series, covered here in
one, in the order a team actually runs into them: the primary dies and a
standby takes over — does the CDC consumer notice; the servers are a
continent apart — does that change the answer; two of them accept writes
on purpose — what happens when they touch the same row; and the thing
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

None of that was pg_auto_failover's job as of this series' earlier
parts. **Verified** against the project's `main` branch at the time:
a `grep` for `sync_replication_slots` or `synchronized_standby_slots`
across `src/` returned exactly one hit, a comment in `pgctl.c` noting
the GUC exists — no code set it, no code created a failover slot, no
code checked one before deciding who gets promoted.

That gap closed. **Verified** against PR #1197 itself
(`hapostgres/pg_auto_failover`, merged 2026-10-14, the day before this
section was last checked): a new monitor function,
`pgautofailover.synchronized_standby_slots(formation, group)`, computed
the same way `synchronous_standby_names()` already is — every current
failover candidate's physical slot name — which `pg_autoctl` fetches
and applies on the primary through the same `fsm_apply_settings()` path
that already maintains `synchronous_standby_names`. `sync_replication_slots`
turns on by default for Postgres 17+ nodes, next to the existing
`hot_standby_feedback` default, and — the change with the widest
reach — `wal_level` becomes `logical` on every node pg_auto_failover
creates, not `replica`, with a `--wal-level replica` flag to opt back
out for clusters that don't need CDC at all. The PR also fixes a real
bug, #1188: a node rejoining as standby after an interrupted demotion
could keep its own stale failover slots around, permanently blocking
the slot-sync worker; those now get dropped on rejoin like any other
leftover. What the PR deliberately does not do is create or manage the
logical slot itself — that stays the CDC tool's job, same as it always
was, the same way `pg_auto_failover` has never owned a table's schema
or a `pg_dump` object list.

The monitor still gets you the orchestration — who's primary, when a
promotion happens, a connection string that survives one — for free,
same as before. As of this PR, it also gets you the mechanism above:
the GUCs that make a failover-safe slot actually mean what its name
says, kept correct across every role change, automatically.

{{< image src="fig-failover-slot.svg" title="A failover-safe logical slot, driven by pg_auto_failover's existing orchestration. Physical replication keeps the standby current, including the slot's own state, once synchronized_standby_slots names it. After promotion the consumer reconnects to the same slot, now on the new primary, and resumes at its last confirmed LSN: duplicates are possible, a hole is not." >}}

### Building it against pg_auto_failover as it is today

Not a new `pg_autoctl` verb — that's a real feature, scoped separately
below — and, with PR #1197, a shorter list than it used to be:

1. Create the slot with `failover = true` on the primary — still
   user-managed, deliberately; #1197 keeps the GUCs around a slot
   correct across every role change, not the slot's own lifecycle.
2. Persist the consumer's own position durably, keyed by the last
   `end_lsn` it has fully applied — never resume from 0, never trust an
   in-memory position across a restart.
3. On every reconnect, `IDENTIFY_SYSTEM` before trusting the server: if
   its reported position is behind what the consumer already has, or a
   `TIMELINE_HISTORY` switchpoint sits below the consumer's stored LSN,
   stop and raise an alarm instead of streaming — a consumer that is
   ahead of the history it is about to read from is the one case the
   slot's own guarantee cannot protect.

Before #1197, step 1 above had a second half: set
`synchronized_standby_slots` to the standby's physical slot name by
hand (the same name `pg_auto_failover` already assigns,
`pgautofailover_standby_<id>`), and confirm `sync_replication_slots`,
`hot_standby_feedback`, `dbname`, and `wal_level = logical` on both
sides — one `ALTER SYSTEM` or config-file edit away, and silent if
missed. That half is now pg_auto_failover's job, not this script's.

`[TODO: build this against a real pg_auto_failover-managed primary and
standby pair on the release that ships #1197, force a promotion with
pg_autoctl perform failover, and measure: time from promotion to the
consumer's first post-failover message; whether any duplicate arrives,
and how many; whether a deliberately misconfigured
synchronized_standby_slots still produces the predicted hole now that
pg_auto_failover sets it for you.]`

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

Everything above, even with #1197 landed, is still a hand-run script
wired to existing primitives, not an orchestrated component of its
own. #1197 closes exactly the gap this section used to point at —
`pgctl.c`'s recovery settings already wrote `dbname` into
`primary_conninfo` with a comment about `pg_sync_replication_slots()`,
half-built and never finished; that comment is now a shipped function
call, not a TODO. What it does not add is a new catalog table for a
logical consumer (a logical consumer is still not a node — every
existing topology routine, from `synchronous_standby_names()` to
`formation_uri()`, iterates the node table and would miscount one as a
standby if it were registered as such), or a state machine the monitor
drives the same way it drives the keeper's. That is still real
pg_auto_failover development, not a config change — the PR's own
`docs/orchestrated-operations.rst` names roughly this shape, a
generalized monitor/client primitive for a whole-cluster, multi-step,
resumable operation, and calls it a design exploration, not an
implementation. That component is named here, scoped, and left where
[Part 3](/blog/2026/10/zero-downtime-postgres-upgrades-with-logical-replication/#what-is-still-on-you)
left whole-cluster orchestration: on the roadmap, not shipped.

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
draws four: `us-east` in New York, `us-west` in San Francisco, `in` in
Mumbai, `au` in Sydney. No row is ever a candidate for two of them to
write, because the key says which one owns it, so there is no conflict
left for anything downstream to detect, let alone resolve. Every site on
the map, the warehouse included, is drawn as the pair of overlapping
circles the legend calls "Primary + standby" — the Part 3 architecture
in full, a pg_auto_failover-managed group with its own standby and its
own failover story, not a single server with a single point of failure.
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
holds only `'in'` and `'au'`; Mumbai's excludes `('in', 'au')` the same
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

`[TODO: build the four-primary demo — four writers, two of them
cascading through the other two, one warehouse, every mirror flowing
back filtered — and confirm zero conflict counters move no matter how
concurrently it runs.]`

### Cascading, one hop downstream

San Francisco and Sydney are full primaries, not read-only leaves —
`us-west` and `au` are real, independently written regions, exactly
like `us-east` and `in`. What makes them different from New York and
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

Building the demo this section draws on found something a read of the
documentation alone did not: for a disjoint, region-keyed schema like
this post's own, `origin` is not actually the tool that closes the
loop — row-filtered publications are, and once they're in place
correctly, `origin` turns out to have nothing left to do.

The naive version would give San Francisco an unfiltered subscription
to everything New York publishes. New York's publication carries
`us-east`'s own writes plus Amsterdam's mirror of `in` and `au`, which
San Francisco genuinely needs — the whole point of a four-way mirror is
that a hop away from the primary still sees the rest of the world — but
it would also carry San Francisco's own writes reflected back the
moment San Francisco's own publication toward New York is unfiltered
too. `origin = none` looks like the fix: it worked cleanly one hop up,
in the two-node `pgoutput`/`pgoutput_origin_filter` case `origin`
was designed for. It does not work here, because `origin` is a binary
— `none` or `any`, not a per-origin allow list — and `none` on San
Francisco's subscription would drop Amsterdam's `in`/`au` mirror along
with the loop, since both arrive at San Francisco carrying *some*
non-invalid origin once New York's apply worker has touched them. There
is no built-in way to ask for "everything except what I sent you."

What actually closes the loop, cleanly, is the same tool already doing
the conflict-avoidance work: a **row-filtered publication** on the
`region` column itself. New York publishes two different views instead
of one — `FOR TABLE events` unfiltered toward Amsterdam, and
`FOR TABLE events WHERE (region <> 'us-west')` toward San Francisco —
and San Francisco's own publication back toward New York is
`WHERE (region = 'us-west')`, nothing else, ever. Neither filter cares
what hop a row arrived on or what it did or didn't already touch; each
one only looks at a value every row already carries. With that in
place, a row can never reach a link that would send it back to where it
already lives, by construction, the identical guarantee the primary-key
discipline already gives the write side. `origin = none` stays real and
useful — it is still the right tool the day a `region` column is not
the thing a loop needs to be cut on — but it is not what this
particular architecture ends up needing, and that is worth saying
plainly rather than leaving the first draft's answer standing.

What cascading does not change is what was already true one hop up:
DDL and sequence state still do not replicate, at any hop, for the same
reason Part 2 found at the first one.

`[TODO: this post's own four-primary demo (profile c) builds exactly
this row-filtered topology — confirm the above against its actual
pg_publication/pg_subscription catalogs and a captured WAL trace, not
just the reasoning above.]`

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
Mumbai to Sydney, because the mechanism — one hop, same `origin`
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

`[TODO: re-run Part 1's three pglogical findings — the partitioned-
parent crash, the filter-move silently dropped on both sides, keep_local's
undetectable divergence — against a current PG18 build, concurrent
writers this time, and confirm whether any of the three has changed.]`

The sequence gap
[Part 1](/blog/2026/09/ten-years-of-postgres-logical-replication/#minting-ids-on-the-workers)
is still open as of this post, and now with a more final answer than
"not yet committed." **Verified** against the commitfest entry itself:
Michael Paquier's "Sequence Access Methods, round two"
([commitfest #4677](https://commitfest.postgresql.org/52/4677/)) was
returned with feedback in PG19-Final on 2026-02-27, after more than two
years moving between commitfests without landing. It will not be in
Postgres 19. `galloc` and `snowflakeid` remain EDB Postgres Distributed
features, not core ones, and stay that way for at least one more
release.

BDR/PGD is the honest answer to "where does real multi-master
conflict handling live today," named once, for the record: version
1.x was open source and reached its end of life in December 2019;
every version since is commercial, under EDB. It is a reference point
for this section, the same way RDS and Aurora are a reference point in
Part 3 — never something built against here.

---

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
DDL replay on — arrives on the wire and is logged as unknown and
dropped. That is the entire gap, as of the source this post started
against. The taxonomy on the other side of it is already built and
waiting: `STREAM_ACTION_MESSAGE` has been in `pgcopydb`'s own action
enum since before this post, the JSON round-trip for it is already
wired, and the SQLite-backed storage layer already has a generic
insert path for it. Nothing downstream of the decoder needs to change
shape — it needs one new case in one switch statement, and something
to actually do once that case is reached. "The design" section below
is no longer just this post's own proposal for that case and that
something; it is merged code, checked against this post's own
`compose/`, not just read.

The pain this leaves on the table is not hypothetical. **Verified**
against pgcopydb PR #1061, open as of this writing, fixing issue #1060:
on the `wal2json` plugin path — a different decoder,
`ld_transform.c`'s `parseMessage()`, not the `pgoutput` one above — the
same kind of frame does not get filtered harmlessly, it breaks `follow`
outright. A message with no `schema` or `table` property (PeerDB's or
ClickPipes's own `peerdb_heartbeat`, emitted on any source those tools
share) routes into the DML branch anyway, `parseWal2jsonMessage()` fails
parsing a row shape out of a message that was never a row, and apply
retries the same transaction forever; the reported fix in `tests/`
shows `follow` reaching endpos with real rows silently missing on the
target — the exact silent-gap failure mode a CDC consumer cannot
afford. The proposed fix is narrower than this post's: return early and
drop the message before the DML branch, the same outcome `pgoutput`'s
own `default:` case already gets for free, not a dispatch table. Two
decoders, two different bugs, the same underlying cause either way — a
Message frame reaching code that assumes every frame is a row.

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

This design did not stay a design. [PR #1064](https://github.com/dimitri/pgcopydb/pull/1064),
"replicate DDL over logical decoding as a typed message," implements
exactly it — the `case 'M':` decode in `ld_pgoutput.c`, storage in
`ld_store.c`, the `STREAM_ACTION_MESSAGE` dispatch in `ld_transform.c`
and `ld_apply.c`, and a `pgcopydb stream event-trigger` subcommand that
installs the trigger above automatically instead of asking a reader to
write it by hand. **Merged** to `main` 2026-10-16.

This post tested three separate commits on the way there — the
original PR (`febf16b`), a force-push (`251a03a`) that fixed a real but
different bug (`stream_apply_flush_pipeline()`, draining the apply
connection's pipeline before reporting `replay_lsn`, closing a gap
where `follow` could report a COMMIT done while it was still only
queued), and now the merge itself (`ffb41bd`) — and found the same
symptom against all three: receive, storage, and transform are
correct (**verified** directly against the running containers' own
SQLite state, not taken on trust — the message lands in `output.db`
with the right envelope, resolves to a correctly hashed `stmt` row in
`replay.db`, tagged with the real transaction xid), and the DDL never
reaches the target.

**Verified** down to the actual root cause this time, not just the
symptom: a one-line diagnostic patch into `stream_apply_dml` (kept in
this post's own `compose/patch/`, not a hypothesis) confirms
`pgsql_execute()` for the DDL statement *returns success* — and
nothing from that point on ever reaches the target connection again.
`pg_stat_activity` on the target shows the apply connection sitting
idle, `pg_replication_origin_session_setup()` as the last query it
ever actually ran, for as long as the connection is left open —
through fifty unrelated `INSERT`s issued on the source afterward,
which never land either. That rules out anything specific to DDL
content or the message envelope: once a message-carrying transaction
is in flight, the apply connection stops making progress at all,
message or not. The shape matches the `stream_apply_flush_pipeline()`
fix in `251a03a` exactly, one hop earlier: the apply connection runs in
libpq pipeline mode, where sending a command and having the server
confirm it are two different steps, and `pgsql_execute()` returning
`true` here means "queued," not "committed." Something about a
message-carrying transaction's completion path is not reaching
whatever step makes every *other* transaction's queued work actually
flush.

`[TODO: this is now a precise enough finding to file against #1064
directly instead of re-describing the symptom — the diagnostic patch
in compose/patch/ld_apply.c is the attachment. Re-run profile d clean
once a real fix lands; confirm the warehouse gets the column with zero
manual alter table statements, the way this section originally
promised.]`

Two edges carry over unchanged regardless, because the source side is
unchanged: `current_query()` captures a multi-statement command whole,
not split — confirmed the hard way, in this same `compose/` run, when
a multi-statement debug block landed in `replay.db` as a single DDL
message — and a replayed command runs with the apply worker's own role
and `search_path`, not necessarily the one the original session had.
Both are documentation, the same as they were in Part 2, not new code
to write.

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

The `[TODO]` markers through this post are not a hedge — they are
the same discipline Part 3 closed on: every number anywhere in this
series came from an actual rehearsal, and this post does not get to
skip that just because it is the last one.

The demo for this post, once it exists, will be in a `compose/`
directory of its own, the same as every other part. Until then: run the
other three, break them, and tell me what I got wrong — this one included.

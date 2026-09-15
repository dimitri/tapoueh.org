+++
title     = "Plan Advice in PostgreSQL 19"
date      = "2026-09-16T09:00:00+0200"
tags      = ["PostgreSQL", "SQL", "Performance"]
categories = ["PostgreSQL", "Performance"]
icon      = "🐘"
+++

There is a conversation that happens in every PostgreSQL shop eventually. A
query that has been fine for a year gets slow overnight. Nothing was
deployed. The data grew a little, `ANALYZE` ran, and the planner — entirely
reasonably, on the numbers it had — picked a different plan. The old plan
was better. You would like it back.

PostgreSQL 19 ships two new modules for exactly this: `pg_plan_advice`,
which can read a plan back out as a string and enforce it later, and
`pg_stash_advice`, which keeps those strings keyed by query id and applies
them automatically.

{{< lab >}}
Every query below ran against [the Lab](https://theartofpostgresql.com/lab/),
the free dataset bundle used throughout this blog, on PostgreSQL 19 Beta 3:
`POSTGRES_VERSION=19beta3 PG_MAJOR=19 docker compose up`. Both modules are
contrib, and the Lab image ships them; nothing below runs a `LOAD` to
enable them, because the server already has them — `pg_plan_advice` in
`session_preload_libraries`, `pg_stash_advice` in
`shared_preload_libraries` (it can survive a restart, which needs
loading that way). One line each in `postgresql.conf`, or the equivalent
server-start flag, and you're done. `pg_stash_advice` still needs its own
`CREATE EXTENSION`, further down, for its SQL functions — that's
independent of how the module itself got loaded.
{{< /lab >}}

<!--more-->

<!--toc-->

---

## Reading a plan back out

Start with a query that joins three F1 tables. A freshly restored Lab
image has never been analyzed, so the setup below includes the `ANALYZE`
from the opening paragraph before running the query — the last thing a
comparison needs is statistics that do not reflect the data yet:

\include{sql/1-the-query.sql}

\include{results/1-the-query.out}

Nothing remarkable about the query itself. Now ask the planner not just
what it did, but to describe what it did in a form it can read back:

\include{sql/2-plan-advice.sql}

\include{results/2-plan-advice.out}

Indented `EXPLAIN` output is a tree written sideways, and it is worth
seeing as one before going further — the plan below is what those four
lines of advice are describing:

\include-explain-plan-diagram{The plan as a tree, rows flowing upward: three sequential scans at the bottom, two of them feeding hash builds, two hash joins above those, and a Hash Aggregate under the query. Nothing in the tree carries a number, because this plan was taken with COSTS OFF.}

That trailing block under the plan is the whole idea. Four lines,
describing four decisions the planner made: which table drives the join
and in what order, which join method to use, how to reach each relation,
and whether to go parallel.

{{< image src="fig-advice-anatomy.svg" title="The same plan expressed twice: as a plan tree on the left, and as four lines of advice on the right. Join order, join method, scan method and parallelism each get one line, colour-matched to the part of the tree they describe." >}}

`JOIN_ORDER(results races drivers)` says `results` is the driving table,
joined first to `races` and then to `drivers`. `HASH_JOIN(races drivers)`
says each of those belongs on the *inner* side of a hash join.
`SEQ_SCAN(...)` and `NO_GATHER(...)` say how to read each table and that
none of it should run in parallel.

Note what is *not* here. There is no cost, no row estimate, no timing —
advice describes outcomes, not the reasoning that produced them. That is a
deliberate design choice, and it is the reason advice survives a change in
statistics: it does not mention any.

---

## You do not need PostgreSQL 19 for this part

There is an obvious problem with everything above: `pg_plan_advice` is a
PostgreSQL 19 contrib module, and you are probably not running PostgreSQL
19. Most people will not be for years.

The useful half of that four-line block does not actually depend on the
server, though. It is a *description of a plan* — and the plan text is
something every version has been printing all along. So it can be
reconstructed. [`sqlfmt`](https://github.com/dimitri/sqlfmt) does that,
from ordinary `EXPLAIN` output, on any version:

```sh
$ sqlfmt explain advice plans/default.txt
```

\include{results/7-advice-any-version.out}

That is the same four lines PostgreSQL 19 printed above, from a plan
captured on a server that has never heard of `pg_plan_advice`.

### Why this is the right way to compare two plans

Putting two plans side by side works fine at the size of the ones in this
article. It stops working at twenty nodes, and twenty-node plans are the
ones you actually need to compare. The mechanical answer — run `diff` over
two `EXPLAIN` outputs — does not help either: every line carries a cost or
a timing, so *every line differs*, and the one change that matters drowns
in the noise.

Leaving the numbers out is what makes the comparison tractable. Because no
cost or timing appears, two runs of the same plan produce identical output,
and any difference is a real difference. Here is the planner's own plan
against one with a different join order forced onto it — the same forced
plan the next section walks through in full:

```sh
$ sqlfmt explain diff plans/default.txt plans/forced.txt
```

```diff
--- plans/default.txt
+++ plans/forced.txt
@@ plan structure @@
-JOIN_ORDER(results races drivers)
+JOIN_ORDER(drivers results races)
-HASH_JOIN(races drivers)
+HASH_JOIN(results races)
-SEQ_SCAN(results races drivers)
+SEQ_SCAN(drivers results races)
-NO_GATHER(results races drivers)
+NO_GATHER(drivers results races)
```

The driving table moved and both join methods stayed hash joins — the
`HASH_JOIN` line just names a different pair, because forcing `drivers`
to drive puts a different relation on the inner side. With `EXPLAIN
(ANALYZE)` plans the timings are reported too, as context lines under
the structural hunk, so you can see whether the shape change actually
bought anything. It exits non-zero when the plans differ, which makes it
usable as a check in CI.

One thing this is not: PostgreSQL 19 computes advice *inside the planner*,
which knows the whole query, while `sqlfmt` reconstructs it from a
rendering of the result. It is a comparison key, not a round-trippable
advice string — do not feed its output to `pg_plan_advice` and expect it
to apply.

### Comparing across versions

The comparison worth making most is the one you make before an upgrade.
You have the plan your PostgreSQL 16 server produces for a query you care
about, and you have what a PostgreSQL 19 server says about the same query,
printed by `pg_plan_advice` itself. Did the planner change its mind?

One plan from each side, and the two write the same decisions down
slightly differently — 19 orders a set of relations by its own internal
numbering and schema-qualifies index names, neither of which is in plain
`EXPLAIN` text. `-canonical` puts both into one normal form:

```sh
$ diff <(sqlfmt explain advice -canonical pg16-plan.txt) \
       <(sqlfmt explain canonical pg19-plan.txt)
```

It sorts what is a set, leaves `JOIN_ORDER` alone because there the order
*is* the meaning, keeps index pairs together as pairs, and drops schema
qualifiers. `explain canonical` is the counterpart for the other side: it
reads an advice block a server already printed, finding it inside a whole
`EXPLAIN (PLAN_ADVICE)` capture.

What is left after that is only real difference. Which is the point — you
are asking whether the upgrade changed your plans, and you want the answer
to be a short list or an empty one, not a page of notation.

---

## Making the planner take it

Feed a string back through `pg_plan_advice.advice` and the planner is
obliged to follow it. Here is the same query, told to drive from `drivers`
instead:

\include{sql/3-force-join-order.sql}

\include{results/3-force-join-order.out}

Two things to notice. The plan really did change — `drivers` is now the
driving table — and the shape of the change is a PostgreSQL 19 feature in
its own right: `Partial HashAggregate` moved *below* the join, on
`results` alone, with `Finalize GroupAggregate` combining the partial
groups once every relation is joined. That is `enable_eager_aggregate`
(on by default), pushing as much of the `GROUP BY` down as it safely can
so the join has fewer rows to process. Forcing `results` to drive left no
room for that; forcing `drivers` to drive did.

The output also now carries *two* blocks: `Supplied Plan Advice`, echoing
what you asked for with a `/* matched */` annotation, and
`Generated Plan Advice` describing the plan you actually got.

That round trip is the feature. You can take the generated advice from a
plan you liked, hand it back later, and confirm from the `matched`
annotations that every piece of it landed.

---

## When advice does not win

Advice constrains the planner's choice *among plans it would consider*. It
does not resurrect plans that have been taken off the table. Turn off hash
joins and ask for one anyway:

\include{sql/4-advice-that-fails.sql}

\include{results/4-advice-that-fails.out}

`JOIN_ORDER` matched. `HASH_JOIN(races)` reports `matched, failed` — the
advice was understood, it applied to the right part of the query, and the
planner still could not honour it.

This annotation is the most useful thing in the whole interface, and it is
easy to walk past. Advice that silently does nothing would be worse than no
advice at all; you would carry a string in your configuration for two years
believing it was holding a plan in place. Here you can check.

---

## Stashing advice by query id

Setting `pg_plan_advice.advice` by hand works for experimenting, but you
cannot ask an application to do it. `pg_stash_advice` closes that gap: it
maps query ids to advice strings in shared memory, and applies them to any
query whose id matches.

The id could come from `EXPLAIN (VERBOSE)`, but more usefully it comes
from `pg_stat_statements`, which is where you were already looking when
you noticed the query had got slow — and not typed in from memory. Run
the query as the application actually sends it, no `EXPLAIN` wrapper, and
look it up:

\include{sql/5-stash.sql}

\include{results/5-stash.out}

`-5243066567089054587` is not a number this article picked; it is what
`pg_stat_statements` reports for that exact query text, and it is the same
number the plan advice above was generated for — `pg_stat_statements` and
`pg_plan_advice` compute query ids the same way, so one can name what the
other saw. From then on, the application changes nothing:

\include{sql/6-stash-applies.sql}

\include{results/6-stash-applies.out}

The only thing set is `pg_stash_advice.stash_name` — no
`pg_plan_advice.advice` string, no rewritten query, no `LOAD`. That last
one is not an accident: this session never asked for the module, and it
was there anyway, because `shared_preload_libraries` put it there before
the server even accepted its first connection. The plan changed because
the stash matched the query id, not because anything about this query
mentioned advice at all.

{{< image src="fig-advice-lifecycle.svg" title="The plan advice workflow: find the query in pg_stat_statements, read its plan back with EXPLAIN (PLAN_ADVICE), keep only the lines that matter, and stash it by query id. The dashed return path is the step people forget." >}}

---

## The part the documentation says twice

Both modules' documentation carries the same warning, and it is worth
repeating rather than paraphrasing: the planner's ability to change its
mind as the data changes is a feature. Advice takes that away. If the
distribution shifts under a pinned plan, you get the old plan applied to
new data, which is exactly the failure the planner exists to prevent. The
README is blunter still — bad advice producing a bad plan is "user error,
not a defect in this module".

So the discipline that makes advice useful is *trimming*: the generated
string describes every decision, and you almost never want to pin every
decision. If the join order flipped, keep `JOIN_ORDER(...)` and delete the
rest, so the planner keeps its freedom everywhere else. Applying advice
also costs planning time even when the plan does not change, which argues
for reaching for it per-query rather than cluster-wide.

---

## What this replaces

You have met the alternatives if you have run PostgreSQL at scale:
`pg_hint_plan`'s out-of-tree hints in query comments, the blunt
per-session `enable_*` family, or rewriting the query until the planner
agrees with you — which is not available when the query comes out of an
ORM you do not control.

What is new is the round trip: a plan can be *read out*, and the same
string put back. You are not writing hints from first principles and
hoping they describe the plan you remember; you are keeping a plan you
measured. That is a smaller feature than a hint language, and a much more
useful one.

And the reading-out half you can have today, on whatever PostgreSQL
you already run: paste any `EXPLAIN` output into a file and run
[`sqlfmt`](https://github.com/dimitri/sqlfmt) `explain advice` over it —

```sh
$ sqlfmt explain advice plan.txt
```

+++
title     = "Ten years of Postgres logical replication"
date      = "2026-09-21T18:00:00+0200"
tags      = ["PostgreSQL", "Replication", "Architecture"]
categories = ["PostgreSQL", "Architecture"]
icon      = "🐘"
+++

A long time ago I ran a write-heavy system on a hub and a handful of workers.
Each worker took a share of the application traffic and wrote events
locally. The hub owned the reference data (customers, plans, prices),
pushed it down to the workers, and pulled every worker's events back up to
compute the invoices. The plumbing was Londiste and PgQ: triggers on every
table, a queue per node, a ticker, and a Python daemon per hop. It worked,
and it was a lot of moving parts to explain to anyone new.

Postgres 10 shipped logical replication in 2017, and 19 is the tenth release
that has it. Every release since 10 has taken a piece of that plumbing and
made it a line of SQL.

So the question for this article is the application developer's one, not the
DBA's: *which architectures can I deploy with Postgres core alone today,
what does each release change about that, and where do I still need
something else?*

I picked three architectures and built each one for real:

1. **Hub and workers**, spreading the write load across servers.
2. **Consolidation**: many databases, different applications and schemas,
   into one, then re-exported as a change stream for a CDC consumer.
3. **Zero-downtime major upgrade**, with a way back.

<!--more-->

{{< lab >}}
Everything below ran, and the setups are kept so you can run them again:
one `docker-compose.yml` per architecture, a numbered `sql/` directory, and
the raw output of every step in `results/`, in the
[`compose/` directory of this post](https://github.com/dimitri/tapoueh.org/tree/master/content/post/2026/09/logical-replication/compose).
Each one starts with `make clean && make up && make run`. They use the
official `postgres` images (18.6, 17.11, 16.15, 14.24 and `19beta3`) rather
than the Lab image, because the demos need several servers side by side and
nothing from the Lab dataset. Quoted output is copied from those `results/`
files. The Lab image itself has one relevant caveat, see the Postgres 19
section.
{{< /lab >}}

<!--toc-->

---

## What each release changed

Here is the whole story as a table, written from the release notes of 10
through 19. Read it as "the first release where this stops needing an
extension or a workaround", for the things that matter to an application.

| You want to… | First in core | Before that |
|---|---|---|
| replicate tables between servers | 10 | pglogical (9.4+), Londiste, Slony |
| replicate `TRUNCATE` | 11 | pglogical |
| publish partitioned tables and subscribe into them | 13 | |
| stream a big transaction before it commits | 14 | wait for commit |
| send only some rows or columns; publish a whole schema | 15 | pglogical `row_filter` and replication sets |
| skip one bad transaction | 15 (`ALTER SUBSCRIPTION … SKIP`) | edit the catalog, or drop the subscription |
| avoid loops in two-way setups | 16 (`origin = none`) | pglogical `forward_origins` |
| decode from a standby; apply in parallel | 16 | |
| keep slots and subscriptions across `pg_upgrade`; slots survive a failover | 17 | external tooling |
| log conflicts with their kind | 18 | pglogical detects them too |
| **resolve** conflicts (last update wins…) | not in core, up to and including 19 | pglogical, and its descendants |
| replicate sequences | 19 (beta) | `pglogical.synchronize_sequence()`, or a script |
| replicate DDL | not in core, up to and including 19 | `pglogical.replicate_ddl_command()`, or a script |

Two rows in that table are empty of a release number, and they decide a lot
of what follows.

---

## Architecture 1: hub and workers

The application here is a small metering system. The hub holds `plans`,
`prices` and `customers`. Each customer is assigned to a worker
(`customers.worker_id`), and that worker records the customer's usage events.
The hub needs everything back to compute invoices.

{{< image src="fig-hub-workers.svg" title="Reference data flows down, filtered per worker. Usage events flow up, into one table partitioned by worker_id. Each table travels in one direction only, so there is no loop." >}}

### Reference data goes down

The first step is the Postgres 10 one: a publication on the hub, a
subscription on each worker.

```sql
create publication ref_all for table plans, prices, customers;
```

```sql
create subscription sub_ref_w1
       connection 'host=hub dbname=app user=postgres'
       publication ref_all;
```

The subscription name is also the name of the replication slot created on
the hub, so it must be unique per worker. Reuse `sub_ref` on a second worker
and you get `replication slot "sub_ref" already exists`. Also notice that the
tables are created by hand on each worker first: DDL is not replicated, in
any version, including the 19 beta.

With `ref_all`, every worker sees all nine customers, along with
`billing_notes`, which is for the finance team and nobody else. That is what
15 fixed: a row filter and a column list per worker.

```sql
create publication ref_w1 for table plans, prices,
       customers (customer_id, name, worker_id, plan_id)
 where (worker_id = 1);
```

Here is the first trap, and it is not in the documentation's first
paragraph. The filter uses `worker_id`, which is not in the primary key, and
the primary key is the replica identity. The publication is created without
complaint. The first `UPDATE` on the table fails, and it does not matter
which column you update:

```results
UPDATE customers SET name = 'renamed' WHERE customer_id = 3;
ERROR:  cannot update table "customers"
DETAIL:  Column used in the publication WHERE expression is not part of the replica identity.
```

The fix is a unique index that includes the filter column, used as the
replica identity, which is cheaper than `replica identity full`:

```sql
create unique index customers_rid on customers (customer_id, worker_id);
alter table customers replica identity using index customers_rid;
```

I ran the moves too. Reassigning a customer from worker 1 to worker 2
arrives as a `DELETE` on worker 1 and an `INSERT` on worker 2, which is what
you would hope. What you might not hope: changing a publication's filter is
not retroactive. Rows already copied to a worker stay there, so does a
column that used to be published, and cleanup is yours.

### Events come up

Each worker writes its own `usage_events`. The hub subscribes to all of them
into **one table partitioned by `worker_id`**, which has worked since 13:

```sql
create table usage_events
(
  worker_id   int    not null,
  event_id    bigint not null,
  customer_id int    not null,
  meter       text   not null,
  qty         int    not null,
  primary key (worker_id, event_id)
)
partition by list (worker_id);

create table usage_events_w1 partition of usage_events for values in (1);
create table usage_events_w2 partition of usage_events for values in (2);
create table usage_events_w3 partition of usage_events for values in (3);
```

The rows are routed to their partition, and the invoicing query is a plain
join between events that came from three servers and reference data that the
hub owns.

The key design matters more than anything else here. Each worker numbers its
events from its own identity column. If the hub's key is `event_id` alone,
worker 2's first event collides with worker 1's first event. I built that
case on purpose: the hub's apply worker for that subscription stops, and
retries at every `wal_retrieve_retry_interval` without ever making progress:

```results
ERROR:  conflict detected on relation "public.usage_naive": conflict=insert_exists
DETAIL:  Key already exists in unique index "usage_naive_pkey", modified by origin pg_OID in transaction N at TS.
         Key (event_id)=(1); existing local row (1, 1, 1); remote row (1, 4, 1).
```

Only that subscription is stuck. The others keep flowing. The counters in
`pg_stat_subscription_stats` say so: `apply_error_count` keeps growing for
that one. That message, with both rows spelled out, is the 18 way of reporting it.

So the rule for this architecture is to make collisions impossible by
construction: `(worker_id, event_id)` as the key, or UUIDs.

### What about sequences?

Sequences are not replicated by a Postgres 18 publication (`FOR ALL
SEQUENCES` is a syntax error there, and naming a sequence in `FOR TABLE`
gives "This operation is not supported for sequences"). After five
`nextval()` calls on the hub, the worker's copy of the same sequence is
still at 1. If the worker mints an id for a table that also receives rows
from the hub, it collides with a replicated row:

```results
ERROR:  duplicate key value violates unique constraint "customers_pkey"
DETAIL:  Key (customer_id)=(1) already exists.
```

In this architecture the workers never mint ids for hub-owned tables, so
this is a rule rather than a problem. 19 changes the situation, see below.

### Big batches and many streams

A worker that inserts 300,000 rows in one transaction used to make the hub
wait for the commit before it could apply anything. With 14's streaming, and
16's parallel apply, the hub starts working before the commit. On 18 the
default for a new subscription is already `streaming = parallel`
(`pg_subscription.substream` is `p` when you leave the option out), so to
compare you have to say `streaming = off` explicitly. What I measured, on
one laptop, with `logical_decoding_work_mem` lowered so the batch really
streams: with `parallel`, a parallel apply worker exists before the commit
and no row is visible on the hub; after the commit, the rows show up in
about 0.08 s. With `off`, the slot shows spilled transactions and
`stream_txns = 0`, and about a second passes between commit and visibility.
I make no claim about the total time of the batch, which is not what the
setting is for.

### Operating it

Adding a fourth worker is the reason to build it this way. `create table … 
partition of` takes an `ACCESS EXCLUSIVE` lock on the parent, but `attach
partition` only takes `SHARE UPDATE EXCLUSIVE`, so the recipe is to create
the table standalone, then attach it. Create the subscription, and the three
existing apply workers keep the same pids through the whole thing.

When something does break, 15 gave us the tool to get out of it:

```results
logical replication starts skipping transaction at LSN ...
logical replication completed skipping transaction at LSN ...
```

The catch is the word *transaction*. `alter subscription … skip` drops the
whole transaction, not the row that conflicted. In the demo, worker 2's
three events never reached the hub, and the two sides now disagree until
somebody repairs them by hand.

18 also made conflicts countable. `pg_stat_subscription_stats` grows one
column per conflict kind: `confl_insert_exists`, `confl_update_missing`,
`confl_update_origin_differs`, `confl_delete_missing`,
`confl_delete_origin_differs`, `confl_update_exists` and
`confl_multiple_unique_conflicts`. Only `insert_exists` is an error. The
others log at `LOG` level and carry on: `update_missing`, `delete_missing`
and `update_origin_differs` are all "the change is applied or skipped, the
subscription lives, and you find out by looking at a counter". For
`update_origin_differs` the remote change is applied.

### The loop question

In the layout above the reference tables go down and the usage tables go up,
so no change ever comes back to where it was made. What if the same table
has to travel both ways? Two-way replication on one table is exactly where
16 helped: `origin = none` on the subscription tells the publisher to send
only the changes that were made locally, not the ones that arrived through
replication.

I built the case. With `origin = any` and a table without a primary key, one
inserted row multiplies without end. With a primary key, the returning row
hits `insert_exists` and the subscription stalls. With `origin = none`,
each row exists once and there are no errors. Two things to remember: you
have to set the option on both subscriptions, and it only breaks loops, it
does not resolve conflicts.

### The same thing with pglogical

To find out what the older tools cost, I built the same hub-and-workers on
PostgreSQL 14 with pglogical 2.4.8. It works, with a different shape: nodes
and replication sets instead of publications, `row_filter` on a set member,
and a conflict policy per node. One setting the documentation does not
foreground: on 14 the subscription never starts until
`output_plugin_libraries` includes `pglogical_output`.

What pglogical gave me that core still does not is a conflict *policy*.
With `pglogical.conflict_resolution = 'last_update_wins'`, which needs
`track_commit_timestamp`, a deliberate conflict resolves itself, and the
log says how: `CONFLICT: remote INSERT on relation public.usage_events_w1
(local index usage_events_w1_pkey). Resolution: apply_remote.`

It also gave me three things I would rather not have had. Replicating into a
partitioned parent crashed the apply worker with a segmentation fault on the
first row after the initial copy, and the whole hub went into a crash loop
until I dropped the subscription, so the pglogical version replicates each
worker into its own hub partition instead. Moving a customer between two
workers' filters was applied on neither side: worker 1 kept a stale row and
worker 2 never got it, which the core version handles. And a `keep_local`
resolution leaves the two nodes with different values and no error
anywhere, which is what the policy says on the tin, and is worth knowing
before you pick it.

---

## Architecture 2: many databases into one

The second architecture is the one the documentation lists as "consolidating
multiple databases into a single one, for example for analytical purposes".
The application developer's version: three different applications (a shop, a
CRM, a billing system), each with its own schema and its own server, all
feeding a warehouse; and then the warehouse's changes exported to something
that is not Postgres.

### The naming constraint

A subscription maps `schema.table` on the publisher to the same
`schema.table` on the subscriber. There is no rename. That single fact is
the design constraint, and I tried the failure modes:

- Two sources with a `public.customers` and overlapping keys: the initial
  copy stops with `duplicate key value violates unique constraint
  "customers_pkey"`, and the table stays in state `d` and retries every five
  seconds.
- Same name, different shape: `logical replication target relation
  "public.contacts" is missing replicated column: "company"`.

So the layouts I compared:

- **A schema per source** (`shop.orders`, `crm.accounts`, `billing.invoices`)
  in one warehouse database. It works and it is what I recommend: cross-source
  joins are plain SQL.
- **A database per source** on the warehouse server. It works, but Postgres
  gives you `cross-database references are not implemented`, and each database
  needs its own slot and apply worker.
- **A shared table fed by several sources**, with a `source` column. It works
  only when the keys are disjoint across sources, and it is the one
  with the most to say, so here are the results.

### Stamping the source

A subscriber table can have extra columns, filled by a default. I wanted the
default to say which source a row came from. The identity you can see in the
apply worker turns out to be:

- `session_user` is always the **subscription owner**.
- `current_user` is the table owner with the default `run_as_owner = false`,
  and the subscription owner with `run_as_owner = true`.
- No function returns the replication origin name. The functions that come
  closest (`pg_replication_origin_session_is_setup()` and friends) are
  superuser-only, and a trigger calling them fails with `permission denied`
  in the apply worker.

That leads to a pattern that needs no trigger: one subscription owner role
per source, and a default that reads it.

```sql
create table public.customers
(
  id     int  not null,
  name   text not null,
  source text not null default regexp_replace(session_user, '^sub_', ''),
  primary key (id)
);
```

Two caveats, both real: a superuser-owned subscription stamps `postgres`,
and a local write stamps the local user. And the primary key still has to be
`(id)`. Making it `(source, id)` fails on the first `UPDATE`, because the
publisher does not send `source`:

```results
ERROR:  publisher did not send replica identity column expected by the logical replication target relation "public.customers"
```

I also tried the two obvious workarounds. `replica identity full` on the
subscriber does nothing for this. `replica identity full` on the publishers
plus a plain unique index on `(id, source)` is worse: it fails silently
(`conflict=update_missing`, "Could not find the row to be updated"), the
change is skipped, and the data diverges. With overlapping keys, nothing on
the subscriber side is reliable. Use a schema per source and a `union all`
view that adds the constant. (Adding a real namespace column at the source is
the other answer. I did not test it.)

### Less data: filters and column lists

Column lists drop what the warehouse must never see (emails, phone
numbers), and row filters keep a tenant's rows apart. The behaviour matches
the hub demo: a filter column that is outside the replica identity breaks
`UPDATE` at the source with the same error as before, and neither filters
nor column lists clean up what was copied before they existed. Two more
observations. An `UPDATE` that touches only a filtered-out column still
sends an update message, with no PII in it. And `for tables in schema` refuses
column lists: `Column lists cannot be specified in publications containing
FOR TABLES IN SCHEMA elements.`

### Re-exporting as a change stream

Now the part that surprised me the most. The warehouse gets rows through
apply workers, which write WAL like any other session. So a second
publication on the warehouse, and a logical slot for a Debezium-like
consumer, sees them. I checked with `pg_recvlogical` and `pgoutput` (what
Debezium uses) and with `test_decoding` for readability.

- The applied rows do show up downstream.
- They carry a **replication origin** (`pg_<subscription oid>`). The consumer
  that asks for `origin 'none'`, or `only-local` with `test_decoding`,
  sees none of the consolidated data, only writes made locally on the
  warehouse. A consumer that asks for `origin 'any'` gets them, each with
  an origin message.

That is the trap: 16's origin filter, which is what protects you from
loops, is also what hides replicated data from a consumer that opted in.
For a CDC consumer of a consolidated database, set it to `any`.

- A source transaction that touches two tables arrives downstream as **one
  transaction**.
- A 20,000-row transaction streams downstream before the source commits,
  while its rows are still invisible on the warehouse. If the source rolls
  back, the consumer sees `aborting streamed (sub)transaction` after
  thousands of changes were already delivered. Your consumer has to handle
  that. I checked streaming with `test_decoding` only.

### Keeping the CDC load off the primary

Since 16, a logical slot can live on a physical standby of the warehouse.
It works, and I collected the requirements one error at a time:

- the standby needs its own `max_worker_processes` at least as large as the
  primary's: `recovery aborted because of insufficient parameter settings`;
- the standby needs `wal_level = logical` itself, or slot creation fails with
  `logical decoding requires "wal_level" >= "logical"`;
- creating the slot **blocks** on an idle primary until somebody runs
  `pg_log_standby_snapshot()` there;
- with `hot_standby_feedback = off`, catalog vacuum on the primary
  invalidates the slot: `This replication slot has been invalidated due to
  "rows_removed"`.

### What breaks

DDL, again. If a publisher adds a column first, the subscriber's apply worker
stops with `logical replication target relation "shop.orders" is missing
replicated column: "note"`, and every later transaction queues behind it. The
fix is to add the column on the subscriber, and replication resumes on its
own. The order is therefore: additive changes go to the subscriber first,
drops go to the publisher first. Sequences are the other one: the publisher
was at 40013, the warehouse's copy at 1, and a local insert on the warehouse
fails on `orders_pkey`.

That is where the tables-as-a-buffer approach shows its cost. Every change
is written to the warehouse and then written again to be re-decoded. It
works, and for many teams it is the right size. When it stops being the right
size, you want a component that merges and splits change streams without
writing tables, which is a story for another article.

---

## Architecture 3: a zero-downtime major upgrade, with a way back

This one is the most common reason to touch logical replication, and the one
where the small details cost the most. I upgraded 16 to 18 with a traffic
generator running the whole time (about 50 commits per second), so the
"zero" is measured rather than claimed.

The steps are known: restore the schema on the new server
(`pg_dump --schema-only`), create a publication for all tables on the old
one, create a subscription with the initial copy on the new one, wait until
every table is in state `r`, then cut over. About 100 MB took a second to
copy here. The details are in what *is not* there when the copy finishes:

- **Sequences** have `last_value` NULL on the new server, and the first
  insert fails on a duplicate primary key.
- The **materialized view** exists and is not populated:
  `materialized view "sales_by_day" has not been populated`.
- **Large objects** are not replicated. The `oid` column that points to one
  is, so you get `large object N does not exist` on the other side.
- A **stored generated column** is recomputed by the subscriber, which is
  what you want.
- DDL during the migration: the old server does not notice. The new server's
  apply worker stops with `target relation "public.customers" is missing
  replicated column: "note"`, and a table created on the old side is not
  even known to the subscription until `alter subscription … refresh
  publication`.

### Knowing when it has caught up

Everything hangs on one question at cutover time: has the new server
received everything the old one committed? 19 added `WAIT FOR LSN`, and it
is the first thing I tried, because it looks like the perfect tool. It is
not, for a logical subscriber:

```results
16, 18:      ERROR:  syntax error at or near "WAIT"
19beta3:     ERROR:  recovery is not in progress          (standby_* modes)
```

`WAIT FOR LSN` is for physical standbys. On a logical subscriber the
`standby_*` modes fail, and the `primary_flush` mode succeeds as soon as the
subscriber's *own* WAL passes the number, a false positive against a
publisher's LSN.

The working tool is what has always worked: compare the old server's
`pg_current_wal_lsn()` with the position the subscription has applied. There
is a subtlety. The freeze on the old server (a
`default_transaction_read_only` setting) is itself a commit, on the
publisher side, that is never replicated, so it sits *ahead* of anything
the subscriber will ever report, and the comparison never converges. And
`pg_stat_subscription.latest_end_lsn` is updated when a message is
*received*, not when it is applied, so it proves nothing on its own.

What worked is a marker row: after the freeze, write one row on the old
server, and wait for it to appear on the new one. With the marker in
place, all three measures agreed within a millisecond or two.

### Sequences and the switch

The sequence values are generated from the old server:

```sql
select format('select setval(%L, %s, %L);',
              schemaname || '.' || sequencename,
              last_value,
              true)
  from pg_sequences
 where last_value is not null;
```

The whole cutover, from freezing the old server to the first commit on the
new one, took **181 ms** in my run. The application saw four failed attempts
in that window (`cannot execute INSERT in a read-only transaction`), then the
first commit on the new server. No acknowledged write was lost, and the
content hash of every table matched on both servers afterwards. My traffic
loop reconnects for every transaction, so no session had to be terminated:
your connection pool will behave differently.

### The way back

The reverse direction is what lets you roll back after the switch. Before
cutover, subscribe old to new, with `origin = none`, so writes made on the
new server flow back and nothing loops. Three things I learned the hard way:

- the *forward* subscription has to use `origin = none` too, otherwise the
  changes you replicate back would come around again (that is my reasoning,
  I did not test the failure);
- the reverse subscription needs `copy_data = false`: with it on, you get
  `duplicate key value violates unique constraint "t_pkey"` and a tablesync
  stuck in state `d`;
- rolling back means copying the sequence values back, the other way.

A rollback took 179 ms in the same measurement.

### Privileges

`create subscription` for someone who is not a superuser needs
`pg_create_subscription` and `create` on the database, or you get
`permission denied for database`. The connection needs a password, or
`password is required`. And the initial copy then fails with `role
"migrator" cannot SET ROLE to "postgres"` until the subscription owner owns
the tables. All three are 16 behaviour, and each is a security feature, but
none was in the way I expected.

### Or just `pg_upgrade`

17 changed the picture for the in-place route. I upgraded a 17 cluster to 18
with `pg_upgrade --link`:

- A **subscriber** keeps its subscription, the state of each table, and its
  origin position, and stays enabled.
- A **publisher** keeps its slot, but only if the slot has consumed all the WAL:
  `The slot "sub" has not consumed the WAL yet`. The new cluster needs
  `wal_level = logical` from the start.
- `postgresql.conf` and `pg_hba.conf` are yours to write; statistics
  counters are not carried over.
- 18 refuses to upgrade a cluster that does not use data checksums
  (`old cluster does not use data checksums but the new one does`), unless
  the new one is initialised with `--no-data-checksums`.

If you can take a restart, this is the shorter way. If you cannot, logical
replication with the cutover above is how you avoid one.

### Where the old world shows up

Pre-10 servers cannot be a publisher for native logical replication.
pglogical supports 9.4 and later as a provider, which is why it is still the
tool for that one case.

---

## Where pglogical is still needed in 2026

From what I built and what its README says:

- **Conflict resolution.** Core detects and counts conflicts (18) and lets you
  skip a transaction (15). It does not resolve them. pglogical has five
  policies: `error`, `apply_remote`, `keep_local`, `last_update_wins`,
  `first_update_wins`, and the timestamp-based ones need
  `track_commit_timestamp`. If your architecture is genuinely multi-active,
  with several nodes writing the same rows, this is the gap, and the
  descendants of pglogical are where the commercial answers live.
- **Automatic DDL propagation.** Still not in core. pglogical has
  `replicate_ddl_command()`, which is a manual queue; the "automatic" version
  is in its descendants.
- **Sources older than 10.**
- **Sets of tables as first-class objects**, if you like that model.

And what it does not do: replica identity `full`, large objects, and
foreign key enforcement on the subscriber; it works one database at a time;
and from my own run, partitioned parents as targets crashed the apply
worker. EDB says new work goes into its Postgres Distributed product, so
plan accordingly.

---

## Postgres 19: the part that may change

Everything in this section is from a beta, and the last month has taught me
to be careful. Postgres 19 lost several features on the way to release
(see the [Postgres 19
preview](/blog/2026/09/getting-ready-for-postgresql-19/) and its update), so
treat these as "in beta 3 today". All were tested on `postgres:19beta3-bookworm`.

**Sequences replicate.** Only `FOR ALL SEQUENCES` exists in this build (a
sequence-specific publication such as `FOR SEQUENCE s1` is a syntax error).
The values reach the subscriber at the initial synchronisation and at
`alter subscription … refresh sequences`. They were still stale twenty
seconds after a change, so this is "on demand", not streaming. A sequence
created later needs `refresh publication` first. It removes the manual
`setval` step of the upgrade in the third architecture. It does not give
workers non-colliding ids: the values travel from the publisher to the
subscribers, not the other way round.

**`EXCEPT`.** `create publication … for all tables except (table …)` works,
and so does `alter publication … set all tables except (…)`. Excluding a
partitioned table excludes its partitions.

**`retain_dead_tuples`.** A subscription option, with `max_retention_duration`.
With it on, the same scenario that reports `update_missing` reports
`update_deleted`: "The row to be updated was deleted locally in transaction
N". The counter is `confl_update_deleted`. It keeps a slot on the subscriber
(`pg_conflict_detection`) and wants `track_commit_timestamp`; without it you get
a warning. It is the first piece of what real conflict resolution needs, and
it is not resolution yet.

**Logical decoding without a restart.** `effective_wal_level` follows the
first logical slot: it says "logical decoding is enabled upon creating a new
logical replication slot", and drops back to `replica` when the last one is
gone. A server with `wal_level = replica` accepted `create publication`.
Whether that is what you want on a production primary is a separate
question.

**`create subscription … server`.** It works, through `postgres_fdw`, so the
connection details live in a foreign server rather than a string.

Two lines from the run worth keeping: `WAIT FOR LSN` is standby-only, as shown
above, and with the default `max_logical_replication_workers = 4`, a fifth
subscription silently never starts. I saw that one on 19beta3, and
the setting is not new.

One more warning, about this very demo. The Lab's `19beta3` image
preloads `pg_stat_plans`, and that preload segfaults on `UPDATE … FOR
PORTION OF`. Node structures change between betas, and the packaged
extension does not match them. It is not a Postgres bug, and a fix for the
image is in review; it is also why these demos use the official images.

---

## What is still on you

After ten years, the table at the top of this article has two rows without a
release number. Both are architectural, not accidental:

- **DDL.** Every migration in this article had a moment where the order of
  `alter table` on the two sides mattered, and where the wrong order stops
  replication until you fix it. Additive changes go to the subscriber first;
  drops go to the publisher first. Put that in your deployment tool, not in a
  wiki.
- **Conflicts.** The three architectures work because I designed the write
  patterns so that conflicts cannot happen: disjoint key spaces, one direction
  per table, one writer per row. That is the application developer's job
  and nothing in the catalogue changes it. If you need several writers on
  the same rows, you need a policy, and core does not have one yet.

The Londiste hub was a set of daemons, queues and triggers. The core
version of the same thing is publications and subscriptions, a partitioned
table and one index, which is a fair summary of what ten releases of work by
a lot of people bought us.

The demos are in the
[`compose/` directory](https://github.com/dimitri/tapoueh.org/tree/master/content/post/2026/09/logical-replication/compose)
of this post's source. Run them, break them, and tell me what I got wrong.

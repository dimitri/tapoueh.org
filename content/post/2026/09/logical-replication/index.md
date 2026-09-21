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
that has it. Every release since Postgres 10 has taken a piece of that plumbing and
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

Here is the whole story as a table, written from the release notes of Postgres 10
through 19. Read it as "the first release where this stops needing an
extension or a workaround", for the things that matter to an application.

| You want to… | First in core (Postgres) | Before that |
|---|---|---|
| replicate tables between servers | 10 | pglogical (9.4+), Londiste, Slony |
| replicate `TRUNCATE` | 11 | pglogical |
| publish partitioned tables and subscribe into them | 13 | |
| stream a big transaction before it commits | 14 | wait for commit |
| send only some rows or columns; publish a whole schema | 15 | pglogical `row_filter` and replication sets |
| skip one bad transaction | 15 (`ALTER SUBSCRIPTION … SKIP`) | edit the catalog, or drop the subscription |
| avoid loops in two-way setups | 16 (`origin = none`) | pglogical `forward_origins` |
| decode from a standby; apply in parallel | 16 | |
| keep slots and subscriptions across `pg_upgrade` (from a 17 or later cluster); slots survive a failover | 17 | external tooling |
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
any version, including the Postgres 19 beta.

With `ref_all`, every worker sees all nine customers, along with
`billing_notes`, which is for the finance team and nobody else. That is what
Postgres 15 fixed: a row filter and a column list per worker.

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
into **one table partitioned by `worker_id`**, which has worked since Postgres 13:

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
that one. That message, with both rows spelled out, is the Postgres 18 way of reporting it.

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
this is a rule rather than a problem. Postgres 19 changes the situation, see below.

### Minting ids on the workers

The events need an id that two workers cannot mint twice. The key
`(worker_id, event_id)` I used above is the first answer, and it needs no
coordination at all, at the cost of a wider key on every table and every
foreign key that points at it. If you want a single-column id, here are the
options, simplest first.

**Modulo and offset.** With `n` workers, give worker `k` a sequence that
starts at `k` and steps by a number at least as large as the largest
worker count you will ever have:

```sql
-- on worker 1; worker 2 uses start 2, worker 3 start 3
create sequence mod_seq start 1 increment 10;

create table usage_mod
(
  event_id bigint primary key default nextval('mod_seq'),
  qty      int    not null
);
```

That is the classic approach, and it works on every version and needs
nothing from the replication layer. After three inserts on each worker, the
hub has:

```results
 event_id | minted_by_worker
----------+------------------
        1 |                1
        2 |                2
        3 |                3
       11 |                1
       12 |                2
       13 |                3
       21 |                1
       22 |                2
       23 |                3
```

The worker is `event_id % 10`, all three subscriptions had
`apply_error_count = 0`, and no server ever talked to another to get its
ids. The catch is the increment. It is a promise about the largest fleet you
will ever run: a worker number 11 would start at 11, which is exactly the id
worker 1 already handed out, and the hub would stop on `insert_exists`.
Pick the increment with headroom (`bigint` has room for a step of a
thousand for a very long time), because changing it later means auditing
every id already issued. The ids are also not time-ordered across workers,
and they have gaps, as any sequence does.

**UUIDv7, which is what I would use.** Postgres 18 has `uuidv7()`: a UUID
whose first 48 bits are a millisecond timestamp, followed by random bits.

```sql
create table usage_uuid
(
  event_id uuid primary key default uuidv7(),
  worker   int  not null,
  qty      int  not null
);
```

There is no headroom to plan, no worker number to assign, and no registry of
who owns which range. A worker that joins next year, or a system you merge
into the hub, mints ids that cannot collide. And, unlike the random UUIDs
that gave UUID keys their reputation, these are time-ordered, so new rows
land at the right edge of the index like a sequence's do. I had the workers
insert in turn, a few milliseconds apart, and sorting by `event_id` on the
hub returned the rows in the order they were written, whichever worker wrote
them:

```results
 worker | version
--------+---------
      2 |       7
      3 |       7
      1 |       7
      2 |       7
      3 |       7
      1 |       7
      2 |       7
      3 |       7
      1 |       7
```

The price is 16 bytes instead of 8, and the creation time is now readable
from the id (`uuid_extract_timestamp(event_id)`), which matters if the id
is ever shown to users. The ordering across workers is only as good as their
clocks and the millisecond resolution. On a version before Postgres 18 you generate
the value in the application or with an extension.

**Reserving ranges, the BDR way.** The BDR extension had a sequence access
method that allocated a chunk of values to each node and agreed on new chunks
between nodes when one ran out; its successor, EDB Postgres Distributed, still
has it under the name `galloc`, next to a `snowflakeid` kind that is computed
in memory. That is not in Postgres. The sequence access method patch sets
date back to 2015 and 2016, and a new one was under discussion on the
mailing list in late 2025. As far as I can tell from the Postgres 19 source tree it
has not been committed: there is no sequence access method API in it. What
Postgres 19 does contain is groundwork, a refactoring that moves the sequence WAL
code into its own file, described in its commit message as preparation for a
sequence patch. Until an API lands, ranges are something you build in the
application, or get from PGD.

What Postgres 19's replicated sequences do *not* do is help here: the values travel
from the publisher to the subscribers, and workers minting their own ids
need the opposite.

### Big batches and many streams

A worker that inserts 300,000 rows in one transaction used to make the hub
wait for the commit before it could apply anything. With Postgres 14's streaming, and
Postgres 16's parallel apply, the hub starts working before the commit. On
Postgres 18 the
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

Adding a fourth worker is the reason to build it this way, and the
partition is where the care goes. `create table … partition of` takes an
`ACCESS EXCLUSIVE` lock on the parent, which blocks everything that touches
`usage_events`, including the apply workers already running. `attach
partition` only takes `SHARE UPDATE EXCLUSIVE`. I asked `pg_locks` from
inside a transaction for both:

```results
attach partition:              ShareUpdateExclusiveLock
create table … partition of:   AccessExclusiveLock
```

So the recipe is to create the table standalone first, give it the check
constraint that matches its partition bound (the documentation says this lets
`attach partition` skip the validation scan), attach it, then subscribe:

```sql
create table usage_events_w4 (like usage_events including all);

alter table usage_events_w4
  add constraint w4_only check (worker_id = 4);

alter table usage_events
  attach partition usage_events_w4 for values in (4);

create subscription sub_usage_w4
       connection 'host=worker4 dbname=app user=postgres'
       publication pub_usage;
```

The three existing apply workers keep the same pids through the whole thing.

When something does break, Postgres 15 gave us the tool to get out of it:

```results
logical replication starts skipping transaction at LSN ...
logical replication completed skipping transaction at LSN ...
```

The catch is the word *transaction*. `alter subscription … skip` drops the
whole transaction, not the row that conflicted. In the demo, worker 2's
three events never reached the hub, and the two sides now disagree until
somebody repairs them by hand.

### Reading the conflict counters

Postgres 18 also made conflicts countable. `pg_stat_subscription_stats` has one
row per subscription. Beyond the two error counters
(`apply_error_count`, `sync_error_count`) it now has one column per kind of
conflict. This is what a fresh subscription looks like. I reset the counters
first with `pg_stat_reset_subscription_stats()`, which is what you want to do
before any experiment, since they are cumulative:

```results
-[ RECORD 1 ]-------------------+------------
subname                         | sub_conf_w1
apply_error_count               | 0
sync_error_count                | 0
confl_insert_exists             | 0
confl_update_origin_differs     | 0
confl_update_exists             | 0
confl_update_missing            | 0
confl_delete_origin_differs     | 0
confl_delete_missing            | 0
confl_multiple_unique_conflicts | 0
```

To learn what each counter means, I provoked all seven on a small table
replicated from a worker to the hub, `conf_demo (id int primary key, code text
unique, note text)`, each time by writing the same row on both sides. They fall
in two families:

| Counter | What I did | What happened |
|---|---|---|
| `update_missing` | the hub deleted row 1, the worker updated it | the update was dropped, no error |
| `delete_missing` | the hub deleted row 2, the worker deleted it | nothing to delete, no error |
| `update_origin_differs` | the hub edited row 3, then the worker edited it | the worker's version was applied over the hub's |
| `delete_origin_differs` | the hub edited row 4, the worker deleted it | the delete was applied |
| `insert_exists` | both inserted id 6 | **the apply worker stops** and retries |
| `multiple_unique_conflicts` | both inserted id 7 with the same `code` | **the apply worker stops** and retries |
| `update_exists` | the worker set a `code` the hub holds on another row | **the apply worker stops** and retries |

After the first four, the counters say so and `apply_error_count` is still
zero, because nothing stopped:

```results
-[ RECORD 1 ]---------------+--
apply_error_count           | 0
confl_update_missing        | 1
confl_delete_missing        | 1
confl_update_origin_differs | 1
confl_delete_origin_differs | 1
```

Those four are the dangerous ones, because nothing raised an alarm. The data is
already different on the two sides: row 1's update is lost, and row 3 holds the
worker's note while the hub's edit is gone. A counter that moves here means two
servers are writing the same rows, which is a design problem, not an
operational one.

The other three stop replication, and the log says exactly what to fix. The
duplicate key from the hub's `insert_exists`:

```results
ERROR:  conflict detected on relation "public.conf_demo": conflict=insert_exists
DETAIL:  Key already exists in unique index "conf_demo_pkey", modified locally in transaction N at TS.
         Key (id)=(6); existing local row (6, hub-6, inserted on the hub); remote row (6, worker-6, inserted on the worker).
CONTEXT:  processing remote data for replication origin pg_OID during message type "INSERT" for replication target relation "public.conf_demo" in transaction N, finished at X/X
```

and the `update_exists`, which also names the row the worker was trying to
change (`replica identity (id)=(5)`):

```results
ERROR:  conflict detected on relation "public.conf_demo": conflict=update_exists
DETAIL:  Key already exists in unique index "conf_demo_code_key", modified locally in transaction N at TS.
         Key (code)=(code-8); existing local row (8, code-8, inserted on the hub); remote row (5, code-8, from worker); replica identity (id)=(5).
```

When the incoming row collides on the primary key and on the unique `code`
together, it is counted under its own name, `multiple_unique_conflicts`, not
under `insert_exists`. So a monitor that only watches
`confl_insert_exists` misses it.

While the conflict stands, the apply worker restarts every
`wal_retrieve_retry_interval` and fails again, so `apply_error_count` and the
conflict counter climb together. The fix is on the subscriber: delete or
correct the local row that is in the way, and the next retry succeeds. The
counters stop moving and the row arrives:

```sql
delete from conf_demo where id = 6;
```

If the remote change is the one to give up, `alter subscription … skip`,
shown above, drops it, with the caveat that it drops the whole transaction.

A query to turn the counters into something a person can act on, that lists
only the kinds that happened and says which of them stop replication:

```sql
select c.kind, c.stops_apply, c.what_it_means
  from pg_stat_subscription_stats s
       cross join lateral (values
         ('insert_exists',             s.confl_insert_exists,             true,
          'a row with this key exists locally: fix or delete one side, or SKIP the transaction'),
         ('update_exists',             s.confl_update_exists,             true,
          'the new value violates a unique index on the subscriber: fix the local row that holds it'),
         ('multiple_unique_conflicts', s.confl_multiple_unique_conflicts, true,
          'the incoming row violates more than one unique index'),
         ('update_missing',            s.confl_update_missing,            false,
          'the row to update is not here: the change was dropped, the data diverged'),
         ('delete_missing',            s.confl_delete_missing,            false,
          'the row to delete is not here: harmless if it was deleted on purpose'),
         ('update_origin_differs',     s.confl_update_origin_differs,     false,
          'the row was changed locally: the remote change won'),
         ('delete_origin_differs',     s.confl_delete_origin_differs,     false,
          'the row was changed locally: the delete was applied')
       ) as c(kind, n, stops_apply, what_it_means)
 where s.subname = 'sub_conf_w1' and c.n > 0
 order by c.stops_apply desc, c.kind;
```

```results
           kind            | stops_apply |                                      what_it_means
---------------------------+-------------+------------------------------------------------------------------------------------------
 insert_exists             | t           | a row with this key exists locally: fix or delete one side, or SKIP the transaction
 multiple_unique_conflicts | t           | the incoming row violates more than one unique index: fix the local row(s) holding those keys
 update_exists             | t           | the new value violates a unique index on the subscriber: fix the local row that holds it
 delete_missing            | f           | the row to delete is not here: harmless if it was deleted on purpose
 delete_origin_differs     | f           | the row was changed locally: the delete was applied
 update_missing            | f           | the row to update is not here: the change was dropped, the data diverged
 update_origin_differs     | f           | the row was changed locally: the remote change won
```

Alert on `apply_error_count` and `sync_error_count` moving, because that is
replication stopped. Review the other counters on a schedule, because they mean
replication is running on data that no longer matches.

### The loop question

In the layout above the reference tables go down and the usage tables go up,
so no change ever comes back to where it was made. What if the same table
has to travel both ways? Two-way replication on one table is exactly where
Postgres 16 helped: the `origin` option of `create subscription`. With `origin = none`
the publisher sends only the changes that were made locally on it, not the ones
that arrived there through replication. See the
[`origin` parameter](https://www.postgresql.org/docs/current/sql-createsubscription.html#SQL-CREATESUBSCRIPTION-PARAMS-WITH-ORIGIN)
of `create subscription` in the documentation.

The setup is two subscriptions, one in each direction, on the same publication.
On the hub, receive the worker's changes:

```sql
create subscription sub_set_from_w1
       connection 'host=worker1 dbname=app user=postgres'
       publication pub_set
       with (origin = none, copy_data = false);
```

and on the worker, the mirror image:

```sql
create subscription sub_set_from_hub
       connection 'host=hub dbname=app user=postgres'
       publication pub_set
       with (origin = none, copy_data = false);
```

Both tables start empty in this demo, hence `copy_data = false`; with rows on
both sides, the documentation has a section on initial data that is worth
reading first. I ran three rounds on the same pair of tables, one with a
primary key and one without:

| Subscriptions | Table without a primary key | Table with a primary key |
|---|---|---|
| `origin = any` (the default) | one row inserted once multiplies, forever | the returning row hits `insert_exists` and the subscription stalls |
| `origin = none`, both directions | each row exists once | each row exists once, `apply_error_count = 0` |

Two things to remember: you have to set the option on both subscriptions, and
it only breaks loops. It does not resolve conflicts, and the conflict
counters above still apply if both sides write the same row.

### The same thing with pglogical

Before 15 and 16, this architecture meant pglogical. To find out what that
cost, I built the hub-and-workers again on PostgreSQL 14, with two workers,
and here is exactly what I used, since the crash below depends on it.

{{< image src="fig-pglogical.svg" title="The same hub and workers with pglogical. A replication set on the provider holds the tables, with the row filter and the column list on the set membership, and each subscriber subscribes to a set." >}}

**Installation.** The official `postgres:14` Docker image (PostgreSQL 14.24,
Debian 13) already has the PGDG apt repository configured, so pglogical is one
package, version 2.4.8:

```
FROM postgres:14
RUN apt-get update \
 && apt-get install -y --no-install-recommends postgresql-14-pglogical
```

```results
postgres (PostgreSQL) 14.24 (Debian 14.24-1.pgdg13+2)
postgresql-14-pglogical 2.4.8-1.pgdg13+1
```

**Server settings.** Each node runs with:

```
shared_preload_libraries = 'pglogical'
wal_level = logical
track_commit_timestamp = on
output_plugin_libraries = 'pglogical_output'
```

The last line is the one to explain, because without it nothing starts:
`could not create replication slot on provider: ERROR:  library
"pglogical_output" may not be used as an output plugin`. It is a security
hardening that first shipped in the 14.24 minor release: until then, users with
the `REPLICATION` privilege were not subject to the restrictions that `LOAD`
applies to library paths, and could ask for any library as the output plugin of
a logical slot. The new setting,
`output_plugin_libraries`, lists the plugins the server trusts for that, and
the default is `'pgoutput, test_decoding'`, the two that ship with Postgres.
Every third-party plugin, and `pglogical_output` is one, must now be added by the
administrator. It is documented in the
[`output_plugin_libraries`](https://www.postgresql.org/docs/devel/runtime-config-replication.html#GUC-OUTPUT-PLUGIN-LIBRARIES)
entry of the replication settings, along with a query on `pg_replication_slots`
that lists the plugins your existing slots need before you upgrade. It is in the
14 and 19 branches of the source; look for it in the minor release you run.

**The hub.** It is a node, with a replication set per worker. The row filter and
the column list live on the membership of the table in the set, not in a
publication:

```sql
create extension pglogical;

select pglogical.create_node(node_name := 'hub',
                             dsn := 'host=pghub dbname=app user=postgres');

select pglogical.create_replication_set('ref_w1');

select pglogical.replication_set_add_table(
         set_name := 'ref_w1',
         relation := 'customers',
         synchronize_data := false,
         columns := array['customer_id', 'name', 'worker_id', 'plan_id'],
         row_filter := 'worker_id = 1');
```

**A worker.** Also a node. It subscribes to its set, and publishes its own events
through a set named `usage`:

```sql
create extension pglogical;

select pglogical.create_node(node_name := 'worker1',
                             dsn := 'host=pgw1 dbname=app user=postgres');

select pglogical.create_subscription(
         subscription_name := 'sub_ref_w1',
         provider_dsn := 'host=pghub dbname=app user=postgres',
         replication_sets := array['ref_w1'],
         synchronize_data := true,
         forward_origins := '{}');

select pglogical.create_replication_set('usage');
select pglogical.replication_set_add_table('usage', 'usage_events_w1');
```

The hub then subscribes to each worker's `usage` set the same way. Compare
that with the core version above: a publication and a subscription per
worker, no node to declare, no extension, and Postgres 15's row filter and column list
are part of `create publication`. That is the answer to "what did each release
buy": the same architecture, in less to set up, to learn and to keep running.

What pglogical gave me that core still does not is a conflict *policy*. With
`pglogical.conflict_resolution = 'last_update_wins'`, which needs
`track_commit_timestamp`, a deliberate conflict resolves itself, and the log says
how:

```results
LOG:  CONFLICT: remote INSERT on relation public.usage_events_w1 (local index usage_events_w1_pkey). Resolution: apply_remote.
```

It also has three problems, on 2.4.8:

- Replicating into a **partitioned parent** crashed the apply worker on the
  first row after the initial copy. The postmaster restarted every backend,
  and the hub crash-looped until I dropped the subscription:
  `background worker "pglogical apply 16384:SUB" was terminated by signal 11:
  Segmentation fault`. That is why the pglogical version replicates each
  worker into its own hub partition, named like the worker's table, instead of
  into the parent.
- Moving a customer between two workers' filters was applied on neither
  side: worker 1 kept a stale row and worker 2 never got it. The core version
  handles it, as a `DELETE` on one worker and an `INSERT` on the other.
- A `keep_local` resolution leaves the two nodes with different values and no
  error anywhere. That is what the policy says on the tin, and it is worth
  knowing before you pick it.

---

## Architecture 2: many databases into one

The second architecture is the one the documentation lists as "consolidating
multiple databases into a single one, for example for analytical purposes".
The application developer's version: three different applications (a shop, a
CRM, a billing system), each with its own schema and its own server, all
feeding a warehouse; and then the warehouse's changes exported to something
that is not Postgres.

{{< image src="fig-consolidation.svg" title="Three application servers, one schema each, subscribe into one warehouse database that keeps a schema per application. The warehouse then publishes its own change stream, read through a logical slot by a Debezium-like consumer." >}}

### The sources

Each application owns a schema named after it, on its own server. These are
the three, trimmed to the tables that matter here (the demo has the full
DDL and the rows):

```sql
-- on the shop server
create schema shop;
create table shop.customers
(
  id         int primary key,
  account_id int  not null,
  name       text not null,
  email      text,          -- personal data, the warehouse will not get it
  phone      text,
  country    text,
  tenant     text not null
);
create table shop.orders
(
  id          serial primary key,
  customer_id int            not null references shop.customers(id),
  amount      numeric(10, 2) not null,
  status      text           not null,
  tenant      text           not null
);
create publication pub_shop for table shop.orders;

-- on the crm server
create schema crm;
create table crm.accounts (id int primary key, name text not null, tier text not null);
create table crm.contacts
(
  id         int primary key,
  account_id int  not null references crm.accounts(id),
  name       text not null,
  email      text
);
create publication pub_crm for tables in schema crm;

-- on the billing server
create schema billing;
create table billing.invoices
(
  id         serial primary key,
  account_id int            not null,
  amount     numeric(10, 2) not null,
  status     text           not null
);
create table billing.payments
(
  id         serial primary key,
  invoice_id int            not null references billing.invoices(id),
  amount     numeric(10, 2) not null
);
create publication pub_billing for tables in schema billing;
```

The warehouse creates the same schemas and tables, by hand, because DDL
is not replicated, and one subscription per application. Each subscription is
owned by its own role, which will matter in a moment:

```sql
create role sub_shop login password 'x' in role pg_create_subscription;
grant create on database warehouse to sub_shop;
create schema shop authorization sub_shop;
create table shop.orders
(
  id          serial primary key,
  customer_id int            not null,
  amount      numeric(10, 2) not null,
  status      text           not null,
  tenant      text           not null
);
alter table shop.orders owner to sub_shop;

set role sub_shop;
create subscription sub_shop
       connection 'host=shop dbname=shop user=repl password=repl'
       publication pub_shop;
```

and the same for `sub_crm` and `sub_billing`. Once the initial copy is done,
the applications' data sits side by side, and a query across applications is
plain SQL. Revenue per CRM account, from the CRM's accounts and the billing
system's invoices:

```results
 account |  tier  | invoiced | status
---------+--------+----------+--------
 Acme    | gold   |   150.00 | paid
 Globex  | silver |    75.00 | open
 Initech | bronze |    20.00 | open
```

### A schema per application, and why it must start at the source

The warehouse keeps a schema per application, as you would want. But the way
it gets there is not what you might expect: **a subscription cannot rename
anything.** It looks for the publisher's `schema.table` under the same name on
the subscriber. The subscriber's code does a plain lookup with the names the
publisher sent (`RangeVarGetRelid` on the remote namespace and relation name,
in `replication/logical/relation.c`), and neither `create subscription` nor
`create publication` has an option to map one name to another.

That matters because the typical application does not have a schema of its
own: its tables are in `public`. I tried the natural thing. The shop has
`public.orders`, and the warehouse has `shopapp.orders`, where I want it.
The figure shows that attempt, and the one that works:

{{< image src="fig-schema-rename.svg" title="A subscription looks up the publisher's own schema and table name on the subscriber. Tables in public on the publisher cannot land in shopapp on the warehouse. Tables moved to their own schema on the publisher land in the same schema on the warehouse." >}}

The attempt:

```sql
create schema shopapp;
create table shopapp.orders (id int primary key, customer_id int not null, amount numeric(10,2) not null);

create subscription sub_rename
       connection 'host=shop dbname=shop user=repl password=repl'
       publication pub_rename;
```

```results
ERROR:  relation "public.orders" does not exist
```

The subscription is not even created. What works is to give the application a
schema of its own on the *publisher*, which is a lot less work than it sounds,
because a role's `search_path` keeps the application's unqualified SQL
resolving:

```sql
create schema shopapp;
alter table public.orders set schema shopapp;

create role app_shop login;
alter role app_shop set search_path = shopapp;
```

The publication follows the table, since it tracks it by identity and not by
name, and the application does not notice, connecting with its own role and its
own unqualified SQL:

```results
 pubname    | schemaname | tablename
------------+------------+-----------
 pub_rename | shopapp    | orders

-- as app_shop:  show search_path;  select count(*) from orders;
 search_path
-------------
 shopapp

 count
-------
     2
```

Now the same `create subscription` as before finds `shopapp.orders` on both
sides, and the copy runs. If you have several applications in `public` on
several servers, this is the migration to do first, and it is cheap; if you
cannot touch the source, the alternatives are a database per source on the
warehouse server, or a component that renames as the changes flow, the kind
I come back to at the end of this architecture.

I also tried what happens when you do not do this. Two sources with a
`public.customers` and overlapping keys: the initial copy stops with
`duplicate key value violates unique constraint "customers_pkey"`, and the table
stays in state `d` and retries every five seconds. Same name and a different
shape: `logical replication target relation "public.contacts" is missing
replicated column: "company"`.

### Less data: filters and column lists

This warehouse is the EU warehouse. It must never hold the customers' email
addresses and phone numbers, and it should only receive the `eu` tenant's
rows. Since Postgres 15 the publisher does both, with a column list and a row filter.

{{< image src="fig-filters.svg" title="The publication sits between the two tables. The column list drops email and phone, the row filter drops the us customer. Neither the columns nor the row are ever sent to the warehouse." >}}

The subscriber's copy of `shop.customers` is created without the personal
columns, because a table only needs the columns that will be sent. On the
warehouse:

```sql
create table shop.customers
(
  id         int  primary key,
  account_id int  not null,
  name       text not null,
  country    text,
  tenant     text not null
);
alter table shop.customers owner to sub_shop;
```

On the shop server, the publication is changed to list its tables with their
column lists and row filters. `shop.orders` was already published and gets a
filter, `shop.customers` is new:

```sql
alter publication pub_shop set table
  shop.orders where (tenant = 'eu'),
  shop.customers (id, account_id, name, country, tenant) where (tenant = 'eu');

select schemaname, tablename, attnames, rowfilter
  from pg_publication_tables
 where pubname = 'pub_shop'
 order by tablename;
```

```results
 schemaname | tablename |               attnames                |       rowfilter
------------+-----------+---------------------------------------+-----------------------
 shop       | customers | {id,account_id,name,country,tenant}   | (tenant = 'eu'::text)
 shop       | orders    | {id,customer_id,amount,status,tenant} | (tenant = 'eu'::text)
```

The first trap comes right after. The row filter uses `tenant`, which is not
part of the primary key, and the primary key is the replica identity. The
publication is accepted, and the next `update` on the publisher fails, whatever
column it changes:

```sql
update shop.orders set status = 'paid' where id = 4;
```

```results
ERROR:  cannot update table "orders"
DETAIL:  Column used in the publication WHERE expression is not part of the replica identity.
```

Same fix as in the hub-and-workers demo: a unique index that contains the
filter column, used as the replica identity.

```sql
create unique index orders_id_tenant on shop.orders (id, tenant);
alter table shop.orders replica identity using index orders_id_tenant;

create unique index customers_id_tenant on shop.customers (id, tenant);
alter table shop.customers replica identity using index customers_id_tenant;
```

Then the subscriber picks up the new table. `refresh publication` copies
only tables that are new to the subscription, and `shop.customers` is one, so
its initial copy honours the filter and the column list:

```sql
alter subscription sub_shop refresh publication;
```

```results
 id | account_id | name  | country | tenant
----+------------+-------+---------+--------
  1 |          1 | Alice | FR      | eu
  2 |          1 | Bob   | FR      | eu
  4 |          3 | Dan   | DE      | eu
```

Carol, the US customer, is not there, and neither are the email and phone
columns. The second trap is in `shop.orders`, which was already being
replicated when the filter was added. A filter only applies to what is sent
from then on, so the US order that was copied before it existed is still on the
warehouse:

```sql
select id, tenant, status from shop.orders order by id;
```

```results
 id | tenant | status
----+--------+--------
  1 | eu     | paid
  2 | eu     | paid
  3 | us     | paid
  4 | eu     | paid
```

Row 3 stays until somebody deletes it by hand, and it will not follow later
changes either, since those are filtered out. After `update shop.orders set
status = 'shipped' where id = 3` on the shop, the warehouse's row 3 still says
`paid`.

### What the publisher sends

The column list is a promise that the personal columns never leave the
publisher, and I wanted to check it on the wire and not only on the subscriber.
A scratch logical slot with the `pgoutput` plugin, read with
`pg_logical_slot_get_binary_changes()`, returns the protocol messages the
subscriber would get. The function below lists the first byte of every message
(`B` begin, `R` relation, `I` insert, `U` update, `D` delete, `C` commit) and
whether any message contains an email or a phone number:

```sql
create function peek(slot text default 'peek', pub text default 'pub_shop')
  returns table (messages text, pii_bytes boolean)
  language sql as $$
  select coalesce(string_agg(chr(get_byte(data, 0)), '' order by lsn), '(nothing)'),
         coalesce(bool_or(encode(data, 'escape') ~ '(@example\.com|\+33|\+49|\+1 555)'), false)
    from pg_logical_slot_get_binary_changes(slot, null, null,
                                            'proto_version', '1',
                                            'publication_names', pub)
$$;

select pg_create_logical_replication_slot('peek', 'pgoutput');
```

Then one statement at a time, followed by `select * from peek();`:

| Statement on the publisher | Messages | PII on the wire |
|---|---|---|
| `update shop.customers set phone = '…' where id = 1` (a column that is not published) | `BRUC` | no |
| `update shop.customers set name = 'Alicia' where id = 1` | `BRUC` | no |
| `update shop.customers set tenant = 'us' where id = 4` (the row leaves the filter) | `BRDC` | no |
| `update shop.customers set tenant = 'eu' where id = 3` (the row enters the filter) | `BRIC` | no |
| `insert into shop.orders … 'us'` | nothing | no |
| `update shop.orders set status = 'shipped' where id = 3` (a row outside the filter) | nothing | no |
| `insert into shop.orders … 'eu'` | `BRIC` | no |

Three things to read in that table. An `update` of a column that is not in the
list still sends an update message, only without the value: the subscriber gets
a no-op update. A row that leaves the filter arrives as a `delete` and a row
that enters it as an `insert`, so the warehouse follows the tenant changes. And
a change to a row outside the filter sends nothing.

To be sure the check can fail, a control: a publication on the same table
*without* a column list, and the same phone update:

```results
 messages | pii_bytes
----------+-----------
 BRUC     | t
```

The warehouse ends up consistent with the filter, apart from the row that
predates it:

```results
 id | account_id |  name  | country | tenant
----+------------+--------+---------+--------
  1 |          1 | Alicia | FR      | eu
  2 |          1 | Bob    | FR      | eu
  3 |          2 | Carol  | US      | eu
```

(Carol is `eu` now, because the demo moved her tenant. Dan, moved to `us`, was
deleted from the warehouse.)

### Filters when the publication is a whole schema

The CRM's publication is `for tables in schema crm`, which is convenient: a
new table in the schema is published without anybody touching the publication.
It comes with a limit. A column list is refused in a publication that has a
`tables in schema` element:

```sql
alter publication pub_crm add table crm.contacts (id, account_id, name);
```

```results
ERROR:  cannot use column list for relation "crm.contacts" in publication "pub_crm"
DETAIL:  Column lists cannot be specified in publications containing FOR TABLES IN SCHEMA elements.
```

To hide the email of a contact you leave the schema form and list the tables,
which gives up the "new tables follow automatically" behaviour:

```sql
alter publication pub_crm drop tables in schema crm;
alter publication pub_crm add table crm.accounts, crm.contacts (id, account_id, name);
```

```results
 tablename |       attnames
-----------+----------------------
 accounts  | {id,name,tier}
 contacts  | {id,account_id,name}
```

And as with the row filter, it applies from now on. A contact inserted after
the change arrives without its email, and the emails copied before stay where
the initial copy put them:

```sql
insert into crm.contacts values (3, 3, 'Jane', 'jane@initech.example');
```

```results
 id | name |        email
----+------+---------------------
  1 | Wile | wile@acme.example
  2 | Hank | hank@globex.example
  3 | Jane |
```

If the personal data must not be on the warehouse at all, the order matters:
set the column list *before* the first copy of the table, or clean the
subscriber up yourself.

### Re-exporting as a change stream

The warehouse gets its rows through apply workers, and apply workers write WAL
like any other session. So the warehouse tables are ordinary tables as far as
logical decoding is concerned: a publication on them and a logical slot give a
Debezium-like consumer the consolidated stream.

{{< image src="fig-cdc.svg" title="The apply workers write the changes of the three sources into the warehouse. Logical decoding reads them from the WAL like any other change, together with the writes made directly on the warehouse. The origin option of the consumer decides which of the two it gets." >}}

On the warehouse, one publication for the three schemas, and two slots created
with the client tools of the server: `pgoutput`, which is what Debezium uses,
and `test_decoding`, which is readable:

```sql
create publication cdc_pub for tables in schema shop, crm, billing;
```

```sh
pg_recvlogical -U postgres -d warehouse --slot cdc_pg --create-slot -P pgoutput
pg_recvlogical -U postgres -d warehouse --slot cdc_td --create-slot -P test_decoding
```

A slot belongs to one database, so a slot created in the `postgres` database
sees nothing of `warehouse`. Then some activity on the sources: one transaction
on the shop that inserts a customer and an order together, and later a write
made directly on the warehouse, to a payment. Reading the `test_decoding` slot
without consuming it:

```sql
select data
  from pg_logical_slot_peek_changes('cdc_td', null, null,
                                    'include-xids', '0',
                                    'skip-empty-xacts', '1');
```

```results
 BEGIN
 table shop.customers: INSERT: id[integer]:10 account_id[integer]:3 name[text]:'Ivy' country[text]:'FR' tenant[text]:'eu'
 table shop.orders: INSERT: id[integer]:7 customer_id[integer]:10 amount[numeric]:42.00 status[text]:'new' tenant[text]:'eu'
 COMMIT
 ...
 BEGIN
 table billing.payments: INSERT: id[integer]:900 invoice_id[integer]:3 amount[numeric]:1.00
 COMMIT
```

Two things to read there. The applied rows do show up downstream, and the
customer's `email` and `phone` are not in the stream, because the publication
that feeds the warehouse never sent them. And the source transaction that
touched two tables arrives as one downstream transaction.

The rows applied by a subscription carry a **replication origin**, named
`pg_<subscription oid>`, and the consumer chooses whether it wants them. The
same slot, read with `pgoutput` messages, with `origin` set to `any` and to
`none` (the letters are the message types: `B` begin, `O` origin, `R` relation,
`I` insert, `U` update, `C` commit):

```sql
select 'origin any' as option, string_agg(chr(get_byte(data, 0)), '' order by lsn) as messages
  from pg_logical_slot_peek_binary_changes('cdc_pg', null, null,
         'proto_version', '1', 'publication_names', 'cdc_pub', 'origin', 'any')
union all
select 'origin none', string_agg(chr(get_byte(data, 0)), '' order by lsn)
  from pg_logical_slot_peek_binary_changes('cdc_pg', null, null,
         'proto_version', '1', 'publication_names', 'cdc_pub', 'origin', 'none');
```

```results
   option    |           messages
-------------+-------------------------------
 origin any  | BORIRICBOICBOUCBORICBORICBRIC
 origin none | BRIC
```

With `origin = any` the consumer gets every transaction, each applied one
preceded by an `O` message with the origin. With `origin = none` it gets only
`BRIC`, the payment written locally on the warehouse. `test_decoding` has the
same switch under another name: `only-local`. That is the trap: Postgres 16's
origin filter, which is what protects you from loops, is also what hides
replicated data from a consumer that asks for it. A consumer of a consolidated
database must use `any`.

Large transactions stream. With `logical_decoding_work_mem` at 64kB, a source
transaction that inserts 20,000 rows reaches the downstream slot while it is
still open, with the option that asks for it (`stream-changes` for
`test_decoding`). The rows are not visible on the warehouse yet, because the
source transaction has not committed:

```results
downstream already streams while the source transaction is open: yes
rows tagged 'bulk_c' visible on the warehouse while the source transaction is open: 0
streamed changes delivered before the end of the transaction: more than 10000
rows tagged 'bulk_c' visible on the warehouse at the end: 20000
```

If the source rolls back instead, the downstream consumer has already received
thousands of changes, and the last line it sees is `aborting streamed
(sub)transaction`. The consumer has to be able to throw them away. I checked
streaming with `test_decoding` only, not with `pgoutput`.

### Keeping the CDC load off the primary

Since Postgres 16, a logical slot can live on a physical standby, which keeps
the CDC consumer off the warehouse primary.

{{< image src="fig-standby.svg" title="The logical slot lives on the physical standby and decodes the WAL the standby replays. The standby reports its needs to the primary with hot_standby_feedback, so that the primary keeps the catalog rows the slot needs." >}}

The standby is a base backup of the warehouse. `-R` writes the recovery
settings, and `-C -S` creates the physical slot on the primary:

```sh
pg_basebackup -h warehouse -U postgres -D $PGDATA -X stream -R -C -S standby1 -v
```

Creating the logical slot on it with `pg_recvlogical` needs four things, and I
collected them one error at a time:

- The standby needs the primary's sizing. With the default `max_worker_processes`
  it does not start: `FATAL: recovery aborted because of insufficient parameter
  settings`, `DETAIL: max_worker_processes = 8 is a lower setting than on the
  primary server, where its value was 24.`
- It needs `wal_level = logical` itself, it does not inherit it from the primary:
  `ERROR: logical decoding requires "wal_level" >= "logical"`.
- Creating the slot **blocks** on an idle primary, until a running-transactions
  record reaches the standby. On the primary:

  ```sql
  select pg_log_standby_snapshot();
  ```

- It needs `hot_standby_feedback = on`. With it off, catalog vacuum on the
  primary removes rows the slot needs and the slot is invalidated:

```results
 slot_name | wal_status | conflicting | invalidation_reason
-----------+------------+-------------+---------------------
 cdc_sb    | lost       | t           | rows_removed

ERROR:  can no longer access replication slot "cdc_sb"
DETAIL:  This replication slot has been invalidated due to "rows_removed".
```

So the standby's `postgresql.conf` carries three settings that the primary does
not force on it:

```
max_worker_processes = 24          # at least the primary's
wal_level = logical
hot_standby_feedback = on
```

### What breaks

DDL, again, and this time it is not a design decision you can avoid: the
schema of the warehouse has to follow the schema of the sources by hand.

{{< image src="fig-ddl-order.svg" title="DDL is not replicated. For an added column the subscriber gets it first, otherwise its apply worker stops. For a dropped column the publisher goes first, and the subscriber keeps a column that stays NULL." >}}

If the publisher adds a column that the subscriber does not have yet, on the
shop:

```sql
alter table shop.orders add column note text;
insert into shop.orders (customer_id, amount, status, tenant, note) values (1, 5.00, 'new', 'eu', 'first with note');
insert into shop.orders (customer_id, amount, status, tenant) values (2, 6.00, 'new', 'eu');
```

the subscriber's apply worker stops, and every later transaction queues behind
the first one, including the second insert, which has nothing to do with the
new column:

```results
ERROR:  logical replication target relation "shop.orders" is missing replicated column: "note"

 subname  | apply_is_failing
----------+------------------
 sub_shop | t

 orders_after_note_insert
--------------------------
                        0

 slot_name | unconfirmed_wal
-----------+-----------------
 sub_shop  | t
```

The publisher keeps that WAL until the subscriber catches up, which is what the
last line shows. The fix is to add the column on the subscriber, and the worker
retries by itself at the next `wal_retrieve_retry_interval` (5 seconds by
default):

```sql
alter table shop.orders add column note text;   -- on the warehouse
```

```results
 amount |      note
--------+-----------------
   5.00 | first with note
   6.00 |
```

So the order for an added column is the subscriber first, then the publisher,
and the same test with that order does not stop:

```sql
alter table shop.orders add column priority int;   -- on the warehouse, then on the shop
```

Dropping a column goes the other way. The publisher goes first, and the
subscriber keeps a column that is NULL for the new rows, until you drop it:

```results
 amount | priority
--------+----------
   7.00 |        1
   8.00 |
```

Sequences are the other thing that does not follow. The publisher's sequence
was at 40013, the warehouse's copy at 1, and a local insert on the warehouse
reuses an id that a replicated row already has:

```results
ERROR:  duplicate key value violates unique constraint "orders_pkey"
DETAIL:  Key (id)=(1) already exists.
```

That is where the tables-as-a-buffer approach shows its cost. Every change
is written to the warehouse and then written again to be re-decoded. It
works, and for many teams it is the right size. When it stops being the right
size, you want a component that merges and splits change streams without
writing tables, which is a story for another article.

---

## Architecture 3: a zero-downtime major upgrade, with a way back

This one is the most common reason to touch logical replication, and the one
where the small details cost the most. I upgraded Postgres 16 to 18 with a traffic
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
received everything the old one committed? Postgres 19 added `WAIT FOR LSN`, which
looks like the perfect tool for this. It is not, for a logical subscriber:

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
new server flow back and nothing loops. Three things to know:

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
the tables. All three are Postgres 16 behaviour, and each is a security feature, but
none was in the way I expected.

### Or just `pg_upgrade`

`pg_upgrade --link` has been there since 9.0, so the in-place route is not new.
What Postgres 17 added is that `pg_upgrade` carries the logical replication
state across: its release notes say it migrates valid logical slots and
subscriptions, and that this only works when the *old* cluster is version 17 or
later. I upgraded a Postgres 17 cluster to 18 with `pg_upgrade --link`:

- A **subscriber** keeps its subscription, the state of each table, and its
  origin position, and stays enabled.
- A **publisher** keeps its slot, but only if the slot has consumed all the WAL:
  `The slot "sub" has not consumed the WAL yet`. The new cluster needs
  `wal_level = logical` from the start.
- `postgresql.conf` and `pg_hba.conf` are yours to write; statistics
  counters are not carried over.
- Postgres 18 refuses to upgrade a cluster that does not use data checksums
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

- **Conflict resolution.** Core detects and counts conflicts (Postgres 18) and lets you
  skip a transaction (Postgres 15). It does not resolve them. pglogical has five
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

Two lines from the run worth keeping. `WAIT FOR LSN` is standby-only, as shown
above. And a hub with many subscriptions runs out of two limits without a
friendly error: `max_logical_replication_workers` (the default is 4), and,
since Postgres 18, `max_active_replication_origins` (the default is 10), because every
subscription and every table being copied holds a replication origin. Past
that, the log repeats `could not find free replication state slot for
replication origin with ID 11`, and the subscriptions sit in the `i` or `d` state
forever. My hub-and-workers demo hit the second limit at step 21, which is
why its compose file sets both.

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

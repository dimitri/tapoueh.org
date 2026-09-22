+++
title     = "Ten years of Postgres logical replication"
date      = "2026-09-22T09:00:00+0200"
tags      = ["PostgreSQL", "Replication", "Logical Decoding", "Architecture"]
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
that has it. Every release since Postgres 10 has taken a piece of that
plumbing and made it a line of SQL.

This is the first article in a series about Postgres logical replication
use-cases, and about how the feature set has evolved over the past ten
years and ten releases. The question is the application developer's one,
not the DBA's: *which architectures can I deploy with Postgres core alone
today, what does each release change about that, and where do I still need
something else?* I built three architectures for real, across three posts:

1. **Hub and workers**, spreading the write load across servers — this post.
2. **Consolidation**: many databases, different applications and schemas,
   into one, then re-exported as a change stream for a CDC consumer.
3. **Zero-downtime major upgrade**, with a way back.

A fourth post, covering what is left out of this series in less detail —
geo-replication, BDR-style multi-active setups, plain CDC and triggers — is
also planned.

<!--more-->

{{< lab >}}
Everything below ran, and the setup is kept so you can run it again: a
`docker-compose.yml`, a numbered `sql/` directory, and the raw output of
every step in `results/`, in the
[`compose/` directory of this post](https://github.com/dimitri/tapoueh.org/tree/master/content/post/2026/09/logical-replication/compose).
Each post in this series has its own demo, in its own `compose/` directory.
This one starts with `make clean && make up && make run`. It uses the
official `postgres` and `postgres:14` images rather than the Lab image,
because the demo needs several servers side by side and nothing from the
Lab dataset. Quoted output is copied from those `results/` files. The Lab
image itself has one relevant caveat, covered later in this series: it
preloads `pg_stat_plans`, which crashes on a beta of Postgres 19.
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

## Hub and workers: spreading the write load

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

### Why not just use Citus?

Everything above builds write scaling out of core logical replication and a
naming convention. [Citus](https://docs.citusdata.com/en/stable/get_started/concepts.html)
is a purpose-built extension for exactly this problem, so it is worth being
honest about what it would have bought, and what it would have cost.

Citus turns a cluster of Postgres servers into one **coordinator** and
several **workers**. In the topology Citus shipped for most of its history,
and still the one its own documentation opens with, the application
connects to the coordinator only, never to a worker directly:

{{< image src="fig-citus-arch.svg" title="The classic Citus topology: the application talks to one coordinator, which plans a query, fans it out to the workers holding the relevant shards, and merges their partial results. A reference table is replicated whole to every worker." >}}

A **distributed table** is sharded across the workers by a distribution
column you pick; a **reference table** is instead kept whole and copied to
every worker. Our `plans` and `prices` are exactly a reference table, and
`usage_events` is exactly a distributed table, sharded on `customer_id` or
`worker_id`. Declaring that is two function calls, `create_reference_table()`
and `create_distributed_table()`, not a publication, a row filter and a
column list per worker.

Three things Citus removes that cost real pages above:

- **DDL propagates.** The documentation says it plainly: changing the schema
  of a distributed table cascades to every shard across every worker. Our
  whole "Operating it" section, and the DDL-order rule in the closing one,
  exist because logical replication does not do this.
- **The key-collision problem does not arise.** Citus computes which shard a
  row belongs to from the distribution column; there is no `usage_naive` to
  build by accident, because there is no second server independently
  minting the same id.
- **Adding a worker is a supported operation**, not a lock-mode reading
  exercise. Since Citus 11.0, `citus_rebalance_start()` moves shards to a
  newly added node without blocking reads or writes, which is the built-in
  version of the `attach partition` dance above.

And what it costs. Citus is an extension: installing it, or paying for a
managed offering that has it, is a decision the plain hub-and-workers
version never asks you to make, because every piece of it is core Postgres
from version 10 on, on whatever managed Postgres you already run.

The single-coordinator picture above is also not the whole current story,
and I want to correct myself here rather than leave a stale claim standing.
What used to be a paid-only feature under the name Citus MX — every node's
metadata kept in sync, so any node can plan and run a distributed query, not
only the coordinator — was open-sourced and turned on by default in Citus
11.0.2, released June 2022 (`citus.enable_metadata_sync`, `true` since that
release). The project now calls it **Query From Any Node**. So "the
coordinator is a SPOF" is the classic diagram, not the current default
behaviour.

It does not make the operational question disappear, it moves it. Citus's
own contributor documentation is direct about the cost: connections stop
being one path per node (application to coordinator) and become every node
to every other node, so you size connection limits for that; and a
production deployment still needs something in front handing the
application one connection string that does not care which node it lands
on, which the project's own README says the managed service did not fully
provide as of its last update. Self-hosted, that "something in front" is
exactly a job for `pg_auto_failover`, which has native support for a Citus
formation: `pg_autoctl create coordinator` and `pg_autoctl create worker`
join a coordinator and its workers to the same monitor, each with its own
failover.

Our hub, either way, has no such requirement to begin with: if it is down,
every worker keeps taking its own traffic, and only the invoicing rollup
waits.

**Trying it.** pg_auto_failover's own test suite ships a complete,
runnable example of exactly this: a coordinator pair and two worker
groups, all under one monitor, with a distributed table, a network
partition, and a failover exercised at every level. It runs on Docker
Compose through the project's own test runner,
[`pgaftest`](https://github.com/hapostgres/pg_auto_failover/blob/main/docs/ref/pgaftest.rst):

```sh
git clone https://github.com/hapostgres/pg_auto_failover
cd pg_auto_failover
pgaftest tmux tests/tap/specs/citus_basic_operation.pgaf
```

`pgaftest tmux` brings the whole stack up under Docker Compose and opens a
three-pane session: live `pg_autoctl watch` state, container logs, and a
shell to drive the test steps one at a time (`pgaftest step`). The spec
file it runs, in full:

```
# Test basic Citus cluster operations: coordinator HA, worker HA with two
# worker groups, distributed table writes/reads, and failover at each level.
#
# Ported from tests/test_basic_citus_operation.py
# Predecessor: tests/test_basic_citus_operation.py

cluster {
    monitor
    formation {
        coordinator1a coordinator
        coordinator1b coordinator
        worker1a worker group 1
        worker1b worker group 1
        worker2a worker group 2
        worker2b worker group 2
    }
}

setup {
    wait until primary, secondary in group 0  timeout 90s
    wait until primary, secondary in group 1  timeout 90s
    wait until primary, secondary in group 2  timeout 90s
    promote coordinator1a
    promote worker1a
    promote worker2a
}

teardown {
    compose down
}

#
# test_001: coordinator pair comes up
#

step test_001_init_coordinator {
    wait until coordinator1a state is primary
        and coordinator1b state is secondary
        timeout 90s
}

#
# test_002: worker groups come up
#

step test_002_init_workers {
    wait until worker1a state is primary
        and worker1b state is secondary
        and worker2a state is primary
        and worker2b state is secondary
        timeout 90s
}

step test_002b_wait_metadata_sync {
    exec coordinator1a  sh -c 'for i in $(seq 1 30); do n=$(psql -U docker -d demo -tAc "SELECT count(*) FROM pg_dist_node WHERE metadatasynced = false AND isactive = true"); [ "$n" = "0" ] && exit 0; sleep 2; done; exit 1'
    sql coordinator1a {
        CREATE OR REPLACE FUNCTION public.wait_until_metadata_sync(timeout
        INTEGER DEFAULT 15000) RETURNS void LANGUAGE C STRICT AS 'citus';
    }
    sql coordinator1a { SELECT public.wait_until_metadata_sync(); }
}

#
# test_003: create distributed table
#

step test_003_create_distributed_table {
    sql coordinator1a { CREATE TABLE t1 (a int); }
    sql coordinator1a { SELECT create_distributed_table('t1', 'a'); }
    sql coordinator1a { INSERT INTO t1 VALUES (1), (2); }
}

step test_004_001_fail_worker2 {
    network disconnect worker2a
    wait until worker2b state is wait_primary  timeout 90s
}

step test_004_003_insert_while_wait_primary {
    wait until worker2b state is wait_primary  timeout 90s
    sql coordinator1a { INSERT INTO t1 VALUES (3); }
}

step test_004_004_reconnect_worker2a {
    network connect worker2a
    wait until worker2b state is primary
        and worker2a state is secondary
        timeout 180s
}

step test_005_read_from_workers_via_coordinator {
    sql coordinator1a { SELECT a FROM t1 ORDER BY a ASC; }
    expect { { 1 } { 2 } { 3 } }
}

#
# test_006: write more rows
#

step test_006_writes_to_coordinator_succeed {
    sql coordinator1a { INSERT INTO t1 VALUES (4); }
    sql coordinator1a { SELECT a FROM t1 ORDER BY a ASC; }
    expect { { 1 } { 2 } { 3 } { 4 } }
}

step test_007_fail_worker2b {
    network disconnect worker2b
    wait until worker2a state is wait_primary  timeout 90s
}

step test_007b_reconnect_worker2b {
    network connect worker2b
    wait until worker2a state is primary
        and worker2b state is secondary
        timeout 180s
}

step test_008_read_from_workers_via_coordinator {
    sql coordinator1a { SELECT a FROM t1 ORDER BY a ASC; }
    expect { { 1 } { 2 } { 3 } { 4 } }
}

step test_009_perform_failover_worker2 {
    perform failover group 2
    wait until worker2b state is primary
        and worker2a state is secondary
        timeout 180s
}

step test_010_perform_failover_coordinator {
    perform failover
    wait until coordinator1a state is secondary
        and coordinator1b state is primary
        timeout 90s
}
```

Step `test_002b_wait_metadata_sync` is the Query From Any Node machinery
made visible: it polls `pg_dist_node.metadatasynced` on the coordinator
until every node's copy of the cluster's metadata is current, the same
synchronisation the "any node" feature depends on. `test_009` and
`test_010` fail over a worker group and the coordinator pair in turn,
each driven by `pg_auto_failover`, not by Citus itself.

The deeper difference is what a "worker" is allowed to be. A Citus worker
is a shard-storage node that the coordinator owns; the application is not
meant to know it exists, and Citus is not designed for you to query it on
its own. Our workers are the opposite: full, independent Postgres servers
that a region, a large customer, or a compliance boundary already needs to
run on its own, and that must keep serving local writes with no hub in
sight. If your workers are really just where the rows happen to live, Citus
is the more transparent tool. If they are autonomous by design, logical
replication keeps that autonomy and Citus does not, because its workers are
not meant to run without their coordinator.

Part 2 of this series consolidates several application databases into one
warehouse. Part 3 covers a zero-downtime major upgrade. A fourth post,
covering the remaining architectures in less detail, is also planned.

The demo for this post is in the
[`compose/` directory](https://github.com/dimitri/tapoueh.org/tree/master/content/post/2026/09/logical-replication/compose)
of this post's source. Run it, break it, and tell me what I got wrong.

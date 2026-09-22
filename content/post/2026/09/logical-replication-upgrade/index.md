+++
title     = "Zero-downtime Postgres upgrades with logical replication"
date      = "2026-09-29T09:00:00+0200"
tags      = ["PostgreSQL", "Replication", "Logical Decoding", "Architecture"]
categories = ["PostgreSQL", "Architecture"]
icon      = "🐘"
+++

This is part 3 of a series on what ten releases of Postgres logical
replication (10 through the 19 beta) buy an application developer, one
architecture at a time.
[Part 1](/blog/2026/09/ten-years-of-postgres-logical-replication/) built a
hub-and-workers system for write scalability and has the table of what each
release added, which this post assumes.
[Part 2](/blog/2026/09/consolidating-databases-with-postgres-logical-replication/)
consolidated several application databases into one warehouse. This one is
the most common reason to touch logical replication at all: a major-version
upgrade with no downtime, and a way back if it goes wrong.

<!--more-->

{{< lab >}}
Everything below ran, and the setup is kept so you can run it again: a
`docker-compose.yml`, a numbered `sql/` directory, and the raw output of
every step in `results/`, in the
[`compose/` directory of this post](https://github.com/dimitri/tapoueh.org/tree/master/content/post/2026/09/logical-replication-upgrade/compose).
`make clean && make up && make run` reproduces it. It uses the official
`postgres` images (18.6, 17.11, 16.15, 14.24 and `19beta3`) rather than the
Lab image, because the demo needs several majors side by side. A traffic
generator keeps writing throughout, at about 50 commits per second, so the
"zero" in the title is measured rather than claimed. Quoted output is
copied from those `results/` files. The Lab image itself has one relevant
caveat, see the Postgres 19 section.
{{< /lab >}}

<!--toc-->

---

This one is the most common reason to touch logical replication, and the one
where the small details cost the most. I upgraded Postgres 16 to 18 with a
traffic generator running the whole time (about 50 commits per second), so the
"zero" is measured rather than claimed.

{{< image src="fig-upgrade-setup.svg" title="Before the cutover. The application writes to the old server. A subscription on the new server copies the tables and then follows the changes. A reverse subscription on the old server, prepared in advance, will carry back what the new server writes, for the way back." >}}

### Setting it up

The schema goes first, because DDL is not replicated. The roles come from
`pg_dumpall`, then the database and its schema from `pg_dump`, run with the
*new* major version's tools against the old server:

```sh
pg_dumpall -h old --roles-only | psql -X -q -h new -d postgres
psql -X -h new -d postgres -c 'create database app owner app'
pg_dump -h old -d app --schema-only | psql -X -q -v ON_ERROR_STOP=1 -h new -d app
```

The roles restore reports one error, `role "postgres" already exists`, which is
harmless. The schema dump also carries the publication and the
`REPLICA IDENTITY FULL` settings, which matters below.

On the old server, one publication for everything:

```sql
create publication app_pub for all tables;
```

This is where a table without a primary key shows itself. Publishing changes
nothing until an `update` or a `delete` hits the table:

```results
ERROR:  cannot update table "audit_log" because it does not have a replica identity and publishes updates
HINT:  To enable updating the table, set REPLICA IDENTITY using ALTER TABLE.
ERROR:  cannot delete from table "audit_log" because it does not have a replica identity and publishes deletes
HINT:  To enable deleting from the table, set REPLICA IDENTITY using ALTER TABLE.
```

The fix is `replica identity full`, which logs the whole old row, or a unique
index over `not null` columns used with `replica identity using index`:

```sql
alter table audit_log replica identity full;
```

Then on the new server, the subscription with the initial copy, while the
traffic keeps running on the old one:

```sql
create subscription app_sub
       connection 'host=old dbname=app user=repl password=repl'
       publication app_pub
       with (copy_data = true);
```

and a query to watch the copy. The states are `i` initialize, `d` data being
copied, `f` finished copy, `s` synchronized, `r` ready:

```sql
select srrelid::regclass as tbl, srsubstate
  from pg_subscription_rel
 order by srrelid::regclass::text;
```

```results
[poll] +26ms rel_states: d=2 i=1 r=5
[poll] +546ms rel_states: d=1 r=7
[poll] +1064ms rel_states: r=8
initial sync of all tables finished
```

About 100 MB took a second to copy here. The details are in what *is not*
there when the copy finishes:

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

### Preparing the way back

The way back is set up before the cutover, not after. The idea is a second
subscription in the other direction, so that writes made on the new server
after the switch reach the old one, and the old server stays current and ready to take the traffic
again. Two subscriptions in opposite directions on the same tables loop unless
they use `origin = none`, so both get it. The forward subscription first:

```sql
-- on the new server
alter subscription app_sub set (origin = none);
```

then the reverse one, on the old server, pulling from the new one. The
`app_pub` publication already exists on the new server, since the schema dump
carried it:

```sql
-- on the old server
create subscription app_rev
       connection 'host=new dbname=app user=repl password=repl'
       publication app_pub
       with (copy_data = false, origin = none);
```

I tested it with a row inserted on each side: each arrives on the other side
once, and nothing bounces back. Three things to know:

- the *forward* subscription has to use `origin = none` too, otherwise the
  changes you replicate back would come around again (that is my reasoning,
  I did not test the failure);
- the reverse subscription needs `copy_data = false`: with it on, you get
  `duplicate key value violates unique constraint "t_pkey"` and a tablesync
  stuck in state `d`;
- rolling back means copying the sequence values back, the other way.

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
`pg_current_wal_lsn()` with the position the subscription has applied:

```sql
-- on the old server
select pg_current_wal_lsn();

-- on the new server
select remote_lsn
  from pg_replication_origin_status
 where external_id = 'pg_' || (select oid from pg_subscription where subname = 'app_sub');
```

There is a subtlety. The freeze on the old server (a
`default_transaction_read_only` setting) is itself a commit, on the
publisher side, that is never replicated, so it sits *ahead* of anything
the subscriber will ever report, and the comparison never converges. And
`pg_stat_subscription.latest_end_lsn` is updated when a message is
*received*, not when it is applied, so it proves nothing on its own.

What worked is a marker row: after the freeze, write one row on the old
server, and wait for it to appear on the new one:

```sql
-- on the old server, once frozen
insert into cutover_markers (id) values ('cutover-old-to-new');

-- on the new server, repeated until it returns true
select exists (select 1 from cutover_markers where id = 'cutover-old-to-new');
```

With the marker in place, all three measures agreed within a millisecond or two.

### The cutover

{{< image src="fig-upgrade-cutover.svg" title="The cutover in four steps, with the time at which each one finished, counted from the start of the freeze. The application sees a write stall of 181 ms, the time between its last commit on the old server and its first on the new one." >}}

The freeze makes the database read-only by default, and ends the sessions of
the application role, so that nothing can start a write on the old server.
Reads still work, and the DBA, who overrides the setting in their own session,
is not affected:

```sql
alter database app set default_transaction_read_only = on;

select count(pg_terminate_backend(pid))
  from pg_stat_activity
 where datname = 'app' and usename = 'app';
```

Once the marker shows on the new server, the sequences. The `setval`
statements are generated from the old server and run on the new one:

```sql
select format('select setval(%L, %s, %L);',
              quote_ident(schemaname) || '.' || quote_ident(sequencename),
              last_value,
              true)
  from pg_sequences
 where last_value is not null;
```

```results
select setval('public.customers_id_seq', 53084, true);
select setval('public.invoice_no', 604083, true);
select setval('public.measurements_id_seq', 303084, true);
select setval('public.orders_id_seq', 603084, true);
```

Then the application's connection target changes to the new server. The
timings the script recorded, and what the application saw:

```results
[time] +34 ms old frozen, 0 app sessions terminated
[time] +100 ms new caught up (marker row visible: 98)
[time] +136 ms sequences copied (4 setval statements)
[time] +138 ms app switched to new
WRITE STALL old -> new: 181 ms
failed attempts inside the window: 4 (of 5 logged attempts, incl. the first success)
for scale: median gap between two commits 18 ms, p99 25 ms, over 3244 commits
```

The procedure took 138 ms from the freeze to the switch, and the application
saw a write stall of 181 ms, with four failed attempts (`cannot execute INSERT
in a read-only transaction`) and then its first commit on the new server. No
acknowledged write was lost, and the content hash of every table matched on
both servers afterwards. My traffic loop reconnects for every transaction, so
no session had to be terminated: your connection pool will behave differently.

### Rolling back

{{< image src="fig-upgrade-back.svg" title="After the switch, the reverse subscription applies on the old server whatever the new server writes. To roll back, the same four steps run in the other direction." >}}

Because `app_rev` has been carrying the new server's writes back, the old server
is current, and rolling back is the same four steps in the other direction:
freeze the new server, wait for the marker to show on the old one, copy the
sequence values back, switch the application. It took 179 ms in the same
measurement:

```results
[time] +34 ms new frozen, 0 app sessions terminated
[time] +101 ms old caught up (marker row visible: 99)
[time] +155 ms old unfrozen, sequences copied back (4 statements)
[time] +156 ms app switched back to old
WRITE STALL new -> old: 179 ms
```

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

After ten releases, [the table in part 1](/blog/2026/09/ten-years-of-postgres-logical-replication/#what-each-release-changed)
has two rows without a release number. Both are architectural, not accidental:

- **DDL.** Every migration across this series had a moment where the order of
  `alter table` on the two sides mattered, and where the wrong order stops
  replication until you fix it. Additive changes go to the subscriber first;
  drops go to the publisher first. Put that in your deployment tool, not in a
  wiki.
- **Conflicts.** The three architectures in this series work because I designed the write
  patterns so that conflicts cannot happen: disjoint key spaces, one direction
  per table, one writer per row. That is the application developer's job
  and nothing in the catalogue changes it. If you need several writers on
  the same rows, you need a policy, and core does not have one yet.

The Londiste hub from the start of this series was a set of daemons, queues
and triggers. The core version of the same thing, across the three
architectures, is publications and subscriptions, a partitioned table and
one index, which is a fair summary of what ten releases of work by a lot of
people bought us.

A fourth post, covering the architectures left out of this series in less
detail — geo-replication, BDR-style multi-active setups, plain CDC and
triggers — is planned. Until then, part 1 has the
[hub-and-workers demo](/blog/2026/09/ten-years-of-postgres-logical-replication/),
part 2 the
[consolidation demo](/blog/2026/09/consolidating-databases-with-postgres-logical-replication/),
and this post's own demo is in its
[`compose/` directory](https://github.com/dimitri/tapoueh.org/tree/master/content/post/2026/09/logical-replication-upgrade/compose).
Run them, break them, and tell me what I got wrong.

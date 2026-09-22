+++
title     = "Consolidating databases with Postgres logical replication"
date      = "2026-09-24T09:00:00+0200"
tags      = ["PostgreSQL", "Replication", "Logical Decoding", "Architecture"]
categories = ["PostgreSQL", "Architecture"]
icon      = "🐘"
+++

This is part 2 of a series about Postgres logical replication use-cases,
and about how the feature set has evolved over the past ten years and ten
releases (10 through the 19 beta), one architecture at a time. Part 1 built
a hub-and-workers system for write scalability and has the table of what
each release added, which this post assumes. Part 3 covers a zero-downtime
major upgrade.

<!--more-->

{{< lab >}}
Everything below ran, and the setup is kept so you can run it again: a
`docker-compose.yml`, a numbered `sql/` directory, and the raw output of
every step in `results/`, in the
[`compose/` directory of this post](https://github.com/dimitri/tapoueh.org/tree/master/content/post/2026/09/logical-replication-consolidation/compose).
`make clean && make up && make run` reproduces it. It uses the official
`postgres:18` image, not the Lab image, because the demo needs several
servers side by side and nothing from the Lab dataset. Quoted output is
copied from those `results/` files.
{{< /lab >}}

<!--toc-->

---

This architecture is the one the documentation lists as "consolidating
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

Part 3 of this series covers zero-downtime major upgrades with a way back.
A fourth post, covering the architectures left out of this series in less
detail — geo-replication, BDR-style multi-active setups, plain CDC and
triggers — is also planned.

The demo is in the
[`compose/` directory](https://github.com/dimitri/tapoueh.org/tree/master/content/post/2026/09/logical-replication-consolidation/compose)
of this post's source. Run it, break it, and tell me what I got wrong.

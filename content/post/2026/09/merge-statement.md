+++
title = "PostgreSQL's MERGE: Upsert Done Right"
date = "2026-09-01T00:00:00+00:00"
tags = ["PostgreSQL", "SQL", "MERGE", "DML"]
categories = ["PostgreSQL", "YeSQL"]
+++

A daily stock-level snapshot arrives in a staging table. Some SKUs are
new — insert them. Some changed — update them. Some have vanished from the
supplier's feed — delete them. Three operations, one source, one
transaction. Before PostgreSQL 15 you wired them up separately. `MERGE`
does all three in a single statement.

<!--more-->
<!--toc-->

## What INSERT ON CONFLICT cannot do

`INSERT ... ON CONFLICT` handles two cases: the row does not exist
(insert) or it does (update or do nothing). That covers the classic upsert
but breaks down the moment you need:

- Different update logic depending on the current state of the target row.
- `DELETE` instead of update for certain conditions.
- Processing rows from a source that may include updates, inserts, *and*
  deletes — a full merge from a staging table.

`MERGE` handles all of these.

## Basic syntax

```sql
merge into target t
using source s on t.id = s.id
when matched then
    update set value = s.value, updated_at = now()
when not matched then
    insert (id, value, created_at)
    values (s.id, s.value, now());
```

`target` is the table you are writing to. `source` can be a table, a CTE,
a subquery, or a `VALUES` list. The `ON` clause is the join condition that
decides which target rows match which source rows.

You can have multiple `WHEN MATCHED` and `WHEN NOT MATCHED` clauses with
`AND condition` guards, and they are evaluated in order:

```sql
merge into product_inventory inv
using incoming_shipment   ship on inv.sku = ship.sku
when matched and ship.quantity = 0 then
    delete
when matched then
    update set quantity = inv.quantity + ship.quantity,
               last_received = ship.received_at
when not matched then
    insert (sku, quantity, last_received)
    values (ship.sku, ship.quantity, ship.received_at);
```

Rows that arrive with `quantity = 0` delete the inventory record. Rows
with a positive quantity update it. New SKUs are inserted. One statement,
one pass through the source, one round trip.

## WHEN NOT MATCHED BY SOURCE (PostgreSQL 17)

PostgreSQL 17 added the third standard clause: `WHEN NOT MATCHED BY
SOURCE`. This fires for target rows that have *no corresponding row in
the source*. It lets you delete or update stale target rows as part of the
same merge:

```sql
merge into product_inventory inv
using (select sku, quantity from today_stock) stock
   on inv.sku = stock.sku
when matched then
    update set quantity = stock.quantity
when not matched by target then
    insert (sku, quantity) values (stock.sku, stock.quantity)
when not matched by source then
    delete;
```

This is the full synchronisation pattern: a daily snapshot table becomes
the source of truth, and `MERGE` brings the target into sync — new rows
inserted, changed rows updated, vanished rows deleted. Three operations,
zero loops, one atomic statement.

## MERGE RETURNING (PostgreSQL 17)

PostgreSQL 17 also added `RETURNING` to `MERGE`, following the same
convention as `INSERT ... RETURNING`:

```sql
merge into audit_log log
using events e on log.event_id = e.id
when not matched then
    insert (event_id, payload) values (e.id, e.payload)
returning log.event_id, log.inserted_at;
```

`merge_action()` is a special function available inside `RETURNING` that
tells you which `WHEN` clause fired for each output row — `'INSERT'`,
`'UPDATE'`, or `'DELETE'`:

```sql
returning merge_action(), log.event_id;
```

## MERGE vs INSERT ON CONFLICT

| Situation | Reach for |
|---|---|
| Simple upsert (insert or update one row) | `INSERT ... ON CONFLICT` |
| Upsert from a CTE or bulk source | Either works; MERGE is clearer |
| Multiple update paths depending on state | `MERGE` |
| Need to delete as well as insert/update | `MERGE` |
| Full sync from a staging table | `MERGE` with `NOT MATCHED BY SOURCE` |
| Need to know which action fired per row | `MERGE ... RETURNING merge_action()` |

`INSERT ... ON CONFLICT` remains the right tool for simple, high-throughput
point upserts. `MERGE` is the right tool when the logic has multiple
branches or involves a source relation.

## Requires PostgreSQL 15 or later

`MERGE` was added in PostgreSQL 15 (released October 2022). `MERGE
RETURNING` and `WHEN NOT MATCHED BY SOURCE` require PostgreSQL 17
(October 2024). If you are on an older version, `INSERT ... ON CONFLICT`
and writable CTEs cover most use cases.

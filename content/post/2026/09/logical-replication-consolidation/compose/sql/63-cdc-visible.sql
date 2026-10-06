-- @service: warehouse
-- Step 4d (i) + (iii): what does the downstream slot see? PEEK does not consume.
-- (i) Rows applied by the subscription workers ARE decoded downstream (default options).
-- (iii) Each source transaction is ONE downstream transaction, tables together.
select data from pg_logical_slot_peek_changes('cdc_td', null, null, 'include-xids', '0', 'include-timestamp', '0', 'skip-empty-xacts', '1') order by lsn;

-- the same, folded per downstream transaction
with l as (
  select row_number() over () as n, data
    from pg_logical_slot_peek_changes('cdc_td', null, null, 'include-xids', '0', 'skip-empty-xacts', '1')
), t as (select n, data, count(*) filter (where data like 'BEGIN%') over (order by n) as txn from l)
select txn, count(*) filter (where data like 'table %') as changes,
       string_agg(distinct substring(data from 'table ([a-z_.]+):'), ', ') as tables
  from t group by txn order by txn;

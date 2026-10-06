-- @service: warehouse
-- Step 3c: replication has caught up (the runner waited), the materialized view has not.
select i.id, i.status, (select sum(amount) from billing.payments p where p.invoice_id = i.id) as paid_now
  from billing.invoices i where i.id = 2;
select account_id, name, invoiced, paid from mart.revenue_per_account where account_id = 2;
refresh materialized view concurrently mart.revenue_per_account;
select account_id, name, invoiced, paid from mart.revenue_per_account where account_id = 2;

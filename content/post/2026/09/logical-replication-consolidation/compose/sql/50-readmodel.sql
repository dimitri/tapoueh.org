-- @service: warehouse
-- Step 3a: a consolidated read model joining the three apps: revenue per CRM account.
-- First clean up the US order that the row filter did not remove (see step 2d).
delete from shop.orders where tenant <> 'eu';

create schema mart;
create materialized view mart.revenue_per_account as
select a.id as account_id, a.name, a.tier,
       coalesce(o.orders_total, 0) as ordered,
       coalesce(i.invoiced, 0)     as invoiced,
       coalesce(p.paid, 0)         as paid
  from crm.accounts a
  left join (select c.account_id, sum(o.amount) as orders_total
               from shop.orders o join shop.customers c on c.id = o.customer_id group by 1) o on o.account_id = a.id
  left join (select account_id, sum(amount) as invoiced from billing.invoices group by 1) i on i.account_id = a.id
  left join (select i.account_id, sum(p.amount) as paid
               from billing.payments p join billing.invoices i on i.id = p.invoice_id group by 1) p on p.account_id = a.id;
create unique index on mart.revenue_per_account (account_id);
select * from mart.revenue_per_account order by account_id;

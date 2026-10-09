-- @service: billing
-- Step 3b: business goes on at the sources.
insert into billing.payments (invoice_id, amount) values (2, 75.00);
update billing.invoices set status = 'paid' where id = 2;

-- @service: billing
create schema billing;
create table billing.invoices (id serial primary key, account_id int not null,
                               amount numeric(10,2) not null, status text not null);
create table billing.payments (id serial primary key, invoice_id int not null references billing.invoices(id),
                               amount numeric(10,2) not null);
insert into billing.invoices (account_id, amount, status) values (1, 150.00, 'paid'), (2, 75.00, 'open'), (3, 20.00, 'open');
insert into billing.payments (invoice_id, amount) values (1, 150.00);
create publication pub_billing for tables in schema billing;
create role repl replication login password 'repl';
grant pg_read_all_data to repl;

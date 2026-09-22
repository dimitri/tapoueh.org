-- @service: warehouse
-- @nosync
-- Layout (a): one schema per source app, distinct names at the source, all in
-- database "warehouse". One subscription (and one owner role) per source.
create role sub_shop    login password 'x' in role pg_create_subscription;
create role sub_crm     login password 'x' in role pg_create_subscription;
create role sub_billing login password 'x' in role pg_create_subscription;
grant create on database warehouse to sub_shop, sub_crm, sub_billing;

create schema shop authorization sub_shop;
create schema crm authorization sub_crm;
create schema billing authorization sub_billing;

create table shop.orders (id serial primary key, customer_id int not null, amount numeric(10,2) not null, status text not null, tenant text not null);
create table crm.accounts (id int primary key, name text not null, tier text not null);
create table crm.contacts (id int primary key, account_id int not null, name text not null, email text);
create table billing.invoices (id serial primary key, account_id int not null, amount numeric(10,2) not null, status text not null);
create table billing.payments (id serial primary key, invoice_id int not null, amount numeric(10,2) not null);
alter table shop.orders owner to sub_shop;
alter table crm.accounts owner to sub_crm;
alter table crm.contacts owner to sub_crm;
alter table billing.invoices owner to sub_billing;
alter table billing.payments owner to sub_billing;

set role sub_shop;
create subscription sub_shop connection 'host=shop dbname=shop user=repl password=repl' publication pub_shop;
set role sub_crm;
create subscription sub_crm connection 'host=crm dbname=crm user=repl password=repl' publication pub_crm;
set role sub_billing;
create subscription sub_billing connection 'host=billing dbname=billing user=repl password=repl' publication pub_billing;
reset role;

-- @service: warehouse
-- @nosync
-- Layout (b): one database per source on the warehouse server. Same table
-- names as the source, no renaming needed, but every database is an island.
create database wh_shop;
create database wh_crm;
create database wh_billing;

\c wh_shop
create schema shop;
create table shop.orders (id serial primary key, customer_id int not null, amount numeric(10,2) not null, status text not null, tenant text not null);
create subscription sub_b_shop connection 'host=shop dbname=shop user=repl password=repl' publication pub_shop;

\c wh_crm
create schema crm;
create table crm.accounts (id int primary key, name text not null, tier text not null);
create table crm.contacts (id int primary key, account_id int not null, name text not null, email text);
create subscription sub_b_crm connection 'host=crm dbname=crm user=repl password=repl' publication pub_crm;

\c wh_billing
create schema billing;
create table billing.invoices (id serial primary key, account_id int not null, amount numeric(10,2) not null, status text not null);
create table billing.payments (id serial primary key, invoice_id int not null, amount numeric(10,2) not null);
create subscription sub_b_billing connection 'host=billing dbname=billing user=repl password=repl' publication pub_billing;

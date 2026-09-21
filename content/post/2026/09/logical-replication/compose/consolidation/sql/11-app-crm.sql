-- @service: crm
create schema crm;
create table crm.accounts (id int primary key, name text not null, tier text not null);
create table crm.contacts (id int primary key, account_id int not null references crm.accounts(id),
                           name text not null, email text);
insert into crm.accounts values (1, 'Acme', 'gold'), (2, 'Globex', 'silver'), (3, 'Initech', 'bronze');
insert into crm.contacts values (1, 1, 'Wile', 'wile@acme.example'), (2, 2, 'Hank', 'hank@globex.example');
create publication pub_crm for tables in schema crm;
create role repl replication login password 'repl';
grant pg_read_all_data to repl;

-- @service: crm
-- Step 2g: FOR TABLES IN SCHEMA follows new tables automatically, but takes no column list.
create table crm.notes (id int primary key, account_id int, body text);
select tablename from pg_publication_tables where pubname = 'pub_crm' order by 1;
alter publication pub_crm add table crm.contacts (id, account_id, name);
-- a column list needs a table-list publication: drop the schema, list the tables
alter publication pub_crm drop tables in schema crm;
alter publication pub_crm add table crm.accounts, crm.contacts (id, account_id, name);
select tablename, attnames from pg_publication_tables where pubname = 'pub_crm' order by 1;
insert into crm.contacts values (3, 3, 'Jane', 'jane@initech.example');

-- @service: warehouse
-- Step 4a: re-export. The warehouse tables are ordinary tables as far as logical decoding
-- is concerned: publish them for the downstream (Debezium-like) consumer.
create publication cdc_pub for tables in schema shop, crm, billing;
select pubname, schemaname, tablename from pg_publication_tables where pubname = 'cdc_pub' order by 2, 3;

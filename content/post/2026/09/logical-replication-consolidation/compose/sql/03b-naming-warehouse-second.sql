-- @service: warehouse
-- Step 1d: now add the second source into the very same tables.
-- @nosync
create subscription sub_naming_crm
  connection 'host=crm dbname=crm user=repl password=repl'
  publication pub_naming;

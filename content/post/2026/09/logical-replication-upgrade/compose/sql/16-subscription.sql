-- @service: new
-- Step 16: subscription with copy_data on the NEW server (traffic keeps running
-- on the old one). Created by the superuser; the connection uses the "repl" role.
CREATE SUBSCRIPTION app_sub
  CONNECTION 'host=old dbname=app user=repl password=repl'
  PUBLICATION app_pub
  WITH (copy_data = true);

SELECT subname, subenabled, subslotname, subsynccommit, suborigin, subfailover FROM pg_subscription;
-- states: i initialize, d data being copied, f finished copy, s synchronized, r ready
SELECT srrelid::regclass AS tbl, srsubstate FROM pg_subscription_rel ORDER BY srrelid::regclass::text;

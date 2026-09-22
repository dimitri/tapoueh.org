-- @service: warehouse
-- Step 4e (ii): replication origins. Applied changes carry the origin of the subscription.
\echo -- test_decoding, only-local = 0 (default): everything
select count(*) as changes_seen from pg_logical_slot_peek_changes('cdc_td', null, null, 'include-xids', '0', 'only-local', '0', 'skip-empty-xacts', '1') where data like 'table %';
\echo -- test_decoding, only-local = 1: changes with an origin are dropped
select data from pg_logical_slot_peek_changes('cdc_td', null, null, 'include-xids', '0', 'only-local', '1', 'skip-empty-xacts', '1') order by lsn;
\echo -- pgoutput message types per option (B begin, O origin, R relation, I insert, U update, C commit)
select 'origin any' as option, string_agg(chr(get_byte(data, 0)), '' order by lsn) as messages
  from pg_logical_slot_peek_binary_changes('cdc_pg', null, null, 'proto_version', '1', 'publication_names', 'cdc_pub', 'origin', 'any')
union all
select 'origin none', string_agg(chr(get_byte(data, 0)), '' order by lsn)
  from pg_logical_slot_peek_binary_changes('cdc_pg', null, null, 'proto_version', '1', 'publication_names', 'cdc_pub', 'origin', 'none');
\echo -- the origin names known on this server (one per subscription, pg_<subid>)
select count(*) as origins, bool_and(external_id ~ '^pg_[0-9]+$') as all_named_after_subscriptions from pg_replication_origin_status;

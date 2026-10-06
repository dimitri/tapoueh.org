# @nosync
# Step 5d: the other primitive, pg_logical_emit_message. Publisher side only:
# prove the DDL text actually lands on the wire. No consumer is built here,
# core's own apply worker discards these frames, see the article for why.

echo '### a second event trigger on shop, emitting the same text as a logical message'
sq shop shop -f - <<'SQL'
create function shop.emit_ddl_message() returns event_trigger
  language plpgsql as $f$
begin
  perform pg_logical_emit_message(true, 'ddl', current_query());
end;
$f$;

create event trigger shop_emit_ddl on ddl_command_end
  when tag in ('CREATE TABLE', 'ALTER TABLE')
  execute function shop.emit_ddl_message();
SQL

echo '### a scratch test_decoding slot: it forwards messages with no messages=true needed'
sq shop shop -qc "select pg_create_logical_replication_slot('peek_ddl', 'test_decoding')" </dev/null

echo '### run one DDL statement; both event triggers on shop fire for it'
sq shop shop -qc "alter table shop.orders add column express boolean default false" </dev/null

echo '### what the slot actually holds'
sq shop shop -c \
  "select data from pg_logical_slot_peek_changes('peek_ddl', null, null) where data like 'message:%'" </dev/null

sq shop shop -qc "select pg_drop_replication_slot('peek_ddl')" </dev/null

echo '### for comparison: pgoutput needs messages=true, or it sends nothing at all'
sq shop shop -qc "select pg_create_logical_replication_slot('peek_ddl_pg', 'pgoutput')" </dev/null
sq shop shop -qc "alter table shop.orders add column priority2 int" </dev/null
sq shop shop -c \
  "select count(*) as pgoutput_messages_without_the_option from pg_logical_slot_peek_binary_changes('peek_ddl_pg', null, null, 'proto_version', '1', 'publication_names', 'pub_shop') where get_byte(data, 0) = ascii('M')" </dev/null
sq shop shop -c \
  "select count(*) as pgoutput_messages_with_the_option from pg_logical_slot_peek_binary_changes('peek_ddl_pg', null, null, 'proto_version', '1', 'publication_names', 'pub_shop', 'messages', 'true') where get_byte(data, 0) = ascii('M')" </dev/null
sq shop shop -qc "select pg_drop_replication_slot('peek_ddl_pg')" </dev/null

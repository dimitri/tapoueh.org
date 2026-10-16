-- identical shape everywhere, including the warehouse: the primary key is
-- what makes two regions' writes structurally unable to collide, not a
-- per-node CHECK constraint (that discipline is enforced once, at the
-- application layer, per the post's own "architecture that makes the
-- question moot" section, not at the replication layer).
create table events (
  region     text   not null,
  event_id   bigint not null,
  payload    text,
  written_at timestamptz not null default clock_timestamp(),
  primary key (region, event_id)
);

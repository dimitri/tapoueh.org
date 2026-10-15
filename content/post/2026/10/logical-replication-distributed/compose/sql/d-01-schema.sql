-- the shop/warehouse table from part 2's own demo, reused here unchanged —
-- the point of this profile is the DDL arriving as a message, not a new
-- schema to design.
create table shop (
  id      bigserial primary key,
  name    text not null,
  created_at timestamptz not null default clock_timestamp()
);

insert into shop (name) select 'item ' || g from generate_series(1, 20) g;

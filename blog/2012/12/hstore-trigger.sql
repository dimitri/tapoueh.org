create table foo (
  id serial primary key,
  d_start timestamptz default now(),
  d_end timestamptz,
  duration interval
);

truncate foo;

insert into foo(d_start, d_end)
     select now() - 10 * random() * interval '1 min',
            now() + 10 * random() * interval '1 min'
       from generate_series(1, 10);

select * from foo;

create or replace function tg_duration()
 -- (
 --  start_name    text,
 --  end_name      text
 -- )
 returns trigger
 language plpgsql
as $$
declare
   hash hstore;
   duration interval;
begin
   duration :=  (hash -> TG_ARGV[1])::timestamptz
              - (hash -> TG_ARGV[0])::timestamptz;

   NEW := NEW #= hstore(TG_ARGV[2], duration);

   RETURN NEW;
end;
$$;

create trigger compute_duration
   before insert on foo
 execute procedure tg_duration('d_start', 'd_end');

select hstore(foo) -> 'date' from foo;

with t(x) as (
  select hstore(foo) -> 'date' from foo
)
select x, pg_typeof(x) from t;

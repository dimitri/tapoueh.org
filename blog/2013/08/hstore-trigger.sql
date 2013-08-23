begin;

create table foo (
  id serial primary key,
  d_start timestamptz default now(),
  d_end timestamptz,
  duration interval
);

truncate foo;

create or replace function tg_duration()
 -- (
 --  start_name    text,
 --  end_name      text,
 --  duration      interval
 -- )
 returns trigger
 language plpgsql
as $$
declare
   hash hstore := hstore(NEW);
   duration interval;
begin
   duration :=  (hash -> TG_ARGV[1])::timestamptz
              - (hash -> TG_ARGV[0])::timestamptz;

   NEW := NEW #= hstore(TG_ARGV[2], duration::text);

   RETURN NEW;
end;
$$;

    create trigger compute_duration
     before insert on foo
          for each row
 execute procedure tg_duration('d_start', 'd_end', 'duration');

insert into foo(d_start, d_end)
     select now() - 10 * random() * interval '1 min',
            now() + 10 * random() * interval '1 min'
       from generate_series(1, 10);

select d_start, d_end, duration from foo;
select id, duration from foo;

select duration, extract(epoch from duration),
       hstore(foo) -> 'duration' as duration from foo;

rollback;

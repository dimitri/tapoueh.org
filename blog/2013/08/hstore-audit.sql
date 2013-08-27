begin;

create extension if not exists hstore;

create table example
 (
   id   serial,
   f1   text,
   f2   text
 );

create table audit
 (
  change_date timestamptz default now(),
  before hstore,
  after  hstore
 );

create function audit()
  returns trigger
  language plpgsql
as $$
begin
  INSERT INTO audit(before, after) SELECT hstore(old), hstore(new);
  return new;
end;
$$;

    create trigger audit
      after update on example
          for each row          -- defaults to FOR EACH STATEMENT!
 execute procedure audit();

insert into example(id, f1, f2) values(1, 'a', 'a');
update example set f1 = 'b' where id = 1;
update example set f2 = 'c' where id = 1;

select * from audit;
select change_date, after - before as diff from audit;

rollback;

create function audit_trigger()
  returns trigger
  language plpgsql
as $$
begin
  perform audit_proxy(old, new);
  return new;
end;
$$;

create function audit_proxy(old example, new example)
  returns void
  language plproxy
as $$
  cluster 'local';
  target audit;
$$;

create or replace function audit(old example, new example)
  returns void
  language SQL
as $$
  INSERT INTO audit(before, after) SELECT hstore(old), hstore(new);   
$$;

drop trigger if exists audit on example;

    create trigger audit
      after update on example
          for each row          -- defaults to FOR EACH STATEMENT!
 execute procedure audit_trigger();

create extension if not exists pg_stat_statements;

  select drivers.surname, count(*) as races
    from f1db.results
    join f1db.races using(raceid)
    join f1db.drivers using(driverid)
   where races.year = 2017
group by drivers.surname;

select queryid
  from pg_stat_statements
 where query like 'select drivers.surname%'
   and query not like '%order by%';

create extension if not exists pg_stash_advice;

select pg_create_advice_stash('production');

select pg_set_stashed_advice(
         'production', -5243066567089054587,
         'JOIN_ORDER(drivers results races)'
       );

select * from pg_get_advice_stash_contents('production');

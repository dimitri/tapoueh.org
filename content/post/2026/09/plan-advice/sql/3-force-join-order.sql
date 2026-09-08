set pg_plan_advice.advice = 'JOIN_ORDER(drivers results races)';

 explain (costs off, plan_advice)
  select drivers.surname, count(*) as races
    from f1db.results
    join f1db.races using(raceid)
    join f1db.drivers using(driverid)
   where races.year = 2017
group by drivers.surname;

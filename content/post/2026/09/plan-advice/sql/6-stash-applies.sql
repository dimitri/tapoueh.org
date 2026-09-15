set pg_stash_advice.stash_name = 'production';

 explain (costs off)
  select drivers.surname, count(*) as races
    from f1db.results
    join f1db.races using(raceid)
    join f1db.drivers using(driverid)
   where races.year = 2017
group by drivers.surname;

create extension if not exists pg_stash_advice;

select pg_create_advice_stash('production');

select pg_set_stashed_advice(
         'production', -5243066567089054587,
         'JOIN_ORDER(drivers results races)'
       );

select * from pg_get_advice_stash_contents('production');

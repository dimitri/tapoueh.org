-- Three regions generate uuidv7() keys while their clocks disagree. psql variables: s0 s1 s2 (intervals).
-- Paced at about 3,000 rows a second for about 60 seconds, so that a millisecond of skew means
-- what it means in production. uuidv7(interval) shifts the timestamp, as a skewed clock would.
create extension if not exists pgstattuple;
select set_config('skew.s', concat_ws(',', :'s0', :'s1', :'s2'), false) as _ \gset
drop table if exists t;
create table t (n bigint generated always as identity, id uuid primary key, region smallint not null);
do $$
declare
    shifts interval[] := string_to_array(current_setting('skew.s'), ',')::interval[];
    g int := 0;
begin
    for k in 1 .. 2000 loop                     -- 2000 rounds x 90 rows, 30 ms apart
        insert into t (id, region)
            select uuidv7(shifts[(g + x) % 3 + 1]), (g + x) % 3 from generate_series(1, 90) x;
        g := g + 90;
        commit;
        perform pg_sleep(0.03);
    end loop;
end $$;
select count(*) as rows,
       round(100.0 * count(*) filter (where k < m) / count(*), 1) as pct_behind_the_rightmost_key
  from (select id::text collate "C" as k,
               max(id::text collate "C") over (order by n rows between unbounded preceding and 1 preceding) as m
          from t) q;
select pg_relation_size('t_pkey') as index_bytes,
       round((pgstatindex('t_pkey')).avg_leaf_density::numeric, 1) as avg_leaf_density,
       (pgstatindex('t_pkey')).leaf_pages as leaf_pages;

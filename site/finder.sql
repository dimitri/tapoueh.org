---
--- affect_choice gives all possible solutions for which class and
--- foreign language groups (lv) are possible to choose for a given
--- student
---
--- next_student counts how many places are left in each class and
--- group and sort by places available, the more first
---
--- both views need an UNION ALL to process separately students having
--- D or E as a lv2 from students having an 'external' lv2 (no constraint).
---
--- function add_next_student() takes the first possible choice from
--- the view next_student and apply then in the final_choice table
---
--- function brute_force() repeat add_next_student() while
--- next_student view returns rows.
---

create or replace view affect_choices as
 SELECT s.id, s.lastname, c.class,
        t1.groupe AS lv1, t2.groupe AS lv2, coalesce(s.option, 'PE') as option
   FROM student s
        LEFT JOIN class_options c 
               ON case when s.class is not null
	       	       then c.class = s.class
		       else c.option = coalesce(s.option, 'PE')
		   end

        LEFT JOIN timetable t1
               ON t1.class = c.class
                  AND case when s.lv1 is null or s.lv1 = 'A'
		      	   then substring(t1.groupe, 1, 1) = 'A'

			   else t1.groupe = s.lv1
		       end

        LEFT JOIN timetable t2
               ON t2.class = c.class
                  AND case when s.lv2 in ('D', 'E') 
                           then substring(t2.groupe, 1, 1) = s.lv2

			   when s.lv2 ~ '^[DE][1-2][1-9]$'
			   then t2.groupe = s.lv2
		      end

  WHERE NOT EXISTS
      ( SELECT 1
         FROM group_conflicts gc
        WHERE gc.a = t1.groupe AND gc.b = t2.groupe
      ) 
      AND c.class IS NOT NULL
      AND t1.groupe IS NOT NULL
      AND t2.groupe IS NOT NULL
      AND s.lv2 ~ '^[DE]([1-2][1-9])*$'

UNION ALL

 SELECT s.id, s.lastname, c.class,
        t1.groupe AS lv1, s.lv2, coalesce(s.option, 'PE') as option
   FROM student s
        LEFT JOIN class_options c 
               ON c.option = coalesce(s.option, 'PE')

        LEFT JOIN timetable t1
               ON t1.class = c.class
                  AND case when s.lv1 is null or s.lv1 = 'A'
		      	   then substring(t1.groupe, 1, 1) = 'A'

			   else t1.groupe = s.lv1
		       end

  WHERE   c.class IS NOT NULL
      AND t1.groupe IS NOT NULL
      AND substring(s.lv2 from 1 for 1) NOT IN ('D', 'E')

ORDER BY 1, 3, 4, 5;

create or replace view next_student as
  select a.id       as id,
         a.lastname as lastname,
	 a.class    as class,
	 a.lv1      as lv1,
	 a.lv2      as lv2,
	 a.option   as option,

         c.places -
         (select count(*)
           from final_choice
          where class = a.class) as class_places,

         g1.places - 
         (select count(*)
            from final_choice
           where lv1 = a.lv1) as lv1_places,

         g2.places -
         (select count(*)
            from final_choice
           where lv2 = a.lv2) as lv2_places,

	 s.count as solutions

    from affect_choices a
         JOIN ( 
           SELECT affect_choices.id, count(affect_choices.id) AS count
             FROM affect_choices
         GROUP BY affect_choices.id
              ) s ON a.id = s.id

         JOIN class c   on a.class = c.class
         JOIN groups g1 on a.lv1   = g1.groupe
         JOIN groups g2 on a.lv2   = g2.groupe

         LEFT JOIN final_choice fc on fc.id = a.id

   where fc.id is null
         and substring(a.lv2 from 1 for 1) IN ('D', 'E')

UNION ALL

  select a.id       as id,
         a.lastname as lastname,
	 a.class    as class,
	 a.lv1      as lv1,
	 a.lv2      as lv2,
	 a.option   as option,


         c.places -
         (select count(*)
           from final_choice
          where class = a.class) as class_places,

         g1.places - 
         (select count(*)
            from final_choice
           where lv1 = a.lv1) as lv1_places,

	 -- needed for the union, as we do not have contraint pretend
	 -- we have a lot of available places
	 30 as lv2_places,

	 s.count as solutions

    from affect_choices a
         JOIN ( 
           SELECT a.id, count(a.id) AS count
             FROM affect_choices a
         GROUP BY a.id
              ) s ON a.id = s.id

         JOIN class c   on a.class = c.class
         JOIN groups g1 on a.lv1   = g1.groupe

         LEFT JOIN final_choice fc on fc.id = a.id

   where fc.id is null
         and substring(a.lv2 from 1 for 1) NOT IN ('D', 'E')
	 
order by 10, 7 desc, 8 desc, 9 desc;

create or replace function add_next_student(p_curpath ltree)
  returns next_student
  language plpgsql
as $f$
declare
  v_ns next_student;
begin
  select ns.*
    into v_ns
    from next_student ns 
   where not exists (select 1
                       from tried
                      where path = (p_curpath || ns.id::text))
         and class_places > 0 and lv1_places > 0 and lv2_places > 0
   limit 1;

  if not found
  then
    return null;
  end if;

  insert into final_choice 
       select v_ns.id, v_ns.lastname, 
              v_ns.class, v_ns.lv1, v_ns.lv2, v_ns.option;

  v_ns.class_places := v_ns.class_places - 1;
  v_ns.lv1_places   := v_ns.lv1_places - 1;
  v_ns.lv2_places   := v_ns.lv2_places - 1;

  return v_ns;
end;
$f$;

create or replace function add_next_student()
  returns next_student
  language sql
as $f$
  select add_next_student(''::ltree)
$f$;

create or replace function brute_force(p_truncate boolean)
  returns void
  language plpgsql
as $f$
declare
  v_final_count integer;
  v_last_count  integer;
  v_check boolean := false;
  v_ns record;
begin
  IF p_truncate
  THEN
    truncate final_choice;
  END IF;

  v_final_count := 0;
  while true
  loop
    select (select count(distinct(id)) from affect_choices)
           =
           (select count(id) from final_choice)
      into v_check;

    v_last_count  := v_final_count;
    v_final_count := (select count(*) from final_choice);
    raise notice 'current count: %', v_final_count;

    if v_check or (v_last_count = v_final_count and v_final_count != 0)
    then
     return;
    end if;

    select into v_ns * from add_next_student();
    raise notice 'next student: %', v_ns;
  end loop;
  return;
end;
$f$;

create or replace function brute_force()
  returns void
  language SQL
as $f$
  SELECT brute_force(false);
$f$;

create or replace function backtrack(p_truncate boolean)
  returns void
  language plpgsql
as $f$
declare
  v_final_count    integer;
  v_tried_count    integer;
  v_next_students  integer;
  v_ns             next_student;
  v_ns_last        next_student;
  v_backt_id       integer;

  v_curtree        ltree;
begin
  v_curtree     := '';
  v_final_count := 0;

  -- record last choices and their order, for being able to backtrack
  -- several times
  if p_truncate
  then
    truncate final_choice;
    truncate tried;
  end if;

  while true
  loop
    -- is this finished?
    v_next_students := (select count(*) from next_student);
    v_final_count := (select count(*) from final_choice);
    v_tried_count := (select count(*) from tried);

    raise notice '';
    raise notice 'current count: % / % paths tried',
                 v_final_count, v_tried_count;

    if v_next_students = 0
    then
     return;
    end if;

    -- get next student to affect to class, lv1, lv2
    select into v_ns
           *
      from add_next_student(v_curtree);

    if v_ns is null
    then
      -- could not affect this student, remove & blacklist last one
      v_ns_last.id = ltree2text(subpath(v_curtree, -1))::int;
      raise notice 'removing % from final_choice', v_ns_last.id;

      delete from final_choice where id = v_ns_last.id;
      v_curtree := subltree(v_curtree, 0, nlevel(v_curtree) - 1);
    else
      -- we just choosed a student
      raise notice 'next student: %', v_ns;
      v_curtree := v_curtree || v_ns.id::text;
      insert into tried values(v_curtree);
    end if;
  end loop;
  return;
end;
$f$;

create or replace function backtrack()
  returns void
  language sql
as $f$
   select backtrack(false);
$f$;


create or replace view final_choice_out as
 SELECT s.lastname, s.firstname, fc."class", fc.lv1, fc.lv2,
        COALESCE(fc."option", s."option") AS option
   FROM student s
   LEFT JOIN final_choice fc USING (id)
  ORDER BY fc."class", s.lastname;


create or replace view check_count as
  select class, count(*) from final_choice group by class 
union
  select lv1, count(*) from final_choice group by lv1
union
  select lv2, count(*) from final_choice where lv2 ~ '^[DE]' group by lv2
order by 1;

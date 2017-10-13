begin;

drop table if exists magic.sets, magic.cards;
drop table if exists settype, set, booster, artist, layout, cardtype, subtype, type, card;
drop type if exists color_t, rarity_t, border_t;

create table magic.sets
    as
select key as name, value - 'cards' as data
  from magic.allsets, jsonb_each(data);
  
create table magic.cards
    as
  with collection as
  (
     select key as set,
            value->'cards' as data
       from magic.allsets,
            lateral jsonb_each(data)
  )
  select set, jsonb_array_elements(data) as data
    from collection;

select jsonb_pretty(data)
  from magic.cards
 where data @> '{"name" : "Sen Triplets"}';

select count(*)
  from magic.cards
 where data @> '{"manaCost": "{2}{W}{U}{B}"}';

  select data->'colors' as colors,
         count(*)
    from magic.cards
   where data @> '{"colors": ["White","Blue","Black"]}'
group by grouping sets(data->'colors', ())
order by count desc;

with booster as
(
    select name,
           initcap(
             jsonb_array_elements_text(sets.data->'booster')
           ) as rarity,
           count(*)
      from magic.sets
     where name = 'LEA'
  group by name, rarity
)
   select rarity, booster.count, count(*)
     from booster
          left join magic.cards
            on cards.set = booster.name
           and cards.data->>'rarity' = booster.rarity
  group by rarity, count
  order by rarity <> 'Uncommon', rarity <> 'Common';

\set collection 'LEA'

with booster as
(
    select name,
           initcap(
             jsonb_array_elements_text(sets.data->'booster')
           ) as rarity,
           count(*)
      from magic.sets
     where name = :'collection'
  group by name, rarity
)
   select cards.data->>'rarity' as rarity,
          cards.data->>'name' as name,
          cards.data->>'manaCost' as manacost,
          cards.data->>'type' as type
     from booster
          left join lateral
          (
            select data
              from magic.cards
             where cards.set = booster.name
               and cards.data->>'rarity' = booster.rarity
          order by random()
             limit booster.count
          )
          as cards
          on true
  order by cards.data->>'rarity' <> 'Uncommon',
           cards.data->>'rarity' <> 'Common';

  select data->>'type' as type, count(*)
    from magic.cards
group by type
order by count desc
   limit 3;

  select jsonb_array_elements_text(data->'subtypes') as subtypes, count(*)
    from magic.cards
group by subtypes
order by count desc
   limit 3;

  select jsonb_array_elements_text(data->'types') as types, count(*)
    from magic.cards
group by types
order by 2 desc
   limit 3;

  select data->>'id' as id, count(*)
    from magic.cards
group by id
  having count(*)>1;

select ctid,
       data->>'name' as name,
       data->>'mciNumber' as mci_number,
       data->>'multiverseid' as multiverseid,
       md5(data::text)
  from magic.cards
 where data @> '{"id":"2ba1628b4169e33a4a6773124bec72fadfb6c983"}'
    or data @> '{"id":"34bbf9e14b771f9eb4c4c2970ed54b8ff836118a"}';

delete
  from magic.cards
 where ctid =
       (
         select ctid
           from magic.cards
          where data->>'id' = '2ba1628b4169e33a4a6773124bec72fadfb6c983'
          limit 1
       );

delete
  from magic.cards
 where ctid =
       (
         select ctid
           from magic.cards
          where data->>'id' = '34bbf9e14b771f9eb4c4c2970ed54b8ff836118a'
          limit 1
       );

  select data->>'rarity' as rarity, count(*)
    from magic.cards
group by rarity
order by count desc;

with booster(rarity_js) as (
  select case jsonb_typeof(booster)
              when 'array'
              then booster
              else array_to_json(array[booster])::jsonb
          end
    from magic.sets,
         jsonb_array_elements(data->'booster') as booster
)
  select rarity, count(*)
    from booster,
         jsonb_array_elements_text(rarity_js) as t(rarity)
group by rarity
order by count desc;

create type rarity_t as enum
 (
  'Common', 'Uncommon', 'Rare', 'Land', 'Mythic Rare',
  'Marketing', 'Timeshifted Common', 'Checklist',
  'Foil Mythic Rare', 'Foil Rare', 'Foil Common', 'Foil Uncommon',
  'Timeshifted Uncommon', 'Double Faced Uncommon',
  'Draft-Matters', 'Double Faced Rare',
  'Timeshifted Rare', 'Timeshifted Purple',
  'Double Faced Mythic Rare', 'Double Faced Common',
  'Foil', 'Urza Land', 'Power Nine', 'Double Faced',
  'Basic Land', 'Special'
 );

create type border_t as enum
 (
  'black', 'white', 'silver'
 );

create table settype
 (
   settypeid  serial primary key,
   name       text,
   
   unique(name)
 );

create table set
 (
   setid     serial primary key,
   mkmid     integer,
   name      text,
   code      text,
   info_code text,
   release   date,
   border    border_t,
   settypeid integer not null references settype(settypeid)
 );

create table booster
 (
   setid      integer not null references set(setid),
   num        integer,
   rarity     rarity_t[],
   
   primary key(setid, num)
 );

create type color_t as enum
 (
  'Black', 'Blue', 'Green', 'Red', 'White'
 );

create table artist
 (
   artistid serial primary key,
   name     text,
   
   unique(name)
 );

create table layout
 (
  layoutid  serial primary key,
  layout    text,
  
  unique(layout)
 );

create table cardtype
 (
  cardtypeid   serial primary key,
  cardtype     text,
  
  unique(cardtype)
 );

create table subtype
 (
  subtypeid serial primary key,
  subtype   text,

  unique(subtype)
);

create table type
 (
  typeid serial primary key,
  type   text,

  unique(type)
 );

create table card
 (
  id           text primary key,
  setid        integer not null references set(setid),
  name         text,
  flavor       text,
  description  text,
  image_name   text,
  layoutid     integer not null references layout(layoutid),
  artistid     integer not null references artist(artistid),
  cardtypeid   integer not null references cardtype(cardtypeid),
  types        integer[],
  subtypes     integer[],
  colors       color_t[],
  color_id     color_t[],
  rarity       rarity_t,
  mci          text,
  multiverseid integer,
  number       text,
  cmc          numeric,
  power        text,
  toughness    text,
  manacost     jsonb,
  extra        jsonb
 ); 

insert into settype(name)
     select distinct(data->>'type')
       from magic.sets;


insert into set(mkmid, name, code, info_code,
                release, border, settypeid)
     select (data->>'mkm_id')::int,
            data->>'name',
            data->>'code',
            data->>'magicCardsInfoCode',
            (data->>'releaseDate')::date,
            (data->>'border')::border_t,
            settypeid
       from magic.sets
            join settype
              on sets.data->>'type' = settype.name;


select setid, mkmid, set.name, code, release, settype.name
  from set
       join settype using(settypeid)
 limit 10;


insert into booster(setid, num, rarity)
     select setid,
            row_number() over(partition by setid) as num,
            case jsonb_typeof(booster)
                 when 'array'
                 then (
                       select array_agg(initcap(rarity)::rarity_t)
                         from jsonb_array_elements_text(booster) as rarity
                      )
                 else array[initcap(booster #>> '{}')::rarity_t]
             end
            as rarity
       from magic.sets
            join set on set.code = sets.name,
            lateral jsonb_array_elements(data->'booster') booster
   order by setid;


select mkmid, release, set.name,
       booster.num, booster.rarity
  from booster
       join set using(setid)
 where set.code = 'LEA';


with stats as
(
      select settype.name as type,
             count(booster.num) as cards
        from set
             join settype using(settypeid)
             left join booster using(setid)
    group by setid, settype.name
      having count(booster.num) > 0
)
  select type, cards, count(*)
    from stats
group by type, cards
order by type, cards desc;


insert into artist(name)
     select distinct(data->>'artist')
       from magic.cards;


insert into layout(layout)
     select distinct(data->>'layout')
       from magic.cards;


insert into cardtype(cardtype)
     select distinct(data->>'type')
       from magic.cards;

insert into subtype(subtype)
     select distinct(jsonb_array_elements_text(data->'subtypes'))
       from magic.cards;

insert into type(type)
     select distinct(jsonb_array_elements_text(data->'types'))
       from magic.cards;


insert into card(id, setid, name, flavor, description, image_name,
                 layoutid, artistid, cardtypeid,
                 types, subtypes, colors, color_id,
                 rarity, mci, multiverseid, number,
                 cmc, power, toughness, manacost, extra)
     select data->>'id',
            set.setid,
            data->>'name',
            data->>'flavour',
            data->>'text',
            data->>'imageName',
            layoutid,
            artistid,
            cardtypeid,
            (
             select array_agg(typeid order by typeid)
               from type
              where data->'types' @> format('["%s"]', type)::jsonb
            ),
            (
             select array_agg(subtypeid order by subtypeid)
               from subtype
              where data->'subtypes' @> format('["%s"]', subtype)::jsonb
            ),
            (
             select array_agg(color::color_t order by color::color_t)
               from jsonb_array_elements_text(data->'colors') as color
            ),
            (
             select array_agg(case when color = 'B' then 'Black'::color_t
                                   when color = 'U' then 'Blue'::color_t
                                   when color = 'G' then 'Green'::color_t
                                   when color = 'R' then 'Red'::color_t
                                   when color = 'W' then 'White'::color_t
                               end
                             )
               from jsonb_array_elements_text(data->'colorIdentity') as color
            ),
            (data->>'rarity')::rarity_t,
            data->>'mciNumber',
            (data->>'multiverseid')::integer,
            data->>'number',
            (data->>'cmc')::numeric,
            data->>'power',
            data->>'toughness',
            data->'manaCost',

            data - 'id' - 'name' - 'flavor' - 'text' - 'imageName'
                 - 'layout' - 'artist' - 'type' - 'rarity'
                 - 'types' - 'subtypes' - 'colors' - 'colorIdentity'
                 - 'mciNumber' - 'multiverseid' - 'number'
                 - 'cmc' - 'power' - 'toughness' - 'manaCost'
            
       from magic.cards
            join set on set.code = cards.set
            join layout on layout.layout = cards.data->>'layout'
            join artist on artist.name = cards.data->>'artist'
            join cardtype on cardtype = cards.data->>'type';


  select type, count(*),
         repeat('■', (   100.0 
                       * count(*)
                       / sum(count(*)) over ()
                     )::int
               ) as pct
    from type
         left join card
                on card.types @> array[typeid]
group by type
order by type;


select data->>'name' as name, data->>'types' as types
  from magic.cards
 where data->'types' @> '["You''ll"]';


  select card.name, count(distinct card.id)
    from card
         join type
           on type.typeid = any (card.types)
          and type.type = any('{"Scariest", "You''ll", "Ever", "See"}'::text[])
group by card.id;


\set collection 'LEA'

with booster as
(
    select name,
           initcap(
             jsonb_array_elements_text(sets.data->'booster')
           ) as rarity,
           count(*)
      from magic.sets
     where name = :'collection'
  group by name, rarity
)
   select cards.data->>'rarity' as rarity,
          cards.data->>'name' as name,
          cards.data->>'manaCost' as manacost,
          cards.data->>'type' as type
     from booster
          left join lateral
          (
            select data
              from magic.cards
             where cards.set = booster.name
               and cards.data->>'rarity' = booster.rarity
          order by random()
             limit booster.count
          )
          as cards
          on true
  order by cards.data->>'rarity' <> 'Uncommon',
           cards.data->>'rarity' <> 'Common';


select booster.num,
       booster.rarity,
       card.name,
       card.rarity,
       card.cardtype
  from booster
       join set using(setid)
       left join lateral
       (
         select card.name, card.rarity, cardtype.cardtype
           from card
                join cardtype using(cardtypeid)
          where setid = booster.setid
            and card.rarity = any(booster.rarity)
       order by random()
          limit 1
       )
       as card
       on true
 where set.code = :'collection';


commit;

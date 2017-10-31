+++
title = "on Json and SQL"
date = "2017-09-18T10:49:03+02:00"
tags = ["PostgreSQL","YeSQL","json","Normalization"]
categories = ["PostgreSQL","YeSQL"]
coverImage = "/img/rube-goldberg-machine.jpg"
coverMeta = "in"
coverSize = "partial"
thumbnailImage = "/img/JSON.png"
thumbnailImagePosition = "left"

+++

PostgreSQL has had proper
[json](https://www.postgresql.org/docs/current/static/datatype-json.html)
support for a while now. The unique extensibility approach of the PostgreSQL
system allows it to enable native [SQL friendly JSON
processing](https://www.postgresql.org/docs/current/static/functions-json.html).

In this article we'll play with the [Magic: the Gathering card data in JSON
format](https://mtgjson.com) data set, provided with a
[CC0](https://creativecommons.org/choose/zero/) licence, and process the
information provided. We also see how to normalize a JSON document into a
proper database model that benefits from some PostgreSQL advanced features,
and how to then inject the JSON documents into the normalized database
schema. Finally, we compare some non-trivial processing done against both
versions of the database schema.

<!--more-->
<!--toc-->

# Loading the data set

First retrieve [AllSets.json](https://mtgjson.com/json/AllSets.json.zip) and
then load it into your local testing PostgreSQL instance as following:

~~~ sql
create schema if not exists magic;
create table magic.allsets(data jsonb);
~~~

For loading the 27MB single json document in PostgreSQL, we can't directly
use COPY this time, so we'll write some Python code:

~~~ python
#! /usr/bin/env python3

import psycopg2

PGCONNSTRING = "dbname=magic"

if __name__ == '__main__':
    pgconn = psycopg2.connect(PGCONNSTRING)
    curs = pgconn.cursor()

    allset = open('AllSets.json').read()
    allset = allset.replace("'", "''")
    sql = "insert into magic.allsets(data) values(%s)"

    curs.execute(sql, (allset,))
    pgconn.commit()
    pgconn.close()
~~~

Let's be fair though, a single json document isn't exactly a good example to
show the PostgreSQL capabilities when it comes to handling this data type.
Also, I doubt your application would do that, even when using MongoDB. I
guess a collection of cards would be better, so let's transform our giant
JSON document into such a collection:

~~~ sql
create table magic.cards
          as
      select jsonb_array_elements(value->'cards') as data
        from magic.allsets, jsonb_each(data);
      
create index on magic.cards using gin(data jsonb_path_ops);
~~~

On the old laptop I'm using for typing this blog post and playing with
PostgreSQL, extracting the Magic™ cards from the single document into 34207
entries in the *magic.cards* table took 603.524 ms, and creating a dedicated
*gin* index took 3121.274 ms. Of course those numbers aren't repeatable and
mean nothing…

# Finding cards

The home page of the <https://mtgjson.com> website features the card named
*Sen Triplets*, how can we find it in our collection? The following query is
one way to do implement the search:

~~~ sql
select jsonb_pretty(data)
  from magic.cards
 where data @> '{"name" : "Sen Triplets"}';
~~~

You might have noticed that we created a *gin* index using the
[jsonb_path_ops](https://www.postgresql.org/docs/9.4/static/datatype-json.html#JSON-INDEXING)
operator class, which supports only the `@>` operator (it reads *contains*).
This index is meant to speed up queries where we search a *json* object
inside our json documents. The query plan shows the index is actually being
used:

~~~
                                  QUERY PLAN                                   
═══════════════════════════════════════════════════════════════════════════════
 Bitmap Heap Scan on cards  (cost=16.27..141.81 rows=34 width=32)
   Recheck Cond: (data @> '{"name": "Sen Triplets"}'::jsonb)
   ->  Bitmap Index Scan on cards_data_idx  (cost=0.00..16.26 rows=34 width=0)
         Index Cond: (data @> '{"name": "Sen Triplets"}'::jsonb)
(4 rows)
~~~

Now, for the result, it would be better if I were to show it as a *JSON*
document here, rather than as a *psql* result set of one line. Save the
query to a *sen-triplets.sql* file, then run the following command:

~~~ bash
psql --no-psqlrc -At -f sen-triplets.sql -d <connection string>
~~~

And here's our JSON object:

~~~ json
{
    "id": "3129aee7f26a4282ce131db7d417b1bc3338c4d4",
    "cmc": 5,
    "name": "Sen Triplets",
    "text": "At the beginning of your upkeep, choose target opponent. This turn, that player can't cast spells or activate abilities and plays with his or her hand revealed. You may play cards from that player's hand this turn.",
    "type": "Legendary Artifact Creature — Human Wizard",
    "power": "3",
    "types": [
        "Artifact",
        "Creature"
    ],
    "artist": "Greg Staples",
    "colors": [
        "White",
        "Blue",
        "Black"
    ],
    "flavor": "They are the masters of your mind.",
    "layout": "normal",
    "number": "109",
    "rarity": "Mythic Rare",
    "manaCost": "{2}{W}{U}{B}",
    "subtypes": [
        "Human",
        "Wizard"
    ],
    "imageName": "sen triplets",
    "mciNumber": "109",
    "toughness": "3",
    "supertypes": [
        "Legendary"
    ],
    "multiverseid": 180607,
    "colorIdentity": [
        "W",
        "U",
        "B"
    ]
}
~~~

Now this *Mana Cost* expression does seem quite complex, and I guess it's
pretty rare to find it. Let's figure that out:

~~~ sql
select count(*)
  from magic.cards
 where data @> '{"manaCost": "{2}{W}{U}{B}"}';
~~~

Only 3 cards in our complete collection do match such a *Mana Cost*.

In order to show other ways to query our collection and benefit from the
index, we could count how many cards are found with the 3 colors `["White",
"Blue", "Black"]` as this one. The *contains* operator (spelled `@>`) when
given a json array searches for arrays that *intersect* with the search
query. It means that it finds arrays with more elements than those in the
search query, as seen in the following example:

~~~ sql
  select data->'colors' as colors,
         count(*)
    from magic.cards
   where data @> '{"colors": ["White","Blue","Black"]}'
group by grouping sets(data->'colors', ())
order by count desc;
~~~

Note we used a *grouping sets* expression that returns both the total count
and the count per specific array of colors:

~~~
                   colors                   │ count 
════════════════════════════════════════════╪═══════
 ¤                                          │    98
 ["White", "Blue", "Black"]                 │    57
 ["White", "Blue", "Black", "Red", "Green"] │    37
 ["White", "Blue", "Black", "Green"]        │     2
 ["White", "Blue", "Black", "Red"]          │     2
(5 rows)
~~~


# Cards sets

When creating our *magic.cards* table, we didn't keep the sets information,
and for more interesting queries we're going to need that.

~~~ sql
begin;

create table magic.sets
    as
select key as name, value - 'cards' as data
  from magic.allsets, jsonb_each(data);
  
drop table magic.cards;

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

commit;
~~~

The expression *value - 'cards'* allow to remove the *cards* key from the
JSON object. In our case this allows to register only the collection parts
of the object, and leave away the cards for the other table.

# Build a Magic™ booster deck

With this data we can see about building a booster from any edition of the
cards collection. First, let's have a look at the *Limited Edition Alpha*
edtition:

~~~ sql
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
~~~

This query shows several interesting techniques:

  - we have to normalize the data embedded into the json data field so that
    we can use them in our join operation: the sets contain a booster field
    with is an array of card rarity to form the booster, but all lower case
    when the *"rarity"* key in the cards are capitalized, that's why we use
    *initcap* in the query,
    
  - the *left join* operation is done against data extracted on the fly from
    the cards, here we have no indexing support for that but the data set
    being very small (34207 different cards) that's no problem,
    
  - finally we abuse the knowledge that *false* sorts arbitrarily before
    *true* in PostgreSQL to build a custom *order by* rule, we want to see
    *Uncommon* then *Common* and then only *Rare*.

And here's the result set:

~~~
  rarity  │ count │ count 
══════════╪═══════╪═══════
 Uncommon │     3 │    95
 Common   │    11 │    74
 Rare     │     1 │   116
(3 rows)
~~~

So, to build a magic deck from the *LEA* set we need to pick 3 uncommon
cards among 95 available, then 11 common cards among the 74 available and
finally 1 rare in the 116 rare cards from the collection.

~~~ sql
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
~~~

The *left join lateral* allows to inject directly how many rows we need in
the subquery limit clause. We use the *order by random()* trick here to get
our cards. It's ok because we know that a booster doesn't contain that many
items, and as said before, even this laptop where the article is typed has
no problem dealing with 34207 rows. Should you need to pick a *random()*
number of items in a bigger list of choice, read [Selecting random rows from
a
table](http://blog.rhodiumtoad.org.uk/2009/03/08/selecting-random-rows-from-a-table/)
by [The Rhodium Toad](http://blog.rhodiumtoad.org.uk), PostgreSQL committer.

And here's such a booster pack:

~~~
  rarity  │             name             │ manaCost │           type           
══════════╪══════════════════════════════╪══════════╪══════════════════════════
 Uncommon │ White Ward                   │ {W}      │ Enchantment — Aura
 Uncommon │ Berserk                      │ {G}      │ Instant
 Uncommon │ Bog Wraith                   │ {3}{B}   │ Creature — Wraith
 Common   │ Ironclaw Orcs                │ {1}{R}   │ Creature — Orc
 Common   │ False Orders                 │ {R}      │ Instant
 Common   │ Twiddle                      │ {U}      │ Instant
 Common   │ Red Elemental Blast          │ {R}      │ Instant
 Common   │ Samite Healer                │ {1}{W}   │ Creature — Human Cleric
 Common   │ Shanodin Dryads              │ {G}      │ Creature — Nymph Dryad
 Common   │ Weakness                     │ {B}      │ Enchantment — Aura
 Common   │ Wall of Wood                 │ {G}      │ Creature — Wall
 Common   │ Llanowar Elves               │ {G}      │ Creature — Elf Druid
 Common   │ Benalish Hero                │ {W}      │ Creature — Human Soldier
 Common   │ Merfolk of the Pearl Trident │ {U}      │ Creature — Merfolk
 Rare     │ Contract from Below          │ {B}      │ Sorcery
(15 rows)
~~~

When consuming the data in JSON in your application, you might want to
return directly *cards.data*, which is a json document, rather than
extracting some of the fields only like we've done here.

# Magic™ card type, type of card and subtype

From the [MTGjson documentation](https://mtgjson.com/documentation.html) we
have different kind of types for a single card:

  - *type*
  
    The card type. This is the type you would see on the card if printed
    today.
    
  - *types*
  
    The types of the card. These appear to the left of the dash in a card
    type. Example values: Instant, Sorcery, Artifact, Creature, Enchantment,
    Land, Planeswalker
    
  - *supertypes*
  
    The supertypes of the card. These appear to the far left of the card
    type. Example values: Basic, Legendary, Snow, World, Ongoing.
    
  - *subtypes*
  
    The subtypes of the card. These appear to the right of the dash in a
    card type. Usually each word is its own subtype. Example values: Trap,
    Arcane, Equipment, Aura, Human, Rat, Squirrel, etc.

Of course those are repeated a lot in every card instance, as we can see
with the following queries:

~~~ sql
  select data->>'type' as type, count(*)
    from magic.cards
group by type
order by count desc
   limit 3;
~~~

~~~
    type     │ count 
═════════════╪═══════
 Instant     │  4052
 Sorcery     │  3805
 Enchantment │  2020
(3 rows)
~~~

~~~ sql
  select jsonb_array_elements_text(data->'subtypes') as subtypes, count(*)
    from magic.cards
group by subtypes
order by count desc
   limit 3;
~~~

~~~
 subtypes │ count 
══════════╪═══════
 Human    │  3192
 Aura     │  1669
 Wizard   │  1013
(3 rows)
~~~

~~~ sql
  select jsonb_array_elements_text(data->'types') as types, count(*)
    from magic.cards
group by types
order by 2 desc
   limit 3;
~~~

~~~
    types    │ count 
═════════════╪═══════
 Creature    │ 15827
 Instant     │  4201
 Enchantment │  3875
(3 rows)
~~~

We had a repetition problem with the *rarity* already, now with the three
different kinds of types a card might have, and then there's also the
*artist*. It's going to be quite hard to maintain that data set, so… I can't
resist. 

First, to check some basic guarantees, second, to normalize the data.

# Constraints and data quality

The data being available in JSON and easy enough to process could be all we
need. Sometimes your application or business rules require data quality, and
Relational Data Base Management Systems provide that with the help of
constraints. As a user, it means quite some work has to be done to declare
the kind of data quality we want.

The [MTGjson documentation](https://mtgjson.com/documentation.html) tells us
that the cards *id* field is

> A unique id for this card. It is made up by doing an SHA1 hash of
> setCode + cardName + cardImageName

There's no constraint to guarantee that the *id* is actually unique, in the
JSON file. Let's have a look. Here's a classic query to find duplicates, and
we expect no result:

~~~ sql
  select data->>'id' as id, count(*)
    from magic.cards
group by id
  having count(*)>1;
~~~

Oh. Oopsie:

~~~
                    id                    │ count 
══════════════════════════════════════════╪═══════
 2ba1628b4169e33a4a6773124bec72fadfb6c983 │     2
 34bbf9e14b771f9eb4c4c2970ed54b8ff836118a │     2
(2 rows)
~~~

Details? Sure:

~~~
select ctid,
       data->>'name' as name,
       data->>'mciNumber' as mci_number,
       data->>'multiverseid' as multiverseid,
       md5(data::text)
  from magic.cards
 where data @> '{"id":"2ba1628b4169e33a4a6773124bec72fadfb6c983"}'
    or data @> '{"id":"34bbf9e14b771f9eb4c4c2970ed54b8ff836118a"}';
~~~

So, real duplicates it seems:

~~~
  ctid   │         name          │ mci_number │ multiverseid │               md5                
═════════╪═══════════════════════╪════════════╪══════════════╪══════════════════════════════════
 (493,7) │ Jaraku the Interloper │ 31b        │ 74489        │ 81afc561a6055eac8d2b59be00b63946
 (493,8) │ Jaraku the Interloper │ 31b        │ 74489        │ 81afc561a6055eac8d2b59be00b63946
 (497,6) │ Scarmaker             │ 69b        │ 74476        │ 975c9860656b05e81d7e46d6af17c507
 (497,7) │ Scarmaker             │ 69b        │ 74476        │ 975c9860656b05e81d7e46d6af17c507
(4 rows)
~~~

Now, to clean up the mess, we need a way to distinguish two rows that are
entirely the same. No user column differ. Mmm.

# Deleting duplicate rows

The *ctid* is one of the [PostgreSQL System
Columns](https://www.postgresql.org/docs/current/static/ddl-system-columns.html).
It is accessible to us users, and to use with care. From the docs:

> The physical location of the row version within its table. Note that
> although the ctid can be used to locate the row version very quickly, a
> row's ctid will change if it is updated or moved by VACUUM FULL. Therefore
> ctid is useless as a long-term row identifier. The OID, or even better a
> user-defined serial number, should be used to identify logical rows.

So we can actually clean up this mess:

~~~ sql
begin;

delete
  from magic.cards
 where data->>'id' = '2ba1628b4169e33a4a6773124bec72fadfb6c983'
   and ctid = '(493,8)';

delete
  from magic.cards
 where data->>'id' = '34bbf9e14b771f9eb4c4c2970ed54b8ff836118a'
   and ctid = '(497,7)';

commit;
~~~

You can see in the *delete* statements above that we use ***both*** the
system column *ctid* and a user column, here *data->>'id'*. This allows to
*delete* nothing should concurrent activity move the data around on disk.
You never know.

{{< alert info >}}

If you like this kind of *dive into SQL* content, where every SQL query is
presented in a context, and with a real data set that you can download and
play with at home, then you will love my new book: [Mastering PostgreSQL in
Application Development](http://masteringpostgresql.com).

{{< /alert >}}

<script async id="_ck_279686" src="https://forms.convertkit.com/279686?v=6"></script>

# Cards and Sets Rarities

As we've seen in a previous section of this document, cards and set's
boosters rarities are oddly different. Let's have a closer look now. For the
cards, it's quite simple as each card has a simple *rarity* key:

~~~ sql
  select data->>'rarity' as rarity, count(*)
    from magic.cards
group by rarity
order by count desc;
~~~

It's interesting to count the cards by *rarity* classification, because
that's how it supposed to be classified. We expect *Rare* cards have a
lesser count than *Common* ones, and that's what we have:

~~~
   rarity    │ count 
═════════════╪═══════
 Common      │ 11297
 Uncommon    │  9618
 Rare        │  8711
 Basic Land  │  1854
 Special     │  1704
 Mythic Rare │  1021
(6 rows)
~~~

Now, the sets objects have a *booster* key, which has its own full blown
documentation paragraph at the [MTGjson
documentation](https://mtgjson.com/documentation.html) page. Quoting from it:

> The 'booster' key is present for each set that has physical boosters (so
> not present for box sets, duel decks, digital masters editions, etc.). It
> is an array containing one item per card in the booster. Thus the array
> length is how many cards are in a booster. Each item in the array is
> either a string representing the type of booster card or an array of
> strings representing possible types for that booster card.

Here's how to extract all the different possible values from the *boosters*,
and it's more complex than we would like it to:

~~~ sql
  select case jsonb_typeof(booster)
              when 'array'
              then initcap(jsonb_array_elements_text(booster))
              else initcap(booster #>> '{}')
          end
         as rarity,
         count(*)
    from magic.sets,
         jsonb_array_elements(data->'booster') booster
group by rarity
order by count desc;
~~~

{{< alert warning >}}

This query won't run in PostgreSQL 10, to get the fixed version and know
why, and understand how to fix it, please read [Set Returning Fonctions and
PostgreSQL 10](/blog/2017/10/set-returning-fonctions-and-postgresql-10/).

{{< /alert >}}

As the *booster* key is associated with an array value, we *unnest* the
array with *jsonb_array_elements* again. As each element of the array might
be either another array or a text, we can't use *jsonb_array_elements_text*
this time. We have to *unnest* nested array elements or to extrat the text
from a single json text value.

To extract the single json text value as a PostgreSQL text, without the
extra json double-quoting, we use the `#>>` operator, that reads *extract
text at path*. And the json path for a single text element is `{}`.

On the result set, we have the following list of entries this time:

~~~
          rarity          │ count 
══════════════════════════╪═══════
 Common                   │  1138
 Uncommon                 │   331
 Rare                     │   110
 Land                     │    50
 Mythic Rare              │    44
 Marketing                │    43
 Timeshifted Common       │    14
 Checklist                │     5
 Foil Mythic Rare         │     4
 Foil Rare                │     4
 Foil Common              │     4
 Foil Uncommon            │     4
 Timeshifted Uncommon     │     4
 Double Faced Uncommon    │     2
 Draft-Matters            │     2
 Double Faced Rare        │     2
 Timeshifted Rare         │     2
 Timeshifted Purple       │     2
 Double Faced Mythic Rare │     2
 Double Faced Common      │     2
 Foil                     │     1
 Urza Land                │     1
 Power Nine               │     1
 Double Faced             │     1
(24 rows)
~~~

The first thing we notice is that our cards collection only has 6 different
rarities and the boosters contain 24 different ones. It's going to be
complex to build some of those boosters…

Other than that, we want to generalize the rules to build a booster. Those
alternative choices — represented as an array in the JSON format — might be
a good representation for every entry in the *booster* so as to unify the
processing of the field. Matching an element against and array is easy to
implement in SQL thanks to both the [Row and Array
Comparisons](https://www.postgresql.org/docs/current/static/functions-comparisons.html)
and the *array contains item* operator, spelled `@>`.

PostgreSQL has support for arrays, and support for enumerated types. It
should be no surprise that PostgreSQL covers arrays of enums too. So our
choice for representing a booster pack is going to be able to use an array
of *rarity_t* elements:

~~~ sql
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
~~~

With that data type, we now are able to build arrays of *rarity_t* elements,
and check if a card's *rarity* value is contained into such as array.

# Normalizing the data set

PostgreSQL is very good at mixing SQL and JSON, as we've seen in the
previous query, thanks to advanced processing functions such as
`jsonb_array_elements_text` and operators such as `->`, `->>` and `@>`.

That said, minor inconsistencies such as the *rarity* information being
spelled in different ways in different contexts is a problem. Duplicate
entries is an ever greater concern.

There's no way for PostgreSQL to guarantee any data quality when all it
knows about are opaque *json* documents. We won't host a lecture of the
[Normal Forms](https://en.wikipedia.org/wiki/Database_normalization)
benefits here, but we can still see how to turn our JSON documents into a
proper relational model.

## Sets

PostgreSQL makes the translation from the JSON document to a proper schema
quite easy, as we are going to see now. First, the sets.

~~~ sql
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
~~~

We made some trade-offs here, and didn't follow the *Normal Forms* rules to
the letter. The *booster* is now a proper reference table with as many
entries as cards, each having a “number”, as in card number 1 is a “Mythic
Rare” and card number 2 is an “Uncommon”. Each entry of the *booster* table
associates a *set* with a *rarity* array containing alternative choices for
this card of the booster. When there's no alternative, we will have there an
array of a single value.

We also chose to create an enumerated data type for the *border* rather than
a reference table. Well since its creation about 25 years ago, only 3 border
types have been produced. I think using an *enum* here is a good trade-off.

## Cards

Now about the cards themselves:

~~~ sql
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
~~~

This isn't a fully normalized schema. Rather than going to full distance and
having association tables between a *color* reference table and the *card*
table, we chose to declare the color as an enumerate data type. That's
because we know that the Magic™ game is defined by those 5 colors. No data
maintenance with new colors is to be expected.

New cards layouts and types have been known to appear in some special
editions, packs, or collections. So we need to have an easy way to manage
them, each in their own reference table.

Still, rather than having a proper association table for this many to many
relationship, we use a PostgreSQL array: once a card is edited, its
properties have zero maintenance needs. Also, PostgreSQL makes it easy to
index and search into arrays. 

The only problem with those arrays is that we can't implement a *foreign
key* to the reference tables type and subtype.

# Dispatching JSON data into normalized tables

Now that we have a table schema that should make our work easier, how do we
populate it? Well, in SQL of course!

## Sets

It begins with the sets and their type:

~~~ sql
insert into settype(name)
     select distinct(data->>'type')
       from magic.sets;
~~~

And we have our 17 types of sets registered. On to the sets themselves:

~~~ sql
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
~~~

As you can see the PostgreSQL json operator `->>` extract the data as *text*
and we need to convert those text entries to their target data types at
times. It's easy to do, and ensures we know what we're dealing with.

And now we have a clean data set for the sets:

~~~ sql
select setid, mkmid, set.name, code, release, settype.name
  from set
       join settype using(settypeid)
 limit 10;
~~~

~~~
 setid │ mkmid │         name          │ code │  release   │   name    
═══════╪═══════╪═══════════════════════╪══════╪════════════╪═══════════
     1 │    74 │ Tenth Edition         │ 10E  │ 2007-07-13 │ core
     2 │     3 │ Unlimited Edition     │ 2ED  │ 1993-12-01 │ core
     3 │     6 │ Revised Edition       │ 3ED  │ 1994-04-01 │ core
     4 │    10 │ Fourth Edition        │ 4ED  │ 1995-04-01 │ core
     5 │    47 │ Fifth Dawn            │ 5DN  │ 2004-06-04 │ expansion
     6 │    23 │ Fifth Edition         │ 5ED  │ 1997-03-24 │ core
     7 │    29 │ Classic Sixth Edition │ 6ED  │ 1999-04-21 │ core
     8 │    37 │ Seventh Edition       │ 7ED  │ 2001-04-11 │ core
     9 │    44 │ Eighth Edition        │ 8ED  │ 2003-07-28 │ core
    10 │    49 │ Ninth Edition         │ 9ED  │ 2005-07-29 │ core
(10 rows)
~~~

## Boosters

Our *set* table is missing the *booster* information, which is now handled
separately. It is now time for us to handle it.

~~~ sql
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
~~~

In this query, we process the booster array elements in the same way as
before, depending on if they are an array again (and then fetch each array
element as text, capitalize this text using the *initcap* function and
casting the result as a *rarity_t* enum value) or a scalar text value, in
which case we build an array containing a single element, the extracted text
value as a *rarity_t* enum value.

We use a *lateral join* in the query in order to *unnest* the
*data->'booster'* entry for each set found. In the output of the query, we
also use the window function *row_number()* over peers as defined by the
expression *partition by setid* so as to associate to each element of the
array its index position, and keep that in our *booster* target table.

Keeping the array index position should allow us to display booster
configurations and randomly generated booster instances in the right order.

We may have a look at one particular booster definition, the first one ever:

~~~ sql
select mkmid, release, set.name,
       booster.num, booster.rarity
  from booster
       join set using(setid)
 where set.code = 'LEA';
~~~

And here's what the booster for the *Limiter Edition Alpha* looks like in
our setup:

~~~
 mkmid │  release   │         name          │ num │   rarity   
═══════╪════════════╪═══════════════════════╪═════╪════════════
     1 │ 1993-08-05 │ Limited Edition Alpha │   1 │ {Rare}
     1 │ 1993-08-05 │ Limited Edition Alpha │   2 │ {Uncommon}
     1 │ 1993-08-05 │ Limited Edition Alpha │   3 │ {Uncommon}
     1 │ 1993-08-05 │ Limited Edition Alpha │   4 │ {Uncommon}
     1 │ 1993-08-05 │ Limited Edition Alpha │   5 │ {Common}
     1 │ 1993-08-05 │ Limited Edition Alpha │   6 │ {Common}
     1 │ 1993-08-05 │ Limited Edition Alpha │   7 │ {Common}
     1 │ 1993-08-05 │ Limited Edition Alpha │   8 │ {Common}
     1 │ 1993-08-05 │ Limited Edition Alpha │   9 │ {Common}
     1 │ 1993-08-05 │ Limited Edition Alpha │  10 │ {Common}
     1 │ 1993-08-05 │ Limited Edition Alpha │  11 │ {Common}
     1 │ 1993-08-05 │ Limited Edition Alpha │  12 │ {Common}
     1 │ 1993-08-05 │ Limited Edition Alpha │  13 │ {Common}
     1 │ 1993-08-05 │ Limited Edition Alpha │  14 │ {Common}
     1 │ 1993-08-05 │ Limited Edition Alpha │  15 │ {Common}
(15 rows)
~~~

Now that we have the booster information properly organized, we can get more
information about them. Not every set type has a booster attached, and we
have boosters with different number of cards in them. Is there something to
learn about that?

~~~ sql
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
~~~

In this very classic SQL query we see a couple for join operations and the
use of the *having* clause to select only those groups with at least a card
in their attached booster, as in the expression *count(booster.num) > 0*.
That's because the *left join* will still produce an entry when there's no
booster associated, and the entry will be *NULL*.

The query is written in two steps, the first one gets how many cards is
found in each booster associated with every Magic™ set we know about, and
the second step then groups those *settype, count of cards* together and
count their occurences:

~~~
    type    │ cards │ count 
════════════╪═══════╪═══════
 conspiracy │    16 │     2
 core       │    16 │     8
 core       │    15 │    10
 expansion  │    16 │    33
 expansion  │    15 │    37
 expansion  │    12 │     1
 expansion  │     8 │     5
 masters    │    15 │     5
 reprint    │    15 │     5
 reprint    │    12 │     1
 starter    │    15 │     3
 starter    │    10 │     1
 un         │    15 │     1
 un         │    10 │     1
(14 rows)
~~~

Without knowing more about the game itself, it's kind of hard to conclude
anything from this dataset, other than saying that only 7 of the 17
different set types have a booster. How many cards per booster seems to be
quite random from those numbers.

{{< alert warning >}}

Exercise left to the reader, try to find other parameters that influence how
many cards are found in each booster. You might begin with the sets release
dates, for example.

This is a pretty good exercise into [Exploring a Data Set in
SQL](/blog/2017/06/exploring-a-data-set-in-sql/) as we saw here in a recent
article.

{{< /alert >}}

## Cards

On to the artists, with a easy query:

~~~ sql
insert into artist(name)
     select distinct(data->>'artist')
       from magic.cards;
~~~

And we have 663 different artists. Now, the layouts:

~~~ sql
insert into layout(layout)
     select distinct(data->>'layout')
       from magic.cards;
~~~

And we have only 12 different layouts used in our cards collection. Now, the
rarity:

~~~ sql
insert into rarity(rarity)
     select distinct(data->>'rarity')
       from magic.cards;
~~~

And we have only 6 different rarities in all our cards collection. Only 6.
For 34207 cards. That's a lot of repetition in there. Now, the card types:

~~~ sql
insert into cardtype(cardtype)
     select distinct(data->>'type')
       from magic.cards;
~~~

And we have 1541 card types to work with. What about the types and subtypes,
which are JSON arrays this time? It might be more complex… but thanks to
PostgreSQL advanced support for JSON in SQL, not that much:

~~~ sql
insert into subtype(subtype)
     select distinct(jsonb_array_elements_text(data->'subtypes'))
       from magic.cards;
~~~

And we have now 349 subtypes registered. The types are going to have about
the same level of complexity of course:

~~~ sql
insert into type(type)
     select distinct(jsonb_array_elements_text(data->'types'))
       from magic.cards;
~~~

And that's 20 types for our 34207 cards. We're left with the main table now:

~~~ sql
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
~~~

And our 34205 cards are now loaded into a normalized (enough) schema, with
some guarantees.

It might be interesting to note that the cumulated size of the normalized
tables is 17 MB while the *magic.cards* table fits into 23 MB. To be honest,
the *magic.allset* single document entry table with all the cards in there
is only 8632 kB, which is quite impressive.

As another interesting point, you can see that we didn't normalize all the
*JSON* document fields into our *card* table. That's because most of the
*extra* fields are only set for some cards, so I didn't want to spend too
much time on them. It's easy to check how many are set, and we find 8055
rows where `extra <> '{}'::jsonb`, an empty value.

You might have spotted that several *number* fields are left as text, that's
because they may contain non numeric entries. We find cards with the
*mciNumber* “128a”, or with a *power* of “2+*”.

As I really can't resists those nice bar-diagram-in-the-console tricks,
here's the repartition of cards by their types:

~~~ sql
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
~~~

The query shows how to join the *type* and the *card* table when a single
card references several types from a single column, an array of types. We do
that with the PostgreSQL array operator `@>` that reads *contains* and works
against an array on each side. And here's the console bar diagram:

~~~
     type     │ count │                      pct                      
══════════════╪═══════╪═══════════════════════════════════════════════
 Artifact     │  3429 │ ■■■■■■■■■■
 Conspiracy   │    25 │ 
 Creature     │ 15825 │ ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■
 Eaturecray   │     1 │ 
 Enchant      │     7 │ 
 Enchantment  │  3875 │ ■■■■■■■■■■■
 Ever         │     1 │ 
 Instant      │  4201 │ ■■■■■■■■■■■■
 Land         │  3549 │ ■■■■■■■■■■
 Phenomenon   │    16 │ 
 Plane        │   152 │ 
 Planeswalker │   183 │ ■
 Player       │     2 │ 
 Scariest     │     1 │ 
 Scheme       │    65 │ 
 See          │     1 │ 
 Sorcery      │  3868 │ ■■■■■■■■■■■
 Tribal       │    83 │ 
 Vanguard     │   116 │ 
 You'll       │     1 │ 
(20 rows)
~~~

The *“You'll”* type reads quite strange, almost like a bug. So let's try and
see where this comes from, by querying the original data set, before
normalization:

~~~ sql
select data->>'name' as name, data->>'types' as types
  from magic.cards
 where data->'types' @> '["You''ll"]';
~~~

And here's what we get:

~~~
─[ RECORD 1 ]────────────────────────────────────────────
name  │ B.F.M. (Big Furry Monster)
types │ ["Scariest", "Creature", "You'll", "Ever", "See"]
~~~

Well, it might be that people encoding the JSON data set had good reasons to
turn that sentence into card types, or it might be a bug.

~~~ sql
  select card.name, count(distinct card.id)
    from card
         join type
           on type.typeid = any (card.types)
          and type.type = any('{"Scariest", "You''ll", "Ever", "See"}'::text[])
group by card.id;
~~~

In this query we search for all the cards that have one of the words as a
type, with *card.types* being an array of *type.typeid* integers. And of
course we omit the *Creature* word from the sentence, as we know that's a
pretty common card type to find around. And here we go:

~~~
            name            │ count 
════════════════════════════╪═══════
 B.F.M. (Big Furry Monster) │     1
(1 row)
~~~

Looks like a bug in the JSON encoding to me.

# Building a Magic™ booster against a normalized schema

For comparing purposes, let's place the previous query against the JSON
encoded data set first:

~~~ sql
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
~~~

To build a *booster* for a given Magic™ set, we have to randomly select a
card of the right *rarity* for each one of the *booster*'s slots. Remember
that each *booster* slot is an array of rarity alternative choices, so a
card of any of those rarity levels may be choosen for that slot entry.

Our task is now to rewrite the same sentence, only in SQL:

~~~ sql
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
~~~

Compare to the query used before we normalized the data set, which had to
*unnest* the booster definition in a first CTE with the
*jsonb_array_elements_text* function, and also to capitalize the obtained
*rarity* to be able to compare it to the *data->>rarity* of all the cards…

This previous query also had to aggregate the count of each rarity asked for
in the booster so as to then use a *limit booster.count* in the lateral
subquery in order to fetch the right amount of cards.

In the “normalized enough” version of the query, we still have a *left join
lateral* dance with an *order by random() limit 1* which allows to randomly
fetch a single card per open slot in the booster definition, and we retain a
booster card entry position.

Also this time we spell the *array contains element* operation differently.
We could have used the `@>` operator support for PostgreSQL arrays, as in
`booster.rarity @> array[card.rarity]`, but chose to use the [=
any](https://www.postgresql.org/docs/current/static/functions-comparisons.html)
operator instead, as in `card.rarity = any(booster.rarity)`.

And here's an instance of such a booster:

~~~
 num │   rarity   │        name         │  rarity  │          cardtype          
═════╪════════════╪═════════════════════╪══════════╪════════════════════════════
   1 │ {Rare}     │ Defiant Bloodlord   │ Rare     │ Creature — Vampire
   2 │ {Uncommon} │ Castle              │ Uncommon │ Enchantment
   3 │ {Uncommon} │ Wall of Omens       │ Uncommon │ Creature — Wall
   4 │ {Uncommon} │ Leeching Licid      │ Uncommon │ Creature — Licid
   5 │ {Common}   │ Dizzy Spell         │ Common   │ Instant
   6 │ {Common}   │ Fertilid            │ Common   │ Creature — Elemental
   7 │ {Common}   │ Sibsig Host         │ Common   │ Creature — Zombie
   8 │ {Common}   │ Shock               │ Common   │ Instant
   9 │ {Common}   │ Zombie Outlander    │ Common   │ Creature — Zombie Scout
  10 │ {Common}   │ Hyena Umbra         │ Common   │ Enchantment — Aura
  11 │ {Common}   │ Lay of the Land     │ Common   │ Sorcery
  12 │ {Common}   │ Sandblast           │ Common   │ Instant
  13 │ {Common}   │ Varchild's Crusader │ Common   │ Creature — Human Knight
  14 │ {Common}   │ Aviary Mechanic     │ Common   │ Creature — Dwarf Artificer
  15 │ {Common}   │ Fertile Ground      │ Common   │ Enchantment — Aura
(15 rows)
~~~

Now, we can also try to generate a booster of cards for the *Dark Ascension*
set, which contains specification for cards rarity we don't have in our
collection. For that we run the same query as previously, and before running
it we issue the command `\set collection 'DKA'`. We get the following
result:

~~~
 num │        rarity         │         name         │  rarity  │            cardtype            
═════╪═══════════════════════╪══════════════════════╪══════════╪════════════════════════════════
   1 │ {Rare,"Mythic Rare"}  │ Blessed Reversal     │ Rare     │ Instant
   2 │ {Uncommon}            │ Brain Pry            │ Uncommon │ Sorcery
   3 │ {Uncommon}            │ Goldmeadow Lookout   │ Uncommon │ Creature — Kithkin Spellshaper
   4 │ {Uncommon}            │ Explosive Revelation │ Uncommon │ Sorcery
   5 │ {Common}              │ Orzhov Basilica      │ Common   │ Land
   6 │ {Common}              │ Tinder Farm          │ Common   │ Land
   7 │ {Common}              │ Dark Banishing       │ Common   │ Instant
   8 │ {Common}              │ Elvish Warrior       │ Common   │ Creature — Elf Warrior
   9 │ {Common}              │ Aether Swooper       │ Common   │ Creature — Vedalken Artificer
  10 │ {Common}              │ Divine Offering      │ Common   │ Instant
  11 │ {Common}              │ Skyhunter Patrol     │ Common   │ Creature — Cat Knight
  12 │ {Common}              │ Orcish Conscripts    │ Common   │ Creature — Orc
  13 │ {Common}              │ Cloud Sprite         │ Common   │ Creature — Faerie
  14 │ {Common}              │ Vedalken Dismisser   │ Common   │ Creature — Vedalken Wizard
  15 │ {Land}                │ ¤                    │ ¤        │ ¤
  16 │ {Marketing,Checklist} │ ¤                    │ ¤        │ ¤
(16 rows)
~~~

As there's no card of rarities *Land*, *Marketing*, or *Checklist* in our
set, then those booster's slot requiring them are left empty.

# Conclusion

PostgreSQL is exceptionally good at providing a modern implementation of the
SQL language. Its extensible design means PostgreSQL is capable of
integrating powerful and advanced features from the non-relational world in
a beautiful way. When processing JSON documents in PostgreSQL, we are still
doing SQL and benefiting from its many features. Of course the
implementation of *unnest* functions and the *lateral join* operation
against them are key to a great success, as we saw in this article.

{{< image classes="fig25 left dim-margin"
              src="/img/icon-sql-json.png" >}}
              
Still, it proves much easier to both maintain data quality and write
analytics queries when using a *normalized* database model. PostgreSQL
provides some tricks beyond the classic *Normal Forms* and allows working
with e.g. *enumerated types*, *arrays*, and *json* data: this is a trade-off
one might find interesting when relaxing *foreign key* references in a
database schema.

Of course, being used to SQL, my own preference leans towards a normalized
schema. Also, I know that PostgreSQL has been designed and implemented to
make the best out of such a schema, with an advanced query planner and
optimizer and several fine tuned algorithms to implement the supported join
operations.

Well, PostgreSQL is YeSQL!


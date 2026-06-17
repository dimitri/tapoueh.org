+++
title = "The Lab: PostgreSQL Practice on Real Data"
date = "2026-07-01T00:00:00+00:00"
tags = ["PostgreSQL", "SQL", "Lab"]
categories = ["PostgreSQL", "YeSQL"]
+++

Reading a SQL query is not the same as writing one. The step that bridges
the two is running queries — against real data, in a real PostgreSQL
server, where you can change things and see what happens.

The Lab exists for that. It is a Docker-based PostgreSQL environment built
around the datasets used in [The Art of PostgreSQL](https://theartofpostgresql.com):
the full Formula 1 race history since 1950, the MoMA collection, the
PostgreSQL project's own commit log, and several others. The SQL query
files from the book — around 500 of them — are available inside the
container, organised by chapter, ready to run.

The Lab is free. You do not need the book to use it.

<!--more-->
<!--toc-->

## Setup

You need Docker (Desktop on Mac/Windows, Engine on Linux). Then:

```sh
git clone https://github.com/dimitri/TheArtOfPostgreSQL.git
cd TheArtOfPostgreSQL
docker compose build
docker compose up -d postgres
docker compose run --rm taop load-data
```

`load-data` fetches and imports every dataset. It prints a summary when
done:

```
             dataset     timing     status
 -------------------  ---------  ---------
              scan34     0.065s         ok
               tweet     4.801s         ok
               rates     0.175s         ok
            pubnames     0.557s         ok
               magic     0.650s         ok
                f1db     0.392s         ok
                moma     0.034s         ok
            opendata     0.328s         ok
                 eav     0.030s         ok
             sandbox     2.278s         ok
 -------------------  ---------
               TOTAL     9.310s
```

Then open a psql session:

```sh
docker compose run --rm -it psql
```

You land in a `taop` database with all schemas ready.

## The queries

The `queries/` directory mirrors the book's chapter structure. Inside psql,
`\i` runs any file relative to that directory:

```sql
\i 04-sql-select/15-sql-102/03_01_f1db.decade.top3.sql
```

```
 decade │ rank │ forename  │  surname   │ wins
════════╪══════╪═══════════╪════════════╪══════
   1950 │    1 │ Juan      │ Fangio     │   24
   1950 │    2 │ Alberto   │ Ascari     │   13
   1960 │    1 │ Jim       │ Clark      │   25
   1970 │    1 │ Niki      │ Lauda      │   17
   1980 │    1 │ Alain     │ Prost      │   39
   1980 │    2 │ Ayrton    │ Senna      │   20
   1990 │    1 │ Michael   │ Schumacher │   35
   2000 │    1 │ Michael   │ Schumacher │   56
   2010 │    1 │ Lewis     │ Hamilton   │   46
```

Top three drivers per decade, from raw race results, in a single query.
That query uses a window function, aggregation, and a LATERAL join. If
that looks unfamiliar, it is a good starting point.

## The datasets

Each dataset is real and used for real questions in the book and the
courses:

| Schema | What it covers |
|--------|---------------|
| `f1db` | Every Formula 1 race since 1950 — drivers, constructors, results, lap times, pit stops |
| `moma` | The Museum of Modern Art's public collection: ~140 000 artworks, ~15 000 artists |
| `commitlog` | The PostgreSQL project's git history since 2000: author, committer, timestamps |
| `sandbox` | Generated categories, articles, and comments for data-modelling experiments |
| `hydrorivers` | HydroRIVERS France: every river reach as a PostGIS geometry with a parent reference |
| `rates` | Historical currency exchange rates |
| `magic` | Magic: The Gathering card database |

## The starter kit

The `starter-kit/` directory contains six guided walkthroughs, each
focused on one SQL pattern. They are Markdown files — read them in a
browser or editor alongside your psql session. Each one points to the
relevant `\i` path for every query it discusses:

1. Nested LATERAL joins — top-N per group
2. GROUPING SETS + FILTER — multiple aggregations in one scan
3. `percentile_cont()` — multiple percentiles at once
4. k-Nearest-Neighbour search with `<->` and a GiST index
5. A density map rendered as text with GROUP BY and PostGIS
6. WITH RECURSIVE — walking the Loire basin upstream

The last three require PostGIS and the `hydrorivers` dataset. Each
walkthrough takes around 15–30 minutes.

## What comes next

The articles coming out of this blog over the next months are all built on
the Lab. Every query runs against one of the datasets above. When an
article references a `\i` path, that file is in your container.

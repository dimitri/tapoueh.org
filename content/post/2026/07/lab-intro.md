+++
title = "The Lab: A Free PostgreSQL Practice Environment"
date = "2026-07-01T00:00:00+00:00"
tags = ["PostgreSQL", "SQL", "Lab", "Learning"]
categories = ["PostgreSQL", "YeSQL"]
+++

SQL is a skill you build by doing, not by reading. You can follow every
example in a book and still feel lost the moment you sit down at a real
database. The gap between *understanding* a query and *writing* one from
scratch is where most people get stuck.

That is why [The Art of PostgreSQL](https://theartofpostgresql.com)
ships with **The Lab** — a free, self-contained PostgreSQL practice
environment that runs on your laptop, no cloud account or DBA required.
Every query in the book has already been tested here; every example in
these articles is meant to run here too.

<!--more-->
<!--toc-->

## What The Lab is

The Lab is a Docker image that starts a PostgreSQL server pre-loaded with
the book's datasets. You get a full `psql` session, all the data, and a
set of guided SQL lessons — ready in two commands.

All you need is Docker Desktop (or the Docker Engine on Linux):

```sh
git clone https://github.com/dimitri/TheArtOfPostgreSQL.git
cd TheArtOfPostgreSQL
docker compose run --rm -it psql
```

That drops you into psql connected to a database called `taop`, with
every dataset already loaded and every lesson file available at `\i`.

## What datasets are included

Several real-world and synthetic datasets ship with The Lab:

| Schema | What it is |
|--------|-----------|
| `f1db` | Full Formula 1 race results from 1950 to the present season — drivers, constructors, circuits, lap times, pit stops, standings |
| `moma` | The Museum of Modern Art's collection: ~140 000 artworks, ~15 000 artists |
| `commitlog` | PostgreSQL's own git history: author, timestamps, diff stats for every commit since 2000 |
| `sandbox` | Synthetic categories, articles, and comments — generated data for modeling experiments |
| `hydrorivers` | The HydroRIVERS dataset for France: every reach of every river, with geometry |
| `rates` | Historical currency exchange rates |
| `magic` | Magic: The Gathering card database |

Most articles on this blog use one of these schemas. If you have The Lab
running, you can execute every example yourself, tweak it, break it, and
learn from all three.

## The starter-kit lessons

Six guided lessons come with The Lab, each aimed at a technique that
looks intimidating until you see it step by step:

1. **Nested LATERAL joins** — Top-N per group: most recent articles per
   category, top comments per article, all in a single query.

2. **GROUPING SETS + FILTER** — Compute driver and constructor champions
   in Formula 1 in one table scan; no UNION, no subquery per dimension.

3. **percentile\_cont() with arrays** — Median, 90th, 95th, and 99th
   percentile of PostgreSQL commit times, all in one ordered-set
   aggregate call.

4. **k-Nearest-Neighbour search** — Find the closest rows with the `<->`
   distance operator and a GiST index.

5. **A text map** — Render a density heat-map as Unicode block characters
   using nothing but `GROUP BY` and a PostGIS geometry column.

6. **WITH RECURSIVE on a river network** — Walk the entire Loire basin
   upstream from the sea using a parent-reference column and a recursive
   CTE.

You can load any lesson with `\i starter-kit/01-nested-lateral.md`
(psql strips the Markdown and runs the SQL blocks).

## What comes next

The next few months of posts here will be built on The Lab. Each one
will focus on one SQL technique — a feature from a recent PostgreSQL
release, a pattern from the book, or a lesson from the starter kit — and
every query will be one you can run yourself in the environment above.

If you want the posts in your inbox as they come out, the subscribe form
below is the right place.

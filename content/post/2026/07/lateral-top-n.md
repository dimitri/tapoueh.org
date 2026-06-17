+++
title = "Top-N Per Group with a LATERAL Join"
date = "2026-07-15T00:00:00+00:00"
tags = ["PostgreSQL", "SQL", "LATERAL", "Lab"]
categories = ["PostgreSQL", "YeSQL"]
+++

"Give me the three most recent articles in each category." This sounds
simple and it is — once you know `LATERAL`. Without it, the query either
gets a correlated subquery per category (slow) or a window function with a
filter (workable but indirect). With `LATERAL`, the intention is direct and
the planner can use indexes properly.

<!--more-->
<!--toc-->

## The schema

The Lab's `sandbox` schema has three tables: `category`, `article`, and
`comment`. Articles belong to a category; comments belong to an article.
We will build from a simple join up to a nested LATERAL that collects the
top comments on each top article.

```sql
\dt sandbox.*
```

```
 Schema  │   Name   │ Type  │ Owner
═════════╪══════════╪═══════╪═══════
 sandbox │ article  │ table │ taop
 sandbox │ category │ table │ taop
 sandbox │ comment  │ table │ taop
 sandbox │ lorem    │ table │ taop
```

## Step 1: a plain JOIN

Start by counting articles per category to understand the data:

```sql
  select category.name, count(article.id)
    from sandbox.category
         join sandbox.article on article.category = category.id
group by category.name
order by count desc;
```

```
    name    │ count
════════════╪═══════
 box office │   343
 news       │   329
 sport      │   170
 music      │   158
```

Four categories, a few hundred articles each. Now: how do you get just
the most recent one per category?

## Step 2: add LATERAL

A `LATERAL` subquery is like a correlated subquery in the `FROM` clause.
It can reference columns from the tables listed before it — here,
`category.id` — and it is executed once per row of those tables. Crucially,
you can `LIMIT` it:

```sql
select category.name,
       article.title,
       to_char(article.pubdate, 'YYYY-MM-DD') as pubdate
  from sandbox.category
  join lateral (
      select id, title, pubdate
        from sandbox.article
       where category = category.id
    order by pubdate desc
       limit 1
  ) as article on true;
```

```
    name    │                    title                     │  pubdate
════════════╪══════════════════════════════════════════════╪════════════
 sport      │ Magna Ut Distinctio Aut Itaque               │ 2026-04-19
 news       │ Aliquip Eius Consectetur Quas Delectus       │ 2026-04-20
 box office │ Sit Odio Rem Aperiam Doloribus               │ 2026-04-20
 music      │ Reprehenderit Adipiscing Nihil Neque Aliquid │ 2026-04-19
```

One row per category. Change `limit 1` to `limit 3` and you get the top
three — something a window function can do too, but `LATERAL` reads more
naturally as intent and lets the planner push the `LIMIT` into the
index scan.

## Step 3: a second LATERAL for comments

Now nest another `LATERAL` *inside* the first — or rather, listed after it
in the `FROM` clause, referencing the article that the first LATERAL already
found. This gives us the three most recent comments on each top article:

```sql
select category.name,
       article.title,
       comment.id,
       to_char(comment.pubdate, 'YYYY-MM-DD') as pubdate
  from sandbox.category
  join lateral (
      select id, title, pubdate
        from sandbox.article
       where category = category.id
    order by pubdate desc
       limit 1
  ) as article on true
  join lateral (
      select id, content, pubdate
        from sandbox.comment
       where article = article.id
    order by pubdate desc
       limit 3
  ) as comment on true;
```

Four categories × 1 article × 3 comments = 12 rows, each giving you a
category, its newest article, and one of that article's three newest comments.

## Step 4: collapse comments into JSON

Twelve rows is awkward to process in application code. `jsonb_agg()` folds
the comment rows back into a single JSONB array per article, so the result
is one row per category:

```sql
\set comments 3
\set articles 1

  select category.name as category,
         article.pubdate,
         title,
         jsonb_pretty(comments) as comments
    from sandbox.category
         left join lateral (
            select id, title, article.pubdate,
                   jsonb_agg(comment) as comments
              from sandbox.article
                   left join lateral (
                       select comment.pubdate,
                              substring(comment.content from 1 for 25) || '…'
                              as content
                         from sandbox.comment
                        where comment.article = article.id
                     order by comment.pubdate desc
                        limit :comments
                   ) as comment on true
             where category = category.id
          group by article.id
          order by article.pubdate desc
             limit :articles
         ) as article on true
order by category.name, article.pubdate desc;
```

The `:comments` and `:articles` variables let you change the limits at the
psql prompt without touching the query.

## When to reach for LATERAL

- **Top-N per group** with a known N and an index on the ordering column —
  `LATERAL` is usually faster than `ROW_NUMBER() OVER (...)` because the
  planner can use an index scan with a stop condition.
- **Parameterised subqueries** — when the subquery depends on a value from
  the outer row (a distance, a date range, a category id) and needs its own
  `ORDER BY` or `LIMIT`.
- **Set-returning functions** — `LATERAL` is implicit when you call a
  function in `FROM`; you only need to write it explicitly with subqueries.

{{< lab >}}
You can run this example directly in The Lab:

```sh
\i starter-kit/01-nested-lateral.md
```
{{< /lab >}}

+++
title = "Introducing sqlfmt: an SQL gofmt-style formatter"
date = "2026-08-11T14:30:37+0200"
tags = ["PostgreSQL", "SQL", "Tools"]
categories = ["PostgreSQL", "Tools"]
coverImage = ""
coverMeta = ""
thumbnailImage = ""
thumbnailImagePosition = "left"
icon = "🛠️"
+++

Formatting SQL tends to bring some of the same questions again and again:
should we uppercase clause keywords? should we put the separating comma at
the start of a line to ease refactoring? how to align the SQL clauses with
one-another?

Over the years I have grown my own SQL style and didn't find tooling that
would implement it. Also, I've been asked here and there if there is a tool
that would replicate [The Art of PostgreSQL](https://theartofpostgresql.com)
SQL indentation style... and now there is finally a good answer to that
question!

[sqlfmt](https://github.com/dimitri/sqlfmt) is a `gofmt`-style formatter
that implements my own favorite SQL indentation style. One opinionated
style, no configuration knobs. Run it, commit the result, move on.

<!--more-->

<!--toc-->

---

## The style

The formatting convention comes from [*The Art of
PostgreSQL*](https://theartofpostgresql.com/) — specifically, from the
hand-formatted query corpus that runs through the book's several hundred
worked examples.

The defining characteristic is **river alignment**: at each query nesting
level, every clause keyword (`select`, `from`, `where`, `group by`,
`having`, `order by`) is right-padded to the same column, so the keywords
form a vertical river and the expressions that follow them flow naturally to
the right.

Take a flat, unformatted query:

```sql
select status, count(*) from results join races using(raceid) where date >= :season group by status having count(*) >= 10 order by count(*) desc;
```

After `sqlfmt`:

```sql
  select status, count(*)
    from results
         join races using(raceid)
   where date >= :season
group by status
  having count(*) >= 10
order by count(*) desc;
```

Every keyword above ends at the same column. `group by` and `order by` are
eight characters — longer than `select`'s six — so they sit flush-left at
base indent. That is a side effect of the alignment rule, not a separate
exception for those keywords.

The full rule set is documented in
[`STYLE.md`](https://github.com/dimitri/sqlfmt/blob/main/STYLE.md) in the
repository, reverse-engineered from the 343 hand-formatted `.sql` files in
the book's own query corpus. A few highlights:

- All SQL keywords and function names are **lowercase** — `count(*)`,
  `coalesce(...)`, `row_number() over(...)`.
- **Trailing commas** on column lists, one column per line after the first.
- `and`/`or` at the **start** of continuation lines, right-aligned to end at
  the same column as `where`.
- Columns in `CREATE TABLE` are **left-padded** so every data type starts in
  the same column.
- Comments are never discarded — leading comments are reindented and
  reflowed to 78 columns; trailing comments in a block are padded to a
  shared column.

That style is known to best fit SQL queries maintained in their own `.sql`
files rather than integrated in another source code file as a string.

If you're not using an ORM but also maintaining hand-written SQL as a set of
static strings within another programming language's source code, it might
be time to see how to manage SQL queries in their own files.

---

## Try it now — no install required

{{< lab >}}
The [PostgreSQL SQL Formatter](https://theartofpostgresql.com/postgresql-sql-formatter/)
runs `sqlfmt` directly in your browser via WebAssembly — paste a query, press
**⌘ / Ctrl + Enter**, and get it back in the river style. Free, no signup.
{{< /lab >}}

The web tool runs the exact same Go engine as the CLI, compiled to
WebAssembly with TinyGo. The compressed payload is roughly 130 KB — a better
fit for a web page integration compared to the 2.9 MB a standard Go WASM
build would produce.

---

## CLI usage

The interface follows `gofmt` exactly:

```sh
sqlfmt query.sql              # print formatted output to stdout
sqlfmt -w query.sql           # rewrite the file in place
sqlfmt -l queries/**/*.sql    # list files whose formatting would change
sqlfmt -d query.sql           # show a unified diff instead of full output
cat query.sql | sqlfmt        # stdin → stdout, pipeable
```

The `-l` flag is useful in CI: exit code 1 if any file would change, so a
`sqlfmt -l $(git diff --name-only '*.sql')` step enforces style on every pull
request without storing the formatted output in the pipeline.

Install with:

```sh
go install github.com/dimitri/sqlfmt/cmd/sqlfmt@latest
```

---

## Editor integration

### Emacs

Drop `sqlfmt.el` on your load path and add a hook:

```elisp
(add-to-list 'load-path "~/dev/sqlfmt/editors/emacs")
(add-hook 'sql-mode-hook #'sqlfmt-mode)
```

With `sqlfmt-mode` active, `C-M-h` selects the statement at point and `TAB`
reformats it. `sqlfmt-before-save-hook` can be used for format-on-save.

### Vim / Neovim

The plugin wires sqlfmt into Vim's `formatprg`/`equalprg`, so the usual
motion operators work:

```vim
gqip    " reformat the paragraph under the cursor
gg=G    " reformat the whole buffer
:%!sqlfmt
```

---

## Why a tokenizer, not an AST

Most production SQL formatters — `pg_format`, `sqlfluff`, `prettier-plugin-sql`
— are built on token streams rather than parse trees, and sqlfmt follows the
same approach. Two reasons matter in practice:

**Comments.**  PostgreSQL's own parser discards comments; any AST-based
formatter needs a separate comment-recovery pass. At that point most of the
advantage of "let the parser handle structure" is already gone.

**Robustness.**  The web widget needs to handle whatever a visitor pastes —
partial statements, snippets from a larger file, syntax that isn't perfectly
valid. A token-stream approach degrades gracefully; a grammar-based one fails
hard.

River alignment is fundamentally about where tokens sit on the page, not about
the query's semantic structure, so the tokenizer approach fits the problem
naturally. The test suite uses `pganalyze/pg_query_go` (wrapping the real
PostgreSQL C parser) as a correctness oracle: if `fingerprint(input) ==
fingerprint(format.Format(input))`, formatting never silently changed what the
query means.

---

## Status

The formatter is a working implementation. The tokenizer, river-alignment
layout engine, comment attachment, and CLI are all in place and covered by a
round-trip corpus test against real book queries. Recent fixes closed several
edge cases: `<->` (KNN distance operator) was previously mis-lexed; `UNION
ALL`/`INTERSECT`/`EXCEPT` between CTEs now correctly resets the river; `with
recursive` no longer silently drops the `RECURSIVE` keyword.

Deeply nested subqueries and exotic DDL remain best-effort — `STYLE.md` itself
acknowledges these as the least mechanically rigid areas of the style, where
the book corpus shows hand-tuning rather than a consistent rule.

The source is at
[github.com/dimitri/sqlfmt](https://github.com/dimitri/sqlfmt). The live
formatter is at
[theartofpostgresql.com/postgresql-sql-formatter](https://theartofpostgresql.com/postgresql-sql-formatter/).
Who knows, this might prove useful beyond now being able to answer when asked
about which tooling I'm using to format the SQL queries in the book!

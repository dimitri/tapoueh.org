# tapoueh.org — redesign & content strategy

Working document for the 2026 redesign of tapoueh.org. Captures the strategy,
what has been built, the technical decisions, and the open roadmap. Branch:
`redesign-2026`.

## 1. Strategy in one paragraph

tapoueh.org is a **personal, non-commercial** technical blog that is syndicated
to [Planet PostgreSQL](https://planet.postgresql.org/) — broad reach, but the
syndicated post body must stay ad-free. theartofpostgresql.com (and its
siblings) are the **commercial** properties with a strong funnel but little
distribution. The plan uses tapoueh.org as **top of funnel** (reach +
credibility + email capture) and the commercial sites as **bottom of funnel**
(conversion), bridged by an owned **mailing list**. All selling lives in the
site *chrome* (header/footer/end-of-post), never inside `.Content`, so Planet
syndication stays clean. The KPI is **mailing-list signups**, not direct sales.

## 2. The ecosystem (Dimitri's properties)

- **tapoueh.org** — this repo. Personal blog. Hugo. Aggregated on Planet.
- **theartofpostgresql.com** — the book + course + workshop + lab marketing
  site. Hugo theme `taop` at `/Users/dim/dev/TAOP/taop.xyz`.
- **oss.theartofpostgresql.com** — paid OSS maintenance offering. Hugo theme
  `oss` (fork of `taop`) at `/Users/dim/dev/TAOP/oss.taop.xyz`.
- **The book manuscript**: `/Users/dim/dev/TAOP/taop-vol-1` (pandoc/LaTeX).
- **The Lab** (free practice env): `/Users/dim/dev/TAOP/TheArtOfPostgreSQL`.

Shared brand DNA reused here: **Bookman** (self-hosted serif) + **Lato** (body),
aubergine `#372649` / `#67527a` / `#2e2241` with cyan `#60b2d3` accent. We
inherit the *tokens* but drop the marketing *register* (no hero carousels,
sticky buy bars, pricing tables) for a quiet, reading-first editorial feel.

## 3. What has been built (this branch)

### Tooling / runtime
- **Dockerized Hugo** — no local Go/Hugo toolchain needed (local Go is pinned by
  an old Xcode). `docker-compose.yml` + `Makefile`:
  - `make serve` → live dev server at http://localhost:1313 (drafts + future
    posts visible).
  - `make build` → production build into `./docs`.
  - `make check` → build to memory, fail on errors (CI/pre-commit).
  - Image: `hugomods/hugo:exts` (currently Hugo 0.154.5 extended).
- **CI/CD** — `.github/workflows/deploy.yml`: build + deploy to GitHub Pages on
  push to `master`, on manual dispatch, and on a **daily 06:00 cron**. The cron
  is the metronome for scheduled publishing (see §5). Generated `docs/` is no
  longer committed (`.gitignore`); `static/CNAME` preserves the custom domain.

### Theme (`themes/tapoueh`)
Purpose-built theme replacing the 2017-era tranquilpeak theme.
- CSS design system in `assets/css/main.css` (CSS custom properties, Chroma
  syntax highlighting, self-hosted Bookman web fonts in `static/fonts`).
- Layouts: `baseof`, `index` (writing archive + intro + subscribe), `single`
  (article, `<!--toc-->` marker honored, end-of-post subscribe), `list`,
  `term` (taxonomy term), `terms` (Topics hub), `conf/list` (talks **timeline**,
  one rail per year), `conf/single` (talk: YouTube embed + local-PDF slides +
  abstract + narrative), `projects` (data-driven card grid).
- Partials: `head`, `header` (aubergine bar, icon-led nav), `footer`
  (social + ecosystem), `post-card` (with topical thumbnail), `post-meta`,
  `pagination`, `subscribe` (the funnel), `icon` (inline SVG set).
- Shortcodes: `image`, `alert`, `ckbookcover` (reimplemented for the new design),
  `lab` (in-content "run it in the Lab" note), `ecosystem` (book/course/
  workshop/lab/oss card).

### Information architecture
- **Writing** (`/`) — paginated archive, list rows with topical thumbnails.
- **Talks** (`/conf/`) — year-by-year timeline; each talk page has video +
  slides + abstract, and a `.Content` body for the *experience* narrative
  (to be backfilled from old "Conferences" blog posts).
- **Topics** (`/categories/`) — hub showing all categories (weighted by size)
  plus the top tags, with counts.
- **Projects** (`/projects/`) — PostgreSQL/data tools: pgloader, pg_auto_failover,
  pgcopydb, prefix, pg_staging (from `data/projects.toml`).
- **Emacs Lisp** (`/emacs/`) — separate project page: el-get, switch-window,
  cssh, rcirc-groups.
- **About** (`/about/`) — bio + PostgreSQL/Debian contributions + ecosystem
  card. Project detail removed (now on Projects).

### Funnel placement (Planet-safe by construction)
- End-of-post **subscribe** band on every article and talk (in template chrome,
  outside `.Content`).
- In-content **Lab note** shortcode (value-first; the Lab is free).
- **Ecosystem** card on About + ecosystem row in the footer.
- Mailing list provider: ConvertKit/Kit (params `convertkitUid` +
  `convertkitSrc`); a styled fallback form shows until the dedicated tapoueh
  form is created.

## 4. Key technical decisions

- **Hugo 0.154 via Docker**, not the local 0.83.1. The new theme targets modern
  Hugo; tranquilpeak is retired (it broke on 0.154: `.Site.Author`, `UniqueID`,
  `DisqusShortname`, internal GA-async template all removed).
- **Brand title is the person** ("Dimitri Fontaine"), not the book.
- **No committed build output** — `docs/` is built and deployed by CI.
- **Left-aligned reading column** (~688px), 19px Lato, not the marketing sites'
  justified text.
- Content compatibility preserved: the 292 posts' `image`/`alert` shortcodes,
  `<!--toc-->`/`<!--more-->` markers, and `thumbnailImage` front matter all work.

## 5. Editorial plan (scheduled publishing)

Hugo gates future-dated content out of the build by default; the CI cron is the
release metronome. Workflow:
1. Write a batch of posts up front (target: 6–12 months at ~2/month).
2. Give each a future `date` on a 1st/15th cadence; commit them all now.
3. The daily CI build releases each post on its date → it enters `index.xml` →
   Planet picks it up within ~an hour.

Content sources (most blog-ready first):
- **Lab starter-kit** lessons (maps-from-SQL, LATERAL top-N, kNN/PostGIS,
  recursive rivers, GROUPING SETS, percentiles).
- **Book modernization** topics from `taop-vol-1/content-gaps.md` (MERGE,
  window frames, JSON_TABLE, EXPLAIN primer, …).
- Every new post is built on **the Lab** so readers can run the queries.

**First post**: an introduction to the Lab — the Planet wake-up. 100%
standalone-useful, zero sales, fully Planet-safe.

## 6. Open items / TODO

- [ ] Create the dedicated **tapoueh ConvertKit form**; set `convertkitUid` +
      `convertkitSrc` in `config.toml`.
- [ ] In GitHub repo **Settings → Pages → Source = "GitHub Actions"** (currently
      "Deploy from `docs/`") so the new workflow deploys.
- [ ] Confirm **course/workshop URLs** (guessed `/course/`, `/workshop/`).
- [ ] Draft the **Lab introduction** post (first scheduled article).
- [ ] Build the 6–12 month **post queue** with future dates.
- [ ] Backfill **talk narratives** from old "Conferences" blog posts.
- [ ] Add privacy-friendly **analytics** (GA4 or Plausible) — UA is dead.
- [ ] Cleanup: remove the unused `themes/tranquilpeak` and stale
      `static/css/{dim,nav}.css`.
- [ ] Decide on **dark mode** (deferred).

## 7. How to run

```sh
make serve     # http://localhost:1313 (live reload; shows drafts + future posts)
make build     # production build into ./docs
make check     # build to memory, fail on error
```

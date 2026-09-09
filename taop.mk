# taop.mk -- opt-in build rules for an article authored with the TAOP
# markdown directives.
#
# Nothing here applies to the site as a whole. An article opts in by
# creating its own Makefile that sets a variable or two and includes this
# file; every other post is untouched and keeps building exactly as
# before.
#
#   # content/post/2026/09/plan-advice/Makefile
#   include ../../../../taop.mk
#
# What it gives that pasting output by hand does not: the SQL lives in
# sql/*.sql and its captured output in results/*.out, each included once
# by a directive instead of duplicated into the prose. Those copies drift
# -- in this very article, five of seven pasted result blocks had already
# diverged from the files sitting beside them.
#
# The tool is taopmd, from the app.taop.xyz repository. It is not
# published: build it there and put it on PATH, or point TAOPMD at it.
#
#   cd ~/dev/TAOP/app.taop.xyz && make taopmd && cp bin/taopmd ~/bin/
#
# Targets:
#   make            fmt, run the queries, render      (the whole thing)
#   make render     render only; never opens a database
#   make run        execute the queries, refresh results/
#   make fmt        put the article's SQL in house style
#   make figs       compile the hand-authored TikZ figures to SVG
#   make prune      list artifacts whose query is gone (-f to delete)
#   make check      is taopmd present, and does the source parse

TAOPMD ?= taopmd

# SRC is the authored document; OUT is what Hugo publishes. Hugo has no
# pre-parse hook -- its render hooks fire on parsed elements, never on raw
# text -- so this is a build step, not something Hugo can call. The .src.md
# extension is not one Hugo recognises, so the source is ignored by the
# site without any ignoreFiles configuration.
SRC ?= index.src.md
OUT ?= index.md

# The lab image every TAOP query is written against. Override CONTAINER
# for a different instance, or set DSN to skip Docker entirely.
CONTAINER ?= apptaopxyz-lab-1
DSN       ?=

# tapoueh.org tags captured output ```results and trims psql's row-count
# footer, which is what the hand-written articles already do.
CAPTURE_LANG   ?= results
STRIP_ROWS     ?= -strip-row-count
# 0 leaves captured EXPLAIN output as psql wrapped it.
PLAN_WIDTH     ?= 0

ifneq ($(DSN),)
  RUN_TARGET := -db $(DSN)
else
  RUN_TARGET := -container $(CONTAINER)
endif

RENDER_FLAGS := -capture-lang $(CAPTURE_LANG) $(STRIP_ROWS) -width $(PLAN_WIDTH)

.PHONY: all article render run fmt figs prune check clean-rendered

all: article

article: check
	$(TAOPMD) build -o $(OUT) $(RENDER_FLAGS) $(RUN_TARGET) $(SRC)

render: check
	$(TAOPMD) render -o $(OUT) $(RENDER_FLAGS) $(SRC)

run: check
	$(TAOPMD) run $(RUN_TARGET) .

fmt: check
	$(TAOPMD) fmt -w $(SRC)

prune: check
	$(TAOPMD) prune .

# The conceptual TikZ figures are not plan diagrams and still need a real
# LaTeX toolchain. Only run when the article has any.
figs:
	@if ls fig-*.tex >/dev/null 2>&1; then \
	  for f in fig-*.tex; do \
	    b=$${f%.tex}; \
	    [ $$b.svg -nt $$f ] && continue; \
	    echo "  TikZ  $$f"; \
	    tmp=$$(mktemp -d); \
	    printf '%s\n' '\documentclass[border=4pt]{standalone}' \
	      '\usepackage[dvipsnames,svgnames,x11names,table]{xcolor}' \
	      '\usepackage{array}' \
	      '\usepackage{tikz,pgfplots,adjustbox,amsmath,amssymb,fontawesome5}' \
	      '\usetikzlibrary{arrows.meta,positioning,shapes,fit,backgrounds,calc,decorations.pathreplacing,patterns}' \
	      '\pgfplotsset{compat=1.18}' '\begin{document}' > $$tmp/doc.tex; \
	    grep -v '\\begin{figure}\|\\end{figure}\|\\centering\|\\caption\|\\label' $$f >> $$tmp/doc.tex; \
	    printf '%s\n' '\end{document}' >> $$tmp/doc.tex; \
	    lualatex -interaction=nonstopmode -output-directory=$$tmp $$tmp/doc.tex > /dev/null 2>&1 || exit 1; \
	    pdf2svg $$tmp/doc.pdf $$b.svg; \
	    rm -rf $$tmp; \
	  done; \
	else echo "  no fig-*.tex here"; fi

check:
	@command -v $(TAOPMD) >/dev/null 2>&1 || { \
	  echo "taop.mk: $(TAOPMD) not found on PATH."; \
	  echo "  cd ~/dev/TAOP/app.taop.xyz && make taopmd && cp bin/taopmd ~/bin/"; \
	  echo "  (or: make TAOPMD=/path/to/taopmd)"; \
	  exit 1; }
	@test -f $(SRC) || { echo "taop.mk: no $(SRC) here"; exit 1; }

# The rendered file is a build artifact, but it IS committed: the site has
# to build without the tool installed, the same rule the captured .out
# files follow. This only exists to see what regenerating would change.
clean-rendered:
	rm -f $(OUT)

# taop.mk -- opt-in build rules for an article authored with the TAOP
# markdown directives.
#
# Nothing here applies to the site as a whole. An article opts in by
# creating its own Makefile that sets a variable or two and includes this
# file; every other post is untouched and keeps building exactly as
# before.
#
#   # content/post/2026/09/plan-advice/Makefile
#   TOPDIR := $(shell git rev-parse --show-toplevel)
#   include $(TOPDIR)/taop.mk
#
# What it gives that pasting output by hand does not: the SQL lives in
# sql/*.sql and its captured output in results/*.out, each included once
# by a directive instead of duplicated into the prose. Those copies drift
# -- in this very article, five of seven pasted result blocks had already
# diverged from the files sitting beside them.
#
# The tool is taopmd, from the app.taop.xyz repository. It is not
# published; install it from there, or point TAOPMD at a copy:
#
#   make -C ~/dev/TAOP/app.taop.xyz install
#
# That is `go install`, so taopmd lands in the Go bin path -- already on
# PATH for anyone who writes Go, which is everyone who would be running
# this. TAOPROOT below only has to be right for the message; nothing here
# reads that repository.
#
# Targets:
#   make            fmt, run the queries, render      (the whole thing)
#   make render     render only; never opens a database
#   make run        execute the queries, refresh results/
#   make fmt        put the article's SQL in house style
#   make figs       compile the hand-authored TikZ figures to SVG
#   make figs-force recompile every figure, up to date or not
#   make prune      list artifacts whose query is gone (-f to delete)
#   make check      is taopmd present, and does the source parse

TAOPMD ?= taopmd

# Where app.taop.xyz is checked out, for the "not installed" message. A
# guess, and only ever printed -- never used to build anything, so a
# wrong value costs a wrong suggestion and nothing else.
TAOPROOT ?= $(HOME)/dev/TAOP/app.taop.xyz

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

# The render conventions moved to taopmd.yaml at the repository root,
# where taopmd itself can read them -- see that file. They were flags
# here, which meant `taopmd build` in an article directory produced a
# different index.md from `make` in the same directory, because the
# conventions lived in this Makefile and the tool could not see them.

ifneq ($(DSN),)
  RUN_TARGET := -db $(DSN)
else
  RUN_TARGET := -container $(CONTAINER)
endif

.PHONY: all article render run fmt figs figs-force prune check tool clean-rendered

all: article

article: check
	$(TAOPMD) build -o $(OUT) $(RUN_TARGET) $(SRC)

render: check
	$(TAOPMD) render -o $(OUT) $(SRC)

run: check
	$(TAOPMD) run $(RUN_TARGET) .

fmt: check
	$(TAOPMD) fmt -w $(SRC)

prune: check
	$(TAOPMD) prune .

# The conceptual TikZ figures: hand-drawn TikZ that only a LaTeX run can
# turn into a picture, unlike the plan diagrams, which are rendered by Go.
#
# This used to be thirty lines of shell here -- a preamble written with
# printf, a grep stripping \caption out of each fragment, lualatex and
# pdf2svg driven by hand. taopmd carries the wrapper now, so the two
# articles that have figures stopped keeping two copies of the same loop.
figs: tool
	$(TAOPMD) figs

figs-force: tool
	$(TAOPMD) figs -f

# tool is "is taopmd here"; check is that plus "is there a document".
# Two, because figs needs only the first: an article can have figures and
# no \include{} directives at all, which is what pg19-preview is, and
# requiring a source it does not have is how that article ended up with
# its own copy of this check.
tool:
	@command -v $(TAOPMD) >/dev/null 2>&1 || { \
	  echo "taop.mk: $(TAOPMD) not found on PATH."; \
	  echo "  make -C $(TAOPROOT) install     installs taop and taopmd with go install"; \
	  echo "  (or: make TAOPMD=/path/to/taopmd)"; \
	  exit 1; }

check: tool
	@test -f $(SRC) || { echo "taop.mk: no $(SRC) here"; exit 1; }

# The rendered file is a build artifact, but it IS committed: the site has
# to build without the tool installed, the same rule the captured .out
# files follow. This only exists to see what regenerating would change.
clean-rendered:
	rm -f $(OUT)

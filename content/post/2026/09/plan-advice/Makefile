# Opt-in: this article is authored with the TAOP markdown directives and
# built with taopmd. Every target lives in the site's own taop.mk; no
# other post on this site is affected.
#
#   make          fmt, run the queries, render index.md from index.src.md
#   make render   render only, without touching a database
#   make figs     compile the TikZ figures (fig-*.tex) to SVG
#
# index.src.md is the source: prose plus \include{} directives naming the
# files in sql/ and results/. index.md is generated from it and committed,
# so the site still builds for anyone without taopmd.

# Where the site's taop.mk lives. `include ../../../../../taop.mk` worked
# and was unreadable: five levels of "..", correct only for an article at
# exactly this depth, and silently wrong the day a post moves a year.
#
# git answers the question directly, and MAKEFILE_LIST gives the fallback
# for a checkout without .git (an export, a tarball) -- walking up from
# THIS file rather than from $(CURDIR), so `make -C` somewhere else still
# finds the right root.
TOPDIR := $(shell git -C '$(dir $(abspath $(lastword $(MAKEFILE_LIST))))' \
                      rev-parse --show-toplevel 2>/dev/null)
ifeq ($(TOPDIR),)
TOPDIR := $(shell d='$(abspath $(dir $(lastword $(MAKEFILE_LIST))))'; \
                  while [ "$$d" != / ]; do \
                    [ -f "$$d/taop.mk" ] && { echo "$$d"; break; }; \
                    d=$$(dirname "$$d"); \
                  done)
endif
ifeq ($(TOPDIR),)
$(error cannot find the repository root: no git checkout and no taop.mk above $(CURDIR))
endif

include $(TOPDIR)/taop.mk

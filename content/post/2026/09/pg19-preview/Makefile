# The TikZ figures in this article, compiled to SVG.
#
# This was thirty lines of shell: a preamble written with printf, a grep
# to strip \caption and \label out of each fragment, then lualatex and
# pdf2svg driven by hand. The same loop had been copied into the
# plan-advice article, which is how one build step becomes two that
# disagree -- so it lives in taopmd now, wrapper and all.
#
# It then grew a second copy of taop.mk's own "is taopmd installed?"
# check, for the same reason: taop.mk's check also insisted on an
# index.src.md, and this article has none. taop.mk splits that in two now
# (tool, then check), so this file includes it like any other article and
# keeps nothing of its own but the default goal.
#
#   make            compile any fig-*.tex whose .svg is out of date
#   make figs-force recompile them all
#
# This article has no \include{} directives: the prose is written
# directly in index.md, so figs is the only target here that applies.

# Where the site's taop.mk lives -- git first, walking up as the fallback
# for a checkout without .git. See plan-advice/Makefile for why this is
# not a row of "..".
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

# taop.mk's own default is the whole fmt/run/render pipeline, which needs
# a source document this article does not have.
.DEFAULT_GOAL := figs

# The name this article's own Makefile used before it included taop.mk.
.PHONY: force
force: figs-force

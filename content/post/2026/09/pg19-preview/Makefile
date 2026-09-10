# The TikZ figures in this article, compiled to SVG.
#
# This was thirty lines of shell: a preamble written with printf, a grep
# to strip \caption and \label out of each fragment, then lualatex and
# pdf2svg driven by hand. The same loop had been copied into the
# plan-advice article, which is how one build step becomes two that
# disagree -- so it lives in taopmd now, wrapper and all.
#
# taopmd is from the app.taop.xyz repository. It is not published: build
# it there and put it on PATH, or point TAOPMD at it.
#
#   cd ~/dev/TAOP/app.taop.xyz && make taopmd && cp bin/taopmd ~/bin/
#
#   make        compile any fig-*.tex whose .svg is out of date
#   make force  compile them all
#
# This article has no \include{} directives, so it needs none of the
# other taop.mk targets: the prose is written directly in index.md.

TAOPMD ?= taopmd

.PHONY: all force check

all: check
	$(TAOPMD) figs

force: check
	$(TAOPMD) figs -f

check:
	@command -v $(TAOPMD) >/dev/null 2>&1 || { \
	  echo "Makefile: $(TAOPMD) not found on PATH."; \
	  echo "  cd ~/dev/TAOP/app.taop.xyz && make taopmd && cp bin/taopmd ~/bin/"; \
	  echo "  (or: make TAOPMD=/path/to/taopmd)"; \
	  exit 1; }

## Makefile — compile TikZ diagrams to SVG for the article
##
## Prerequisites:
##   brew install texlive   (or MacTeX / TeX Live with lualatex + standalone)
##   brew install pdf2svg
##
## Usage:
##   make          — build all SVGs
##   make clean    — remove generated files

DIAGRAMS := \
	fig-for-portion-of \
	fig-partition-merge

SVGS := $(addsuffix .svg, $(DIAGRAMS))

.PHONY: all clean

all: $(SVGS)

## Wrap each .tex snippet in a standalone document, compile to PDF, convert to PNG.
## Strips \begin{figure}/\end{figure}/\adjustbox wrappers that don't work in standalone.
## Uses printf '%s\n' (not echo) because zsh echo interprets \e as ESC (0x1b).
%.pdf: %.tex
	@echo "  LaTeX  $<"
	@tmpdir=$$(mktemp -d) && \
	  printf '%s\n' '\documentclass[border=4pt]{standalone}' > $$tmpdir/doc.tex && \
	  printf '%s\n' '\usepackage[dvipsnames,svgnames,x11names]{xcolor}' >> $$tmpdir/doc.tex && \
	  printf '%s\n' '\usepackage{tikz,pgfplots,adjustbox,amsmath,amssymb,fontawesome5}' >> $$tmpdir/doc.tex && \
	  printf '%s\n' '\usetikzlibrary{arrows.meta,positioning,shapes,fit,backgrounds,calc,decorations.pathreplacing,patterns}' >> $$tmpdir/doc.tex && \
	  printf '%s\n' '\pgfplotsset{compat=1.18}' >> $$tmpdir/doc.tex && \
	  printf '%s\n' '\begin{document}' >> $$tmpdir/doc.tex && \
	  grep -v '\\begin{figure}\|\\end{figure}\|\\centering\|\\caption\|\\label' $< >> $$tmpdir/doc.tex && \
	  printf '%s\n' '\end{document}' >> $$tmpdir/doc.tex && \
	  lualatex -interaction=nonstopmode -output-directory=$$tmpdir $$tmpdir/doc.tex > /dev/null 2>&1 && \
	  cp $$tmpdir/doc.pdf $@ && \
	  rm -rf $$tmpdir

%.svg: %.pdf
	@echo "  SVG    $@"
	pdf2svg $< $@

clean:
	rm -f $(SVGS) $(addsuffix .pdf, $(DIAGRAMS))

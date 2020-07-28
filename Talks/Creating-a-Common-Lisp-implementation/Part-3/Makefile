NAME=creating-a-common-lisp-implementation-part-3
LATEX=pdflatex
TEXFILES=$(NAME).tex $(shell ./tex-dependencies $(NAME).tex)
PSTEX_T=$(shell ./strip-dependence inputfig $(TEXFILES))
PSTEX=$(subst .pstex_t,.pstex,$(PSTEX_T))
EPS=$(shell ./strip-dependence inputeps $(TEXFILES))

FIG = $(wildcard *.fig) $(wildcard *.pstex_t)

.SUFFIXES:
.SUFFIXES: .fig .tex .pdf .bib .eps .pstex .pstex_t

all: $(NAME).pdf

$(NAME).pdf: $(TEXFILES) $(PSTEX_T) $(EPS) $(CODE) $(VERBATIM) Makefile
	$(LATEX) $(NAME).tex
	$(LATEX) $<

%.pdf: %.fig
	fig2dev -Lpdftex -m 0.60  $< $@

%.pdf_t: %.fig %.pdf
	fig2dev -Lpdftex_t -m 0.60 -p $(basename $<).pdf $< $@

.PHONY: clean spotless install init

init:
	ln -s ../v0-v1-v7.eps
clean:
	rm -f *.aux *.log *~ *.bak \#*\# $(NAME).aux $(NAME).log $(NAME).out \
	$(NAME).nav $(NAME).snm $(NAME).toc 

spotless:
	make clean
	rm -f  $(NAME).pdf $(PSTEX) $(PSTEX_T)

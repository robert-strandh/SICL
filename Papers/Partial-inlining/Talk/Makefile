NAME=talk
LATEX=pdflatex
TEXFILES=$(NAME).tex $(shell ./tex-dependencies $(NAME).tex)
EPS=$(shell ./strip-dependence inputeps $(TEXFILES))
PDF_T=$(shell ./strip-dependence inputfig $(TEXFILES))
PDF=$(subst .pdf_t,.pdf,$(PDF_T))

FIG = $(wildcard *.fig) $(wildcard *.pstex_t)

.SUFFIXES:
.SUFFIXES: .fig .tex .pdf .bib .eps .pstex .pstex_t

all: $(NAME).pdf

$(NAME).pdf: $(TEXFILES) $(PDF) $(PDF_T) $(EPS) $(CODE) $(VERBATIM) Makefile
	$(LATEX) $(NAME).tex
	$(LATEX) $<

%.pdf: %.fig
	fig2dev -Lpdftex -m 0.3  $< $@

%.pdf_t: %.fig %.pdf
	fig2dev -Lpdftex_t -m 0.3 -p $(basename $<).pdf $< $@

.PHONY: clean spotless install init

init:
	ln -s ../v0-v1-v7.eps
clean:
	rm -f *.aux *.log *~ *.bak \#*\# $(NAME).aux $(NAME).log $(NAME).out \
	$(NAME).nav $(NAME).snm $(NAME).toc 

spotless:
	make clean
	rm -f  $(NAME).pdf $(PSTEX) $(PSTEX_T)

install: $(NAME).pdf
	cp $(NAME).pdf /usr/labri/idurand/public_html/ELS2015/.

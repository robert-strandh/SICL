NAME=merge-sort

TEXFILES=$(NAME).tex $(shell ./tex-dependencies $(NAME).tex)
BIBFILES=$(NAME).bib
VERBATIM=$(shell ./strip-dependence verbatimtabinput $(TEXFILES))
CODEFILES=$(shell ./strip-dependence inputcode $(TEXFILES))
PDF_T=$(shell ./strip-dependence inputfig $(TEXFILES))
#PDF=$(subst .pdf_t,.pdf,$(PDF_T))
PDF=$(PDF_T:.pdf_t=.pdf)
#TESTFILE=v0-v1-v7
FORMAT=eps
#CURVE=$(TESTFILE).$(FORMAT)

.SUFFIXES:
.SUFFIXES: .fig .ps .dvi .tex .pdf .bib .eps .pstex .pstex_t .gp .png

all : $(NAME).pdf

%.pdf: %.fig
	fig2dev -Lpdftex -m 0.75  $< $@

%.pdf_t: %.fig %.pdf
	fig2dev -Lpdftex_t -m 0.75 -p $(basename $<).pdf $< $@

%.code: %.lisp 
	./codify $<

%.eps: %
	echo $<
	sh curves.sh $<
	gnuplot $<.gp

%.tex: %
	echo $%
	echo $<	
	sh curves.sh $<
	gnuplot $<.gp

$(NAME).pdf: $(TEXFILES) $(PDF) $(PDF_T) $(VERBATIM) $(CODEFILES) $(BIBFILES) $(CURVE)
	pdflatex $<
	bibtex $(NAME)
	pdflatex $<
	pdflatex $<

$(CURVE): curves.sh

.PHONY: clean spotless view

view: $(NAME).pdf
	xpdf $<

clean:
	rm -f *.aux *.log *~ *.pdf *.pdf *.pdf_t *.bbl *.blg

spotless: clean
	rm -f *.ps *.dvi *.pdf *.pdf_t *.toc *.idx *.ilg *.ind *.fig.bak
	rm -f *.out *.cb *.cb2

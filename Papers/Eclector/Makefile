NAME=eclector

TEXFILES=$(NAME).tex $(shell ./tex-dependencies $(NAME).tex)
BIBFILES=$(NAME).bib
PDF_T=$(shell ./strip-dependence inputfig $(TEXFILES))
VERBATIM=$(shell ./strip-dependence verbatimtabinput $(TEXFILES))
CODEFILES=$(shell ./strip-dependence inputcode $(TEXFILES))
EPSFILES=$(shell ./strip-dependence inputeps $(TEXFILES))
PDF=$(subst .pdf_t,.pdf,$(PDF_T))

all : $(NAME).pdf

%.pdf: %.fig
	fig2dev -Lpdftex -m 0.35  $< $@

%.pdf_t: %.fig %.pdf
	fig2dev -Lpdftex_t -m 0.35 -p $(basename $<).pdf $< $@

%.code : %.lisp 
	./codify $<

$(NAME).pdf: $(TEXFILES) $(PDF) $(PDF_T) $(VERBATIM) $(CODEFILES) $(BIBFILES) $(EPSFILES)
	pdflatex $<
	bibtex $(NAME)
	pdflatex $<
	pdflatex $<

%.eps : %.gp
	gnuplot $<

%.gp : %.res curves.sh
	bash curves.sh $<

.PHONY: view clean spotless

# view: $(NAME).pdf
# 	xpdf $<

clean:
	rm -f *.aux *.log *~ *.pdf *.pdf *.pdf_t *.bbl *.blg

spotless: clean
	rm -f *.ps *.dvi *.pdf *.pdf_t *.toc *.idx *.ilg *.ind *.fig.bak
	rm -f *.out *.cb *.cb2 $(EPSFILES)

NAME=slides

TEXFILES=$(NAME).tex $(shell ./tex-dependencies $(NAME).tex)
PDF_T=$(shell ./strip-dependence inputfig $(TEXFILES))
PDF=$(subst .pdf_t,.pdf,$(PDF_T))
VERBATIM=$(shell ./strip-dependence verbatimtabinput $(TEXFILES))

all : $(NAME).pdf

%.pstex: %.fig
	fig2dev -Lpstex -m 1.5 $< $@

%.pdf: %.fig
	fig2dev -Lpdftex -m 0.75  $< $@

%.pdf_t: %.fig %.pdf
	fig2dev -Lpdftex_t -m 0.75 -p $(basename $<).pdf $< $@

$(NAME).pdf: $(TEXFILES) $(PDF) $(PDF_T) $(VERBATIM)
	pdflatex $<

clean:
	rm -f *.aux *.log *~ *.pdf 

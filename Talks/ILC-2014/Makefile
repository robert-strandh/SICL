NAME=slides

TEXFILES=$(NAME).tex $(shell ./tex-dependencies $(NAME).tex)
PSTEX_T=$(shell ./strip-dependence inputfig $(TEXFILES))
VERBATIM=$(shell ./strip-dependence verbatimtabinput $(TEXFILES))
PSTEX=$(subst .pstex_t,.pstex,$(PSTEX_T))

all : $(NAME).pdf

%.pstex: %.fig
	fig2dev -Lpstex -m 1.5 $< $@

%.pstex_t: %.fig %.pstex
	fig2dev -Lpstex_t -m 1.5 -p$(basename $<).pstex $< $@

$(NAME).pdf: $(TEXFILES) $(PSTEX_T) $(VERBATIM)
	pdflatex $<

clean:
	rm -f *.aux *.log *~ *.pdf 

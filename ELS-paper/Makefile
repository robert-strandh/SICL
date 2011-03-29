.SUFFIXES: fig .ps .dvi .tex .pdf

NAME=slides

TEXFILES=$(NAME).tex $(shell ./tex-dependencies $(NAME).tex)
PSTEX_T=$(shell ./strip-dependence inputfig $(TEXFILES))
VERBATIM=$(shell ./strip-dependence verbatimtabinput $(TEXFILES))
PSTEX=$(subst .pstex_t,.pstex,$(PSTEX_T))

all : $(NAME).ps

%.pstex: %.fig
	fig2dev -Lpstex -m 1.2 $< $@

%.pstex_t: %.fig %.pstex
	fig2dev -Lpstex_t -m 1.2 -p$(basename $<).pstex $< $@

.dvi.ps :
	dvips $< -o

.ps.pdf :
	ps2pdf $<

.tex.dvi:
	latex $<

$(NAME).dvi: $(TEXFILES) $(PSTEX_T) $(VERBATIM)
	latex $<

$(NAME).ps: $(NAME).dvi $(PSTEX)
	dvips -t landscape $< -o

view: $(NAME).ps
	gv -antialias -seascape -scale 2 $<

install-slides: slides.pdf
	cp slides.pdf ../Web/slides.pdf
clean:
	rm -f *.aux *.log *.bak *~

spotless:
	make clean
	rm -f $(NAME).ps $(NAME).pdf $(NAME).dvi *.pstex *.pstex_t *.toc *.idx *.ilg *.ind

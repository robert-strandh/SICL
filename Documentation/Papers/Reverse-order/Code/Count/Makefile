.SUFFIXES:
the-test.lisp: 0-count.lisp 1-count.lisp 7-count.lisp general.lisp
	echo $^
	cat $^ > $@

.phony: clean spotless

clean:
	-rm -f \#*\# *~ *.fasl
	-rm -f the-test.lisp

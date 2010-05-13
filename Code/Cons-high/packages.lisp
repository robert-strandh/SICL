;;; The symbols that are shadowed from the COMMON-LISP package
;;; are also symbols that we want to export.  To avoid repeating 
;;; that list of symbols, we use the reader macros #= and ##. 
;;; It is interesting to note that we would normally write 
;;; (:shadow <string1> <string2> ...), but in order to put a 
;;; reader label on the list (<string1> <string2> ...) we need to
;;; express that as (:shadow . (<string1> <string2> ...)) instead. 
(defpackage #:sicl-cons-high
    (:use #:cl)
  (:shadow . #1=(#:caar #:cadr #:cdar #:cddr
                 #:caaar #:caadr #:cadar #:caddr
                 #:cdaar #:cdadr #:cddar #:cdddr
                 #:caaaar #:caaadr #:caadar #:caaddr
                 #:cadaar #:cadadr #:caddar #:cadddr
                 #:cdaaar #:cdaadr #:cdadar #:cdaddr
                 #:cddaar #:cddadr #:cdddar #:cddddr
                 #:first #:second #:third #:fourth #:fifth
                 #:sixth #:seventh #:eighth #:ninth #:tenth
                 #:rest
                 #:list #:list* #:last
                 #:copy-list #:list-length #:make-list
                 #:nth #:nthcdr #:copy-tree #:tree-equal
                 #:endp
		 #:mapcar #:mapc #:maplist #:mapl #:mapcan #:mapcon
		 #:append #:nconc
                 #:revappend #:nreconc
                 #:last #:butlast #:nbutlast))
  (:export . #1#))

;;; In the test package, we want to import all symbols of the
;;; COMMON-LISP package, except the ones that were shadowed in
;;; the package that we are testing.  We do that by programatically
;;; getting the list of those symbols using the function 
;;; package-shadowing symbols.  But since defpackage doesn't evaluate
;;; the symbols in the :shadowing-import-from list, we need to 
;;; have the reader produce that list, hence the use of the #. reader
;;; macro and again of the consing dot followed by a list. 
(defpackage #:sicl-cons-high-test
    (:shadowing-import-from #:sicl-cons-high .
                            #.(package-shadowing-symbols '#:sicl-cons-high))
    (:use #:sicl-cons-high #:cl #:lisp-unit))


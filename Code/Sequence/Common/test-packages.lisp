;;; The symbols that are shadowed from the COMMON-LISP package
;;; are also symbols that we want to export.  To avoid repeating
;;; that list of symbols, we use the reader macros #= and ##.
;;; It is interesting to note that we would normally write
;;; (:shadow <string1> <string2> ...), but in order to put a
;;; reader label on the list (<string1> <string2> ...) we need to
;;; express that as (:shadow . (<string1> <string2> ...)) instead.
(defpackage #:sicl-sequence
    (:use #:common-lisp)
  (:shadow . #1=(#:find #:find-if #:find-if-not
                 #:position #:position-if #:position-if-not
                 #:length #:subseq
                 #:reduce
                 #:fill
                 #:remove #:remove-if #:remove-if-not
                 #:delete #:delete-if #:delete-if-not
                 #:count #:count-if #:count-if-not
                 #:substitute #:substitute-if #:substitute-if-not
                 #:nsubstitute #:nsubstitute-if #:nsubstitute-if-not
                 #:copy-seq #:elt
                 #:merge #:sort
                 #:mismatch
                 #:reverse #:nreverse
                 #:map #:map-into))
  (:export . #1#))

;;; In the test package, we want to import all symbols of the
;;; COMMON-LISP package, except the ones that were shadowed in
;;; the package that we are testing.  We do that by programatically
;;; getting the list of those symbols using the function
;;; package-shadowing symbols.  But since defpackage doesn't evaluate
;;; the symbols in the :shadowing-import-from list, we need to
;;; have the reader produce that list, hence the use of the #. reader
;;; macro and again of the consing dot followed by a list.
(defpackage #:sicl-sequence-test
    (:shadowing-import-from #:sicl-sequence .
                            #.(package-shadowing-symbols '#:sicl-sequence))
    (:use #:sicl-sequence #:cl #:lisp-unit))

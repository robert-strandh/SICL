(cl:in-package #:common-lisp-user)

;;; In the test package, we want to import all symbols of the
;;; COMMON-LISP package, except the ones that were shadowed in
;;; the package that we are testing.  We do that by programatically
;;; getting the list of those symbols using the function 
;;; package-shadowing symbols.  But since defpackage doesn't evaluate
;;; the symbols in the :shadowing-import-from list, we need to 
;;; have the reader produce that list, hence the use of the #. reader
;;; macro and again of the consing dot followed by a list. 
(defpackage #:sicl-conditionals-test
  (:shadowing-import-from #:sicl-conditionals .
                          #.(package-shadowing-symbols '#:sicl-conditionals))
  (:use #:sicl-conditionals #:cl #:lisp-unit))

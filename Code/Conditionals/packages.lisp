;;;; Copyright (c) 2008, 2009, 2010, 2011, 2012
;;;;
;;;;     Robert Strandh (strandh@gmail.com)
;;;;
;;;; all rights reserved. 
;;;;
;;;; Permission is hereby granted to use this software for any 
;;;; purpose, including using, modifying, and redistributing it.
;;;;
;;;; The software is provided "as-is" with no warranty.  The user of
;;;; this software assumes any responsibility of the consequences. 

;;;; This file is part of the conditionals module of the SICL project.
;;;; See the file SICL.text for a description of the project. 
;;;; See the file conditionals.text for a description of the module.

;;; The symbols that are shadowed from the COMMON-LISP package
;;; are also symbols that we want to export.  To avoid repeating 
;;; that list of symbols, we use the reader macros #= and ##. 
;;; It is interesting to note that we would normally write 
;;; (:shadow <string1> <string2> ...), but in order to put a 
;;; reader label on the list (<string1> <string2> ...) we need to
;;; express that as (:shadow . (<string1> <string2> ...)) instead. 

(defpackage #:sicl-conditionals
  (:use #:common-lisp #:sicl-code-utilities)
  (:shadow . #1=(#:or #:and #:when #:unless #:cond
		      #:case #:ccase #:ecase
		      #:typecase #:ctypecase #:etypecase))
  (:export . #1#))	

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


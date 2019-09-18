;;;; Copyright (c) 2008 - 2013
;;;;
;;;;     Robert Strandh (robert.strandh@gmail.com)
;;;;
;;;; all rights reserved. 
;;;;
;;;; Permission is hereby granted to use this software for any 
;;;; purpose, including using, modifying, and redistributing it.
;;;;
;;;; The software is provided "as-is" with no warranty.  The user of
;;;; this software assumes any responsibility of the consequences. 

;;;; This file is part of the cons-high module of the SICL project.
;;;; See the file SICL.text for a description of the project. 
;;;; See the file cons-high.text for a description of the module.

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
                 #:last #:butlast #:nbutlast
                 #:subst #:subst-if #:subst-if-not
                 #:nsubst #:nsubst-if #:nsubst-if-not
                 #:assoc #:assoc-if #:assoc-if-not
                 #:rassoc #:rassoc-if #:rassoc-if-not
                 #:member #:member-if #:member-if-not
                 #:sublis #:nsublis
		 #:acons #:pairlis #:copy-alist
		 #:tailp #:ldiff
		 #:union #:nunion #:intersection #:nintersection
		 #:set-difference #:nset-difference
		 #:set-exclusive-or #:nset-exclusive-or
		 #:adjoin #:subsetp
		 #:getf #:get-properties
		 #:push #:pop
		 #:remf #:pushnew))
  (:export #:reverse-list . #1#))

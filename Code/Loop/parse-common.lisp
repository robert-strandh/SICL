;;;; Copyright (c) 2014
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

(cl:in-package #:sicl-loop)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Given a symbol S (no matter what package), return a singleton
;;; parser Q that recognizes symbols with the same name as S.  If Q
;;; succeeds, it returns S.

(defun keyword-parser (symbol)
  (singleton (constantly symbol)
	     (lambda (token) (symbol-equal symbol token))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parser for COMPOUND-FORM+, i.e. a non-empty sequence of compound
;;; forms.

(define-parser compound+
  (repeat+ (lambda (&rest forms)
	     (cons 'progn forms))
	   (singleton #'identity #'consp)))
		      


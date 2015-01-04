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
;;; Parser

(define-parser simple-type-spec-parser
  (lambda (tokens)
    (if (and (not (null tokens))
	     (member (car tokens) '(fixnum float t nil)))
	(values t
		(car tokens)
		(cdr tokens))
	(values nil nil tokens))))

(define-parser destructured-type-spec-parser
  (consecutive (lambda (of-type tree)
		 (declare (ignore of-type))
		 tree)
	       (keyword-parser 'of-type)
	       'anything-parser))

(define-parser type-spec-parser
  (alternative 'simple-type-spec-parser 'destructured-type-spec-parser))

(define-parser optional-type-spec-parser
  (optional t 'type-spec-parser))

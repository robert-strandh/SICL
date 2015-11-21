;;;; Copyright (c) 2015
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

;;; Recall that in the dictionary entry for LOOP, the HyperSpec says:
;;;
;;;   main-clause ::= unconditional | 
;;;                   accumulation |
;;;                   conditional |
;;;                   termination-test |
;;;                   initial-final
;;;
;;; Though here, we exclude initial-final so that we have:
;;;
;;;   main-clause ::= unconditional | 
;;;                   accumulation |
;;;                   conditional |
;;;                   termination-test
;;;
;;; Furthermore, the HyperSpec defines selectable-clause like this:
;;;
;;;   selectable-clause ::= unconditional | accumulation | conditional 
;;;
;;; so we can say:
;;;
;;;    main-clause ::= selectable-clause | termination-test

(defclass selectable-clause (main-clause) ())

(defmethod bound-variables ((clause selectable-clause))
  '())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parsers.

(define-parser selectable-clause-parser
  (alternative 'do-clause-parser
	       'return-clause-parser
	       'collect-clause-parser
	       'append-clause-parser
	       'nconc-clause-parser
	       'count-clause-parser
	       'sum-clause-parser
	       'maximize-clause-parser
	       'minimize-clause-parser
	       'conditional-clause-parser))

(define-parser and-selectable-clause-parser
  (consecutive (lambda (and selectable-clause)
		 (declare (ignore and))
		 selectable-clause)
	       (keyword-parser 'and)
	       'selectable-clause-parser))

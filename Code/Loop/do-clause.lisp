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

;;;; Clause DO-CLAUSE.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class DO-CLAUSE.
;;;
;;; An DO clause does not exist as a separate grammar item in
;;; the HyperSpec, but we define it here anyway.  The syntax is:
;;;
;;;    do-clause ::= do compound-form+

(defclass do-clause (unconditional-clause main-clause-mixin)
  ((%body :initarg :body :reader body)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parser

(define-parser do-clause-parser
  (consecutive (lambda (do compound+)
		 (declare (ignore do))
		 (make-instance 'do-clause
		   :body compound+))
	       (alternative (keyword-parser 'do)
			    (keyword-parser 'doing))
	       'compound+))

(add-clause-parser 'do-clause-parser)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the body-form.

(defmethod body-form ((clause do-clause) end-tag)
  (declare (ignore end-tag))
  (body clause))

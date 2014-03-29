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

;;;; Clause RETURN-CLAUSE.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class RETURN-CLAUSE.
;;;
;;; An RETURN clause does not exist as a separate grammar item in
;;; the HyperSpec, but we define it here anyway.  The syntax is:
;;;
;;;    return-clause ::= return {form | it}

(defclass return-clause
    (clause variable-clause-mixin main-clause-mixin)
  ())

(defclass return-it-clause (return-clause)
  ())

(defclass return-form-clause (return-clause)
  ((%form :initarg :form :reader form)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parser

(define-parser return-it-clause-parser
  (consecutive (lambda (return it)
		 (declare (ignore return it))
		 (make-instance 'return-it-clause))
	       (keyword-parser 'return)
	       (keyword-parser 'it)))

(define-parser return-form-clause-parser
  (consecutive (lambda (return form)
		 (declare (ignore return))
		 (make-instance 'return-clause
		   :form form))
	       (keyword-parser 'return)
	       (singleton #'identity (constantly t))))

(define-parser return-clause-parser
  (alternative 'return-it-clause-parser
	       'return-form-clause-parser))

(add-clause-parser 'return-clause-parser)


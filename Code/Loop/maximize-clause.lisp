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

(defclass maximize-it-clause
    (accumulate-it-clause numeric-accumulation-mixin)
  ())

(defclass maximize-form-clause
    (accumulate-form-clause numeric-accumulation-mixin)
  ())

(defclass maximize-it-into-clause
    (accumulate-it-into-clause numeric-accumulation-mixin)
  ())

(defclass maximize-form-into-clause
    (accumulate-form-into-clause numeric-accumulation-mixin)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parsers.

(define-parser maximize-it-into-clause-parser
  (consecutive (lambda (maximize it into var)
		 (declare (ignore maximize it into))
		 (make-instance 'maximize-it-into-clause
		   :into-var var))
	       (alternative (keyword-parser 'maximize)
			    (keyword-parser 'maximizing))
	       (keyword-parser 'it)
	       (keyword-parser 'into)
	       (singleton #'identity
			  (lambda (x)
			    (and (symbolp x) (not (constantp x)))))))

(define-parser maximize-it-clause-parser
  (consecutive (lambda (maximize it)
		 (declare (ignore maximize it))
		 (make-instance 'maximize-it-clause))
	       (alternative (keyword-parser 'maximize)
			    (keyword-parser 'maximizing))
	       (keyword-parser 'it)))

(define-parser maximize-form-into-clause-parser
  (consecutive (lambda (maximize form into var)
		 (declare (ignore maximize into))
		 (make-instance 'maximize-form-into-clause
		   :form form
		   :into-var var))
	       (alternative (keyword-parser 'maximize)
			    (keyword-parser 'maximizing))
	       (singleton #'identity (constantly t))
	       (keyword-parser 'into)
	       (singleton #'identity
			  (lambda (x)
			    (and (symbolp x) (not (constantp x)))))))

(define-parser maximize-form-clause-parser
  (consecutive (lambda (maximize form)
		 (declare (ignore maximize))
		 (make-instance 'maximize-form-clause
		   :form form))
	       (alternative (keyword-parser 'maximize)
			    (keyword-parser 'maximizing))
	       (singleton #'identity (constantly t))))

(define-parser maximize-clause-parser
  (alternative 'maximize-it-into-clause-parser
	       'maximize-it-clause-parser
	       'maximize-form-into-clause-parser
	       'maximize-form-clause-parser

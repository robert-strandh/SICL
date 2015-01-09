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

(defclass nconc-clause (list-accumulation-clause) ())

(defclass nconc-it-clause
    (nconc-clause accumulate-it-clause)
  ())

(defmethod accumulation-variables ((clause nconc-it-clause))
  '((nil list list)))

(defclass nconc-form-clause
    (nconc-clause accumulate-form-clause)
  ())

(defmethod accumulation-variables ((clause nconc-it-clause))
  '((nil list list)))

(defclass nconc-it-into-clause
    (nconc-clause accumulate-it-into-clause)
  ())

(defmethod accumulation-variables ((clause nconc-it-into-clause))
  `((,(into-var clause) list list)))

(defclass nconc-form-into-clause
    (nconc-clause accumulate-form-into-clause)
  ())

(defmethod accumulation-variables ((clause nconc-it-into-clause))
  `((,(into-var clause) list list)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parsers.

(define-parser nconc-it-into-clause-parser
  (consecutive (lambda (nconc it into var)
		 (declare (ignore nconc it into))
		 (make-instance 'nconc-it-into-clause
		   :into-var var))
	       (alternative (keyword-parser 'nconc)
			    (keyword-parser 'nconcing))
	       (keyword-parser 'it)
	       (keyword-parser 'into)
	       (singleton #'identity
			  (lambda (x)
			    (and (symbolp x) (not (constantp x)))))))

(define-parser nconc-it-clause-parser
  (consecutive (lambda (nconc it)
		 (declare (ignore nconc it))
		 (make-instance 'nconc-it-clause))
	       (alternative (keyword-parser 'nconc)
			    (keyword-parser 'nconcing))
	       (keyword-parser 'it)))

(define-parser nconc-form-into-clause-parser
  (consecutive (lambda (nconc form into var)
		 (declare (ignore nconc into))
		 (make-instance 'nconc-form-into-clause
		   :form form
		   :into-var var))
	       (alternative (keyword-parser 'nconc)
			    (keyword-parser 'nconcing))
	       'anything-parser
	       (keyword-parser 'into)
	       (singleton #'identity
			  (lambda (x)
			    (and (symbolp x) (not (constantp x)))))))

(define-parser nconc-form-clause-parser
  (consecutive (lambda (nconc form)
		 (declare (ignore nconc))
		 (make-instance 'nconc-form-clause
		   :form form))
	       (alternative (keyword-parser 'nconc)
			    (keyword-parser 'nconcing))
	       'anything-parser))

(define-parser nconc-clause-parser
  (alternative 'nconc-it-into-clause-parser
	       'nconc-it-clause-parser
	       'nconc-form-into-clause-parser
	       'nconc-form-clause-parser))

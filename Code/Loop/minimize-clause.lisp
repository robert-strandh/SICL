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

(defclass minimize-clause (max/min-accumulation-clause) ())

(defclass minimize-it-clause (minimize-clause it-mixin)
  ())

(defclass minimize-form-clause (minimize-clause form-mixin)
  ())

(defclass minimize-it-into-clause (minimize-clause it-mixin into-mixin)
  ())

(defclass minimize-form-into-clause (minimize-clause form-mixin into-mixin)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parsers.

(define-parser minimize-it-into-clause-parser
  (consecutive (lambda (minimize it into var)
		 (declare (ignore minimize it into))
		 (make-instance 'minimize-it-into-clause
		   :into-var var))
	       (alternative (keyword-parser 'minimize)
			    (keyword-parser 'minimizing))
	       (keyword-parser 'it)
	       (keyword-parser 'into)
	       (singleton #'identity
			  (lambda (x)
			    (and (symbolp x) (not (constantp x)))))))

(define-parser minimize-it-clause-parser
  (consecutive (lambda (minimize it)
		 (declare (ignore minimize it))
		 (make-instance 'minimize-it-clause))
	       (alternative (keyword-parser 'minimize)
			    (keyword-parser 'minimizing))
	       (keyword-parser 'it)))

(define-parser minimize-form-into-clause-parser
  (consecutive (lambda (minimize form into var)
		 (declare (ignore minimize into))
		 (make-instance 'minimize-form-into-clause
		   :form form
		   :into-var var))
	       (alternative (keyword-parser 'minimize)
			    (keyword-parser 'minimizing))
	       'anything-parser
	       (keyword-parser 'into)
	       (singleton #'identity
			  (lambda (x)
			    (and (symbolp x) (not (constantp x)))))))

(define-parser minimize-form-clause-parser
  (consecutive (lambda (minimize form)
		 (declare (ignore minimize))
		 (make-instance 'minimize-form-clause
		   :form form))
	       (alternative (keyword-parser 'minimize)
			    (keyword-parser 'minimizing))
	       'anything-parser))

(define-parser minimize-clause-parser
  (alternative 'minimize-it-into-clause-parser
	       'minimize-it-clause-parser
	       'minimize-form-into-clause-parser
	       'minimize-form-clause-parser))

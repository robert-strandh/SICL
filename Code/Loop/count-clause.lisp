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

(defclass count-clause (accumulation-clause)
  ((%type-spec :initform T :initarg :type-spec :reader type-spec)))

(defclass count-it-clause
    (count-clause accumulate-it-clause numeric-accumulation-mixin)
  ())

(defmethod accumulation-variables ((clause append-it-clause))
  '((nil list list)))

(defclass count-form-clause
    (count-clause accumulate-form-clause numeric-accumulation-mixin)
  ())

(defclass count-it-into-clause
    (count-clause accumulate-it-into-clause numeric-accumulation-mixin)
  ())

(defclass count-form-into-clause
    (count-clause accumulate-form-into-clause numeric-accumulation-mixin)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parsers.

(define-parser count-it-into-clause-parser
  (consecutive (lambda (count it into var)
		 (declare (ignore count it into))
		 (make-instance 'count-it-into-clause
		   :into-var var))
	       (alternative (keyword-parser 'count)
			    (keyword-parser 'counting))
	       (keyword-parser 'it)
	       (keyword-parser 'into)
	       (singleton #'identity
			  (lambda (x)
			    (and (symbolp x) (not (constantp x)))))))

(define-parser count-it-clause-parser
  (consecutive (lambda (count it)
		 (declare (ignore count it))
		 (make-instance 'count-it-clause))
	       (alternative (keyword-parser 'count)
			    (keyword-parser 'counting))
	       (keyword-parser 'it)))

(define-parser count-form-into-clause-parser
  (consecutive (lambda (count form into var)
		 (declare (ignore count into))
		 (make-instance 'count-form-into-clause
		   :form form
		   :into-var var))
	       (alternative (keyword-parser 'count)
			    (keyword-parser 'counting))
	       'anything-parser
	       (keyword-parser 'into)
	       (singleton #'identity
			  (lambda (x)
			    (and (symbolp x) (not (constantp x)))))))

(define-parser count-form-clause-parser
  (consecutive (lambda (count form)
		 (declare (ignore count))
		 (make-instance 'count-form-clause
		   :form form))
	       (alternative (keyword-parser 'count)
			    (keyword-parser 'counting))
	       'anything-parser))

(define-parser count-clause-parser
  (alternative 'count-it-into-clause-parser
	       'count-it-clause-parser
	       'count-form-into-clause-parser
	       'count-form-clause-parser))

(add-clause-parser 'count-clause-parser)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the BODY-FORM.

(defmethod body-form ((clause count-form-clause) end-tag)
  (declare (ignore end-tag))
  `(when ,(form clause)
     (setq ,*accumulation-variable*
	   (1+ ,*accumulation-variable*))))

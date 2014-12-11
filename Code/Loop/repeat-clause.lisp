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

(defclass repeat-clause (clause var-and-type-spec-mixin)
  ((%form :initarg :form :reader form)
   (%form-value-var :initform (gensym) :reader form-value-var))
  (:default-initargs :var-spec (gensym) :type-spec 'real))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parsers.

(define-parser repeat-clause-parser
  (consecutive (lambda (repeat form)
		 (declare (ignore repeat))
		 (make-instance 'repeat-clause :form form))
	       (keyword-parser 'repeat)
	       (singleton #'identity (constantly t))))

(add-clause-parser 'repeat-clause-parser)

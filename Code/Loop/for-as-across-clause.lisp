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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Clause FOR-AS-ACROSS

(defclass for-as-across (for-as-subclause var-and-type-spec-mixin)
  ((%vector-form :initarg :vector-form :reader vector-form)
   (%form-var :initform (gensym) :reader form-var)
   (%length-var :initform (gensym) :reader length-var)
   (%index-var :initform (gensym) :reader index-var)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parser

(define-parser for-as-across-parser
  (consecutive (lambda (var type-spec across vector-form)
		 (declare (ignore across))
		 (make-instance 'for-as-across
		   :var-spec var
		   :type-spec type-spec
		   :vector-form vector))
	       'd-var-spec-parser
	       'optional-type-spec-parser
	       (keyword-parser 'across)
	       (singleton #'identity (constantly t))))

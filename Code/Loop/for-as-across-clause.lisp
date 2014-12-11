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
  (;; This slot contains a copy of the tree contained in the VAR-SPEC
   ;; slot except that the non-NIL leaves have been replaced by
   ;; GENSYMs.
   (%temp-vars :initarg :temp-vars :reader temp-vars)
   ;; This slot contains a list of pairs.  Each pair is a CONS cell
   ;; where the CAR is a variable in VAR-SPEC and the CDR is the
   ;; corresponding variable in TEMP-VARS.
   (%dictionary :initarg :dictionary :reader dictionary)
   (%vector-form :initarg :vector-form :reader vector-form)
   (%form-var :initform (gensym) :reader form-var)
   (%length-var :initform (gensym) :reader length-var)
   (%index-var :initform (gensym) :reader index-var)))

(defmethod initialize-instance :after
    ((clause for-as-across) &key &allow-other-keys)
  (multiple-value-bind (temp-vars dictionary)
      (fresh-variables (var-spec clause))
    (reinitialize-instance clause
			   :temp-vars temp-vars
			   :dictionary dictionary)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parser

(define-parser for-as-across-parser
  (consecutive (lambda (var type-spec across vector-form)
		 (declare (ignore across))
		 (make-instance 'for-as-across
		   :var-spec var
		   :type-spec type-spec
		   :vector-form vector-form))
	       'd-var-spec-parser
	       'optional-type-spec-parser
	       (keyword-parser 'across)
	       (singleton #'identity (constantly t))))

(add-for-as-subclause-parser 'for-as-across-parser)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute bindings.

(defmethod initial-bindings ((clause for-as-across))
  `((,(form-var clause) ,(vector-form clause))
    (,(length-var clause) (length ,(form-var clause)))
    (,(index-var 0))
    ,@(loop for (real-var) in (dictionary clause)
	    collect `(,real-var nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute declarations.

(defmethod declarations ((clause for-as-across))
  (extract-variables (var-spec clause) (type-spec clause)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute prologue and body

(defun for-as-across-prologue-or-body (clause)
  `(progn (when (>= ,(index-var clause) ,(length-var clause))
	    (go end))
	  (let* ,(destructure-variables (var-spec clause)
					`(aref ,(form-var clause)
					       ,(index-var clause)))
	    (setf ,@(loop for (real-var . temp-var) in (dictionary clause)
			  append `(,real-var ,temp-var))))))

(defmethod prologue ((clause for-as-across))
  (for-as-across-prologue-or-body clause))

(defmethod body ((clause for-as-across))
  (for-as-across-prologue-or-body clause))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute termination.

(defmethod termination ((clause for-as-across))
  `(when (>= ,(index-var clause) ,(length-var clause))
     (go end)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute step.

(defmethod step ((clause for-as-across))
  `(incf ,(index-var clause)))

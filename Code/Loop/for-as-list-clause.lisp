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

(in-package #:sicl-loop)

(defclass for-as-list (for-as-subclause)
  (;; This slot contains a copy of the tree contained in the VAR-SPEC
   ;; slot except that the non-NIL leaves have been replaced by
   ;; GENSYMs.
   (%temp-vars :initarg :temp-vars :reader temp-vars)
   ;; This slot contains a list of pairs.  Each pair is a CONS cell
   ;; where the CAR is a variable in VAR-SPEC and the CDR is the
   ;; corresponding variable in TEMP-VARS.
   (%dictionary :initarg :dictionary :reader dictionary)
   (%list-form :initarg :list-form :reader list-form)
   (%list-var :initform (gensym) :reader list-var)
   (%by-form :initarg :by-form :reader by-form)
   (%by-var :initform (gensym) :reader by-var)
   (%rest-var :initform (rest-var) :reader rest-var)))

(defmethod initialize-instance :after
    ((clause for-as-list) &key &allow-other-keys)
  (multiple-value-bind (temp-vars dictionary)
      (fresh-variables (var-spec clause))
    (reinitialize-instance clause
			   :temp-vars temp-vars
			   :dictionary dictionary)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Clause FOR-AS-IN-LIST.

(defclass for-as-in-list (for-as-list) ())

(define-parser for-as-in-list-parser-1
  (consecutive (lambda (var type-spec in list-form by-form)
		 (declare (ignore in))
		 (make-instance 'for-as-in-list
		   :var-spec var
		   :type-spec type-spec
		   :list-form list-form
		   :by-form by-form))
	       (singleton #'identity (constantly t))
	       'optional-type-spec-parser
	       (keyword-parser 'in)
	       (singleton #'identity (constantly t))
	       'by-parser))

(define-parser for-as-in-list-parser-2
  (consecutive (lambda (var type-spec in list-form)
		 (declare (ignore in))
		 (make-instance 'for-as-in-list
		   :var-spec var
		   :type-spec type-spec
		   :list-form list-form
		   :by-form #'cdr))
	       'd-var-spec-parser
	       'type-spec-parser
	       (keyword-parser 'in)
	       (singleton #'identity (constantly t))))

;;; Define a parser that tries the longer form first
(define-parser for-as-in-list-parser
  (alternative 'for-as-in-list-parser-1
	       'for-as-in-list-parser-2))

(add-for-as-subclause-parser 'for-as-in-list-parser)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Clause FOR-AS-ON-LIST.

(defclass for-as-on-list (for-as-list) ())

(define-parser for-as-on-list-parser-1
  (consecutive (lambda (var type-spec on list-form by-form)
		 (declare (ignore on))
		 (make-instance 'for-as-on-list
		   :var-spec var
		   :type-spec type-spec
		   :list-form list-form
		   :by-form by-form))
	       (singleton #'identity (constantly t))
	       'optional-type-spec-parser
	       (keyword-parser 'on)
	       (singleton #'identity (constantly t))
	       'by-parser))

(define-parser for-as-on-list-parser-2
  (consecutive (lambda (var type-spec on list-form)
		 (declare (ignore on))
		 (make-instance 'for-as-on-list
		   :var-spec var
		   :type-spec type-spec
		   :list-form list-form
		   :by-form #'cdr))
	       'd-var-spec-parser
	       'type-spec-parser
	       (keyword-parser 'on)
	       (singleton #'identity (constantly t))))

;;; Define a parser that tries the longer form first
(define-parser for-as-on-list-parser
  (alternative 'for-as-on-list-parser-1
	       'for-as-on-list-parser-2))

(add-for-as-subclause-parser 'for-as-on-list-parser)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the bindings.

(defmethod initial-bindings ((clause for-as-list))
  `((,(list-var clause) ,(list-form clause))
    (,(rest-var clause) ,(list-var clause))
    ,@(if (member (by-form clause) (list #'cdr #'cddr))
	  '()
	  `(,(by-var clause) ,(by-form clause)))))

(defmethod final-bindings ((clause for-as-list))
  `((,(var-spec clause) nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the declarations.

(defmethod declarations ((clause for-as-list))
  (loop with d-var-spec = (var-spec clause)
	with d-type-spec = (type-spec clause)
	for (variable type) in (extract-variables d-var-spec d-type-spec)
	collect `(type (or null ,type) ,variable)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the termination-forms.

(defmethod termination-forms ((clause for-as-in-list) end-tag)
  `(when (endp ,(rest-var clause))
     (go ,end-tag)))

(defmethod termination-forms ((clause for-as-on-list) end-tag)
  `(when (atom ,(rest-var clause))
     (go ,end-tag)))

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
;;; Clause FOR-AS-PACKAGE

(defclass for-as-package (for-as-subclause)
  ((%package-form :initarg :package-form :reader package-form)
   (%package-var :initform (gensym) :reader package-var)
   (%temp-entry-p-var :initform (gensym) :reader temp-entry-p-var)
   (%temp-symbol-var :initform (gensym) :reader temp-symbol-var)
   (%iterator-var :initform (gensym) :reader iterator-var)
   (%iterator-keywords :initarg :itetator-keywords :reader iterator-keywords)))

(defmethod bound-variables ((subclause for-as-package))
  (mapcar #'car
	  (extract-variables (var-spec subclause) nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parsers

(define-parser symbol-parser
  (alternative (keyword-parser 'symbol)
	       (keyword-parser 'symbols)))

(define-parser present-symbol-parser
  (alternative (keyword-parser 'present-symbol)
	       (keyword-parser 'present-symbols)))

(define-parser external-symbol-parser
  (alternative (keyword-parser 'external-symbol)
	       (keyword-parser 'external-symbols)))

(define-parser package-symbol-parser
  (consecutive (lambda (var-spec
			type-spec
			being
			each
			symbol
			of
			package-form)
		 (declare (ignore being each symbol of))
		 (make-instance 'for-as-package-symbols
		   :var-spec var-spec
		   :type-spec type-spec
		   :package-form package-form))
	       'anything-parser
	       'optional-type-spec-parser
	       'being-parser
	       'each-the-parser
	       'symbol-parser
	       'in-of-parser
	       'anything-parser))

(define-parser package-present-symbol-parser
  (consecutive (lambda (var-spec
			type-spec
			being
			each
			present-symbol
			of
			package-form)
		 (declare (ignore being each present-symbol of))
		 (make-instance 'for-as-package-present-symbols
		   :var-spec var-spec
		   :type-spec type-spec
		   :package-form package-form))
	       'anything-parser
	       'optional-type-spec-parser
	       'being-parser
	       'each-the-parser
	       'present-symbol-parser
	       'in-of-parser
	       'anything-parser))


(define-parser package-external-symbol-parser
  (consecutive (lambda (var-spec
			type-spec
			being
			each
			external-symbol
			of
			package-form)
		 (declare (ignore being each external-symbol of))
		 (make-instance 'for-as-package-external-symbols
		   :var-spec var-spec
		   :type-spec type-spec
		   :package-form package-form))
	       'anything-parser
	       'optional-type-spec-parser
	       'being-parser
	       'each-the-parser
	       'external-symbol-parser
	       'in-of-parser
	       'anything-parser))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the initial bindings.

(defmethod initial-bindings ((clause for-as-package))
  `((,(package-var clause) ,(package-form clause))))

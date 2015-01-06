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
   (%iterator-var :initform (gensym) :reader iterator-var)))

(defclass for-as-package-symbols (for-as-package) ())

(defclass for-as-package-present-symbols (for-as-package) ())

(defclass for-as-package-external-symbols (for-as-package) ())

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

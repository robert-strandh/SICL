;;;; Copyright (c) 2015
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Accumulation clauses

(defclass accumulation-clause (selectable-clause)
  ())

(defclass list-accumulation-clause (accumulation-clause) ())

(defclass numeric-accumulation-clause (accumulation-clause)
  ((%type-spec :initform T :initarg :type-spec :reader type-spec)))

(defclass count/sum-accumulation-clause (numeric-accumulation-clause) ())

(defclass max/min-accumulation-clause (numeric-accumulation-clause) ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Mixin class for INTO clause variants.

(defclass into-mixin ()
  ((%into-var :initform nil :initarg :into-var :accessor into-var)))


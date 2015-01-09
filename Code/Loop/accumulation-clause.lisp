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

(defclass accumulate-it-clause (accumulation-clause)
  ())

(defclass accumulate-form-clause (accumulation-clause)
  ((%form :initform nil :initarg :form :accessor form)))

(defclass accumulate-it-into-clause (accumulate-it-clause)
  ((%into-var :initform nil :initarg :into-var :accessor into-var)))

(defclass accumulate-form-into-clause (accumulate-form-clause)
  ((%into-var :initform nil :initarg :into-var :accessor into-var)))

(defclass list-accumulation-clause (accumulation-clause) ())

(defclass numeric-accumulation-clause (accumulation-clause)
  ((%type-spec :initform T :initarg :type-spec :reader type-spec)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Mixin class for INTO clause variants.

(defclass into-mixin ()
  ((%into-var :initform nil :initarg :into-var :accessor into-var)))


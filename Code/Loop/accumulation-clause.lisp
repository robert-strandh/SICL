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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Mixin for accumulation clauses.

(defclass accumulation-clause-mixin () ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Mixin for accumulation clauses that accumulate into a default
;;; variable.

(defclass default-accumulation-clause-mixin () ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Accumulation clauses

(defclass accumulation-clause (selectable-clause main-clause-mixin)
  ())

(defclass accumulate-it-clause (accumulation-clause)
  ())

(defclass accumulate-form-clause (accumulation-clause)
  ((%form :initform nil :initarg :form :accessor form)))

(defclass accumulate-it-into-clause (accumulate-it-clause)
  ((%into-var :initform nil :initarg :into-var :accessor into-var)))

(defclass accumulate-form-into-clause (accumulate-form-clause)
  ((%into-var :initform nil :initarg :into-var :accessor into-var)))

(defclass list-accumulation-mixin () ())

(defclass numeric-accumulation-mixin () ())

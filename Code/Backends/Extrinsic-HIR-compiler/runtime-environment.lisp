(cl:in-package #:sicl-extrinsic-hir-compiler)

(defclass dynamic-environment-entry () ())

(defclass variable-binding (dynamic-environment-entry)
  ((%symbol :initarg :symbol :reader symbol)
   (%value :initarg :value :reader value)))

(defclass unwind-protect (dynamic-environment-entry)
  ((%thunk :initarg :thunk :reader thunk)))

(defclass catch-tag (dynamic-environment-entry)
  ((%value :initarg :value :reader value)
   ;; The thunk in this slot executes a non-local GO in the host,
   ;; which unwinds the host stack. 
   (%thunk :initarg :thunk :reader thunk)))

;;; The dynamic environment is a list of instances of the class
;;; DYNAMIC-ENVIRONMENT-ENTRY.
(defparameter *dynamic-environment* '())

(cl:in-package #:sicl-extrinsic-hir-compiler)

(defclass dynamic-environment-entry () ())

(defclass variable-binding (dynamic-environment-entry)
  ((%symbol :initarg :symbol :reader symbol)
   (%value :initarg :value :reader value)))

(defclass unwind-protect (dynamic-environment-entry)
  ((%thunk :initarg :thunk :reader thunk)))

(defclass catch-tag (dynamic-environment-entry)
  ((%value :initarg :value :reader value)
   ;; The function in this slot takes a single argument which must be
   ;; a list of values to return from the CATCH.  When invoked, this
   ;; function executes the host form (RETURN-FROM <block-name> (APPLY
   ;; #'VALUES ARG)).
   (%function :initarg :function :reader function)))

;;; The dynamic environment is a list of instances of the class
;;; DYNAMIC-ENVIRONMENT-ENTRY.
(defparameter *dynamic-environment* '())

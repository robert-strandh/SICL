(cl:in-package #:sicl-hir-to-cl)

(defparameter *dynamic-environment* '())

(defclass entry () ())

(defclass block/tagbody-entry (entry)
  ((%identifier :initarg :identifier :reader identifier)))

(defclass special-variable-entry (entry)
  ((%name :initarg :name :reader name)
   (%value :initarg :value :accessor value)))

(defclass unwind-protect-entry (entry)
  ((%thunk :initarg :thunk :reader thunk)))

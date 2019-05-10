(cl:in-package #:sicl-hir-to-cl)

(defparameter *dynamic-environment* '())

(defclass entry () ())

(defclass exit-point (entry)
  ((%valid-p :initform t :accessor valid-p)))

(defclass block/tagbody-entry (exit-point)
  ((%identifier :initarg :identifier :reader identifier)))

(defclass catch-entry (exit-point)
  ((%tag :initarg :tag :reader tag)))

(defclass special-variable-entry (entry)
  ((%name :initarg :name :reader name)
   (%value :initarg :value :accessor value)))

(defclass unwind-protect-entry (entry)
  ((%thunk :initarg :thunk :reader thunk)))

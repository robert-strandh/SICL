(cl:in-package #:sicl-ast-to-hir)

(defclass context ()
  (;; This slot contains 0, 1, or :ALL.
   (%values-count
    :initarg :values-count
    :reader values-count)
   (%next-instruction
    :initarg :next-instruction
    :reader next-instruction)
   (%target-register
    :initarg :target-register
    :reader target-register)))


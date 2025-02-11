(cl:in-package #:sicl-hir)

;;; This instruction has no inputs and one output.  The output is the
;;; dynamic environment upon entry of the currently-executing
;;; function.

(defclass dynamic-environment-instruction (instruction)
  ())

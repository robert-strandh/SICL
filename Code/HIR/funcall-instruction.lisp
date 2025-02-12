(cl:in-package #:sicl-hir)

;;; This instruction has at least two inputs and it has one output.
;;; The first input is a function object to be called.  The second
;;; input is the dynamic-environment object representing the current
;;; dynamic environment.  The remaining inputs are the arguments to be
;;; transferred to the function.  The output is the value (possibly
;;; multiple values) returned from the instruction.

(defclass funcall-instruction (instruction)
  ())

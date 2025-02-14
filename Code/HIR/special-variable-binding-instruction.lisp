(cl:in-package #:sicl-hir)

;;; This instruction has two or three inputs.  The first input is a
;;; dynamic-environment object representing the current dynamic
;;; environment.  The second input is a symbol naming the variable to
;;; bind.  If present, the third input is a value to give to the
;;; variable in the new binding.  If the third input is absent, the
;;; new binding has no initial value.
;;;
;;; This instruction has a single output which is a
;;; dynamic-environment object that is like the one in the input
;;; except that a new entry representing the binding has been added.

(defclass special-variable-binding-instruction (instruction)
  ())

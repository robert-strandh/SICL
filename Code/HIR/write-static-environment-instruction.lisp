(cl:in-package #:sicl-hir)

;;; This instruction contains at least one input (which is then the
;;; first one) which is a register containing a closure object.  The
;;; remaining inputs are either registers or literals.  The effect of
;;; executing this instruction is that the static environment for the
;;; closure object is initialized to contain the objects of the
;;; remaining inputs in that order.

(defclass write-static-environment-instruction (instruction)
  ())

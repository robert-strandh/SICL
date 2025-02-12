(cl:in-package #:sicl-hir)

;;; This instruction has one input and two or three outputs.  The
;;; input is a dynamic-environment object representing the current
;;; dynamic environment.  The first output is a dynamic-environment
;;; object which is like the input but with a new exit point added.
;;; The second output is a unique identifier for this exit point.  The
;;; unique identifier is used by the UNWIND-INSTRUCTION to find the
;;; unique exit point where control is to be transferred.  The third
;;; output is present only when this instruction was created from
;;; BLOCK, and only when the BLOCK is in a context where its value is
;;; needed, and it is the value (possibly multiple values) transferred
;;; by the optional input of the UNWIND-INSTRUCTION.

(defclass exit-point-instruction (instruction)
  ())

(cl:in-package #:sicl-hir)

;;; This instruction has two or three inputs and no outputs.  The
;;; first input is a dynamic-environment object representing the
;;; current dynamic environment.  The second input is the unique
;;; identifier of an exit point as created by the
;;; EXIT-POINT-INSTRUCTION.  The third input is used only when this
;;; instruction is created from a RETURN-FROM, and only when the
;;; corresponding BLOCK is in a context where its value is needed, and
;;; is the value (perhaps multiple values) to be transferred to the
;;; exit point.  The successor of this instruction is a
;;; RECEIVE-INSTRUCTION that receives the values and transmits them to
;;; the right register if this instruction is created from a
;;; RETURN-FROM, or the instruction following a TABODY tag if this
;;; instruction was created from a GO.

(defclass unwind-instruction (instruction)
  ())

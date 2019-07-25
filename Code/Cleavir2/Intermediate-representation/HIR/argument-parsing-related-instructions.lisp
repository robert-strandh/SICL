(cl:in-package #:cleavir-ir)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction COMPUTE-ARGUMENT-COUNT-INSTRUCTION.
;;;
;;; This instruction can be used by implementation that would like to
;;; make argument parsing explicitly represented in HIR code.  It has
;;; no input, and it has a single output, which is a fixnum containing
;;; the number of arguments passed to a function.
;;;
;;; If used, this instruction should be the immediate successor of the
;;; ENTER-INSTRUCTION in a function.

(defclass compute-argument-count-instruction (instruction one-successor-mixin)
  ())

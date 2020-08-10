(cl:in-package #:cleavir-ir)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction CHARACTERP-INSTRUCTION.
;;;
;;; This instruction is used to test whether its input is a CHARACTER.
;;; If that is the case, then the first output is chosen.  Otherwise,
;;; the second output is chosen.

(defclass characterp-instruction (instruction multiple-successors-mixin)
  ())

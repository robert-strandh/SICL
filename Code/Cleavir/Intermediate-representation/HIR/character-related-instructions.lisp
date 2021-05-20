(cl:in-package #:cleavir-ir)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction CHARACTERP-INSTRUCTION.
;;;
;;; This instruction is used to test whether its input is a CHARACTER.
;;; If that is the case, then the first output is chosen.  Otherwise,
;;; the second output is chosen.

(defclass characterp-instruction
    (instruction multiple-successors-mixin test-mixin)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction CHAR-CODE-INSTRUCTION.
;;;
;;; This instruction is used to convert a character to its code.
;;; It has one input which must be a character.  It has one output
;;; which is a fixnum representing the code of that character.

(defclass char-code-instruction (instruction one-successor-mixin)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction CODE-CHAR-INSTRUCTION.
;;;
;;; This instruction is used to convert a character code to the
;;; character with that code.  It has one input which must be a
;;; fixnum.  It has one output which is a character.

(defclass code-char-instruction (instruction one-successor-mixin)
  ())

(cl:in-package #:cleavir-ir)

(defgeneric shifted-input (instruction))

(defgeneric shift-count (instruction))

(defclass shift-instruction (instruction one-successor-mixin)
  ())

(defmethod shifted-input ((instruction shift-instruction))
  (first (inputs instruction)))

(defmethod shift-count ((instruction shift-instruction))
  (second (inputs instruction)))

(normalize-arguments
 shift-instruction
 (shifted-input shift-count)
 (output)
 (successor))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; MIR instruction SHIFT-LEFT-INSTRUCTION.

(defclass shift-left-instruction (shift-instruction)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; MIR instruction LOGIC-SHIFT-RIGHT-INSTRUCTION.

(defclass logic-shift-right-instruction (shift-instruction)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; MIR instruction ARITHMETIC-SHIFT-RIGHT-INSTRUCTION.

(defclass arithmetic-shift-right-instruction (shift-instruction)
  ())

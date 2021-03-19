(cl:in-package #:cleavir-ir)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; MIR instruction SIGNED-ADD-INSTRUCTION

(defclass signed-add-instruction
    (instruction
     multiple-successors-mixin
     binary-operation-mixin
     commutative-mixin)
  ())

(normalize-arguments
 signed-add-instruction
 (augend addend)
 (output)
 (normal-successor overflow-successor))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; MIR instruction SIGNED-SUB-INSTRUCTION

(defclass signed-sub-instruction
    (instruction
     multiple-successors-mixin
     binary-operation-mixin)
  ())

(normalize-arguments
 signed-sub-instruction
 (minuend subtrahend)
 (output)
 (normal-successor overflow-successor))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; MIR instruction SIGNED-LESS-INSTRUCTION

(defclass signed-less-instruction (instruction multiple-successors-mixin)
  ())

(normalize-arguments
 signed-less-instruction
 (argument1 argument2)
 (output)
 (true-successor false-successor))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; MIR instruction SIGNED-NOT-GREATER-INSTRUCTION

(defclass signed-not-greater-instruction (instruction multiple-successors-mixin)
  ())

(normalize-arguments
 signed-not-greater-instruction
 (argument1 argument2)
 (output)
 (true-successor false-successor))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; MIR instruction UNSIGNED-ADD-INSTRUCTION

(defclass unsigned-add-instruction
    (instruction
     multiple-successors-mixin
     binary-operation-mixin
     commutative-mixin)
  ())

(normalize-arguments
 unsigned-add-instruction
 (augend addend)
 (output)
 (normal-successor carry-successor))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; MIR instruction UNSIGNED-SUB-INSTRUCTION

(defclass unsigned-sub-instruction
    (instruction
     multiple-successors-mixin
     binary-operation-mixin)
  ())

(normalize-arguments
 unsigned-sub-instruction
 (minuend subtrahend)
 (output)
 (normal-successor carry-successor))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; MIR instruction UNSIGNED-DIV-INSTRUCTION

(defclass unsigned-div-instruction (instruction multiple-successors-mixin)
  ())

(normalize-arguments
 unsigned-div-instruction
 (dividend divisor)
 (quotient remainder)
 (successor))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; MIR instruction UNSIGNED-LESS-INSTRUCTION

(defclass unsigned-less-instruction (instruction multiple-successors-mixin)
  ())

(normalize-arguments
 unsigned-less-instruction
 (argument1 argument2)
 (output)
 (true-successor false-successor))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; MIR instruction UNSIGNED-NOT-GREATER-INSTRUCTION

(defclass unsigned-not-greater-instruction (instruction multiple-successors-mixin)
  ())

(normalize-arguments
 unsigned-not-greater-instruction
 (argument1 argument2)
 (output)
 (true-successor false-successor))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; MIR instruction EQUAL-INSTRUCTION

(defclass equal-instruction (instruction multiple-successors-mixin)
  ())

(normalize-arguments
 equal-instruction
 (argument1 argument2)
 (output)
 (true-successor false-successor))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; MIR instruction NEGATE-INSTRUCTION

(defclass negate-instruction (instruction one-successor-mixin)
  ())

(cl:in-package #:cleavir-ir)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction FIXNUM-ADD-INSTRUCTION.

(defclass fixnum-add-instruction (instruction multiple-successors-mixin)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction FIXNUM-SUB-INSTRUCTION.

(defclass fixnum-sub-instruction (instruction multiple-successors-mixin)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction FIXNUM-LESS-INSTRUCTION.

(defclass fixnum-less-instruction (instruction multiple-successors-mixin)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction FIXNUM-NOT-GREATER-INSTRUCTION.

(defclass fixnum-not-greater-instruction (instruction multiple-successors-mixin)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction FIXNUM-EQUAL-INSTRUCTION.

(defclass fixnum-equal-instruction (instruction multiple-successors-mixin)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction FIXNUM-DIVIDE-INSTRUCTION.
;;;
;;; This instruction takes two inputs, each of which must be a fixnum.
;;; It has two outputs, both fixnums.  The first output contains the
;;; quotient between the two inputs, and the second output contains
;;; the remainder.  The slot ROUNDING-MODE one of the symbols
;;; CL:FLOOR, CL:CEILING, CL:TRUNCATE, and CL:ROUND.

(defclass fixnum-divide-instruction (instruction one-successors-mixin)
  ((%rounding-mode :initarg :rounding-mode :reader rounding-mode)))

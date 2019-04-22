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

(cl:in-package #:cleavir-ir)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction CAR-INSTRUCTION.
;;;
;;; This instruction takes a single input which is assumed to be a
;;; CONS cell, and produces a single output, which is the contents of
;;; the CAR of the cons cell.

(defclass car-instruction (instruction one-successor-mixin)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction CDR-INSTRUCTION.
;;;
;;; This instruction takes a single input which is assumed to be a
;;; CONS cell, and produces a single output, which is the contents of
;;; the CDR of the cons cell.

(defclass cdr-instruction (instruction one-successor-mixin)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction RPLACA-INSTRUCTION
;;;
;;; This instruction takes two inputs.  The first input is assumed to
;;; be a CONS cell, and the second can be any object.  It has no
;;; outputs.  It replaces the CAR of the CONS cell with the object in
;;; the second input. 

(defclass rplaca-instruction
    (instruction one-successor-mixin side-effect-mixin)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction RPLACD-INSTRUCTION
;;;
;;; This instruction takes two inputs.  The first input is assumed to
;;; be a CONS cell, and the second can be any object.  It has no
;;; outputs.  It replaces the CDR of the CONS cell with the object in
;;; the second input. 

(defclass rplacd-instruction
    (instruction one-successor-mixin side-effect-mixin)
  ())

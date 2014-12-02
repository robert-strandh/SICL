(in-package #:cleavir-ir)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction CAR-INSTRUCTION.
;;;
;;; This instruction takes a single input which is assumed to be a
;;; CONS cell, and produces a single output, which is the contents of
;;; the CAR of the cons cell.

(defclass car-instruction (instruction one-successor-mixin)
  ())

(defun make-car-instruction (input output successor)
  (make-instance 'car-instruction
    :inputs (list input)
    :outputs (list output)
    :successors (list successor)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction CDR-INSTRUCTION.
;;;
;;; This instruction takes a single input which is assumed to be a
;;; CONS cell, and produces a single output, which is the contents of
;;; the CDR of the cons cell.

(defclass cdr-instruction (instruction one-successor-mixin)
  ())

(defun make-cdr-instruction (input output successor)
  (make-instance 'cdr-instruction
    :inputs (list input)
    :outputs (list output)
    :successors (list successor)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction RPLACA-INSTRUCTION
;;;
;;; This instruction takes two inputs.  The first input is assumed to
;;; be a CONS cell, and the second can be any object.  It has no
;;; outputs.  It replaces the CAR of the CONS cell with the object in
;;; the second input. 

(defclass rplaca-instruction (instruction one-successor-mixin)
  ())

(defun make-rplaca-instruction (input1 input2 successor)
  (make-instance 'rplaca-instruction
    :inputs (list input1 input2)
    :outputs '()
    :successors (list successor)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction RPLACD-INSTRUCTION
;;;
;;; This instruction takes two inputs.  The first input is assumed to
;;; be a CONS cell, and the second can be any object.  It has no
;;; outputs.  It replaces the CDR of the CONS cell with the object in
;;; the second input. 

(defclass rplacd-instruction (instruction one-successor-mixin)
  ())

(defun make-rplacd-instruction (input1 input2 successor)
  (make-instance 'rplacd-instruction
    :inputs (list input1 input2)
    :outputs '()
    :successors (list successor)))

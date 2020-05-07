(in-package #:cleavir-ir)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction SLOT-READ-INSTRUCTION.
;;;
;;; This instruction takes two inputs.  The first input is assumed
;;; to be a standard instance.  The second is assumed to be a FIXNUM
;;; and represents the index in the instance of the slot to be read.
;;; This instruction produces a single output, which is the contents
;;; of the SLOT.

(defclass slot-read-instruction (one-successor-mixin instruction)
  ())

(defun make-slot-read-instruction (input1 input2 output successor)
  (make-instance 'slot-read-instruction
    :inputs (list input1 input2)
    :outputs (list output)
    :successors (list successor)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction SLOT-WRITE-INSTRUCTION
;;;
;;; This instruction takes three inputs.  The first input is assumed
;;; to be a standard instance.  The second is assumed to be a FIXNUM
;;; and represents the index in the instance of the slot to be
;;; written.  The third is the value to write.  This instruction
;;; replaces the contents of the slot with the new value.

(defclass slot-write-instruction
    (one-successor-mixin side-effect-mixin instruction)
  ())

(defun make-slot-write-instruction (input1 input2 input3 successor)
  (make-instance 'slot-write-instruction
    :inputs (list input1 input2 input3)
    :outputs '()
    :successors (list successor)))

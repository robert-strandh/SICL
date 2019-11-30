(in-package #:cleavir-ir)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction SLOT-WRITE-INSTRUCTION
;;;
;;; This instruction takes three inputs.  The first input is assumed
;;; to be a standard object.  The second is assumed to be a FIXNUM
;;; and represents the index in the object of the slot to be
;;; written.  The third is the value to write.  This instruction
;;; replaces the contents of the slot with the new value.

(defclass slot-write-instruction
    (instruction one-successor-mixin side-effect-mixin)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction NOOK-READ-INSTRUCTION.
;;;
;;; This instruction takes two inputs.  The first input is assumed
;;; to be a standard object.  The second is assumed to be a FIXNUM
;;; and represents the index in the object of the nook to be read.
;;; This instruction produces a single output, which is the contents
;;; of the NOOK.

(defclass nook-read-instruction (instruction one-successor-mixin)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction NOOK-WRITE-INSTRUCTION
;;;
;;; This instruction takes three inputs.  The first input is assumed
;;; to be a standard object.  The second is assumed to be a FIXNUM
;;; and represents the index in the object of the nook to be
;;; written.  The third is the value to write.  This instruction
;;; replaces the contents of the nook with the new value.

(defclass nook-write-instruction
    (instruction one-successor-mixin side-effect-mixin)
  ())

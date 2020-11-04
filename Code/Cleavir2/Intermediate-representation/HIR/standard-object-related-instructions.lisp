(in-package #:cleavir-ir)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction STANDARD-OBJECT-CLASS-OF-INSTRUCTION.
;;;
;;; This instruction takes a single input which is assumed to be a
;;; standard object, and produces a single output, which is the class
;;; of the standard object.

(defclass standard-object-class-of-instruction
    (instruction one-successor-mixin)
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

(cl:in-package #:sicl-ir)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction RACK-INSTRUCTION.
;;;
;;; This instruction takes a single input which is assumed to be a
;;; standard object, and produces a single output, which is the
;;; rack of the standard object.

(defclass rack-instruction
    (cleavir-ir:instruction cleavir-ir:one-successor-mixin)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction SET-RACK-INSTRUCTION
;;;
;;; This instruction takes two inputs.  The first input is assumed to
;;; be a standard object, and the second is assumed to be a rack.  It
;;; has no outputs.  It replaces the rack of the standard object with the
;;; rack in the second input.

(defclass set-rack-instruction
    (cleavir-ir:instruction
     cleavir-ir:one-successor-mixin
     cleavir-ir:side-effect-mixin)
  ())

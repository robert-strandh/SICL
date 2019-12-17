(cl:in-package #:cleavir-ir)

;;;; These instruction can be used by implementation that would like
;;;; to make return-value parsing explicitly represented in HIR code.
;;;;
;;;; Such an implementation would translate the
;;;; MULTIPLE-TO-FIXED-INSTRUCTION to tests comparing the value
;;;; computed by the COMPUTE-RETURN-VALUE-COUNT-INSTRUCTION to fixnums
;;;; between 0 and the number of fixed outputs of the
;;;; MULTIPLE-TO-FIXED-INSTRUCTION.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction COMPUTE-RETURN-VALUE-COUNT-INSTRUCTION.
;;;
;;; This instruction has no input, and it has a single output, which
;;; is a fixnum containing the number of return values returned from a
;;; function call.

(defclass compute-return-value-count-instruction
    (instruction one-successor-mixin)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction RETURN-VALUE-INSTRUCTION.
;;;
;;; This instruction has a single input, which must contain a
;;; non-negative fixnum.  It has a single output, which is the return
;;; value with the index indicated by the input.  The first return
;;; value has index 0.  The input must have a value that is strictly
;;; less than the argument count as computed by the instruction
;;; COMPUTE-RETURN-VALUE-COUNT-INSTRUCTION, except that in
;;; implementations that always return NIL in the first return-value
;;; position, an input value of 0 is always valid.

(defclass return-value-instruction (instruction one-successor-mixin)
  ())

(cl:in-package #:cleavir-ir)

;;;; These instruction can be used by implementations that would like
;;;; to make return-value processing explicitly represented in HIR
;;;; code.
;;;;
;;;; Such an implementation would translate the
;;;; MULTIPLE-TO-FIXED-INSTRUCTION to tests, comparing the value
;;;; computed by the COMPUTE-RETURN-VALUE-COUNT-INSTRUCTION to fixnums
;;;; between 0 and the number of fixed outputs of the
;;;; MULTIPLE-TO-FIXED-INSTRUCTION.
;;;;
;;;; And such an implementation would translate the
;;;; FIXED-TO-MULTIPLE-INSTRUCTION to an instance of the
;;;; INITIALIZE-RETURN-VALUES-INSTRUCTION followed by a sequence of
;;;; instances of the SET-RETURN-VALUE-INSTRUCTION.

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction INITIALIZE-RETURN-VALUES-INSTRUCTION.
;;;
;;; This instruction has a single input, which must contain a
;;; non-negative fixnum.  It has no outputs.  It is used to initialize
;;; the return values from a function, and the input indicates how
;;; many return values are to be returned.  Some implementations may
;;; use this instruction to allocate room for return values.

(defclass initialize-return-values-instruction
    (instruction one-successor-mixin side-effect-mixin)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction SET-RETURN-VALUE-INSTRUCTION.
;;;
;;; This instruction has two inputs.  The first input must contain a
;;; non-negative fixnum that is strictly less than the value given as
;;; an input to the INITIALIZE-RETURN-VALUES-INSTRUCTION, except that
;;; in implementations that always return NIL in the first
;;; return-value position, an input value of 0 is always valid.  The
;;; second input contains the value to be store in the the
;;; distinguished values location, at the index corresponding to the
;;; first input.

(defclass set-return-value-instruction
    (instruction one-successor-mixin side-effect-mixin)
  ())

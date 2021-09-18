(cl:in-package #:sicl-ir)

;;; This instruction takes three inputs.  The first input is the
;;; object computed at load time and which will become a literal
;;; object at run time.  The second input is an index into the code
;;; vector where the object should be stored for use by an instruction
;;; that loads an immediate object from the instruction stream.  The
;;; third input is an index into the vector of constants where the
;;; object should be stored so that it can be traced by the garbage
;;; collector to keep it live.
;;;
;;; The slot contains a CONS cell that is shared by the corresponding
;;; LOAD-LITERAL instruction.  The HIR evaluator evaluates this
;;; instruction by storing the first input in the CAR of the CONS
;;; cell, and by calling the function
;;; SICL-RUN-TIME:RESOLVE-LOAD-TIME-VALUE with the three inputs as
;;; arguments.

(defclass patch-literal-instruction
    (cleavir-ir:instruction
     cleavir-ir:one-successor-mixin
     cleavir-ir:side-effect-mixin)
  (;; This slot holds a CONS cell that is shared with the LOAD-LITERAL
   ;; instruction.
   (%literal-cell :initarg :literal-cell :reader literal-cell)))

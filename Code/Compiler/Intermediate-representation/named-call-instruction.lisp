(cl:in-package #:sicl-ir)

;;; This instruction class adds a slot to the ordinary
;;; NAMED-CALL-INSTRUCTION and similar instructions for the purpose of
;;; the HIR evaluator.  This slot serves the purpose of a double
;;; indirection.
;;;
;;; When the HIR evaluator processes instructions, the code object has
;;; not yet been tied to any environment, so the function cells are
;;; unknown.  If we stored the function cell directly in this
;;; instruction, the HIR evaluator would have to close over the
;;; instruction, which is not desirable, because the instruction will
;;; change when HIR is transformed to MIR, LIR, etc.  So instead we
;;; store a CONS cell in the instruction, and the CAR of that CONS
;;; cell will ultimately contain the function cell, i.e. after the
;;; code object has been tied.  The HIR evaluator closes over this
;;; cell.
;;;
;;; So when we establish call sites, we find all the
;;; NAMED-CALL-INSTRUCTIONs, change their classes to this one, and
;;; store a CONS cell in the new slot.  When the code object is tied
;;; to the environment, we store the function cell in the CAR of this
;;; cell.
(defclass named-call-mixin ()
  ((%function-cell-cell
    :initarg :function-cell-cell
    :reader function-cell-cell)))

(defmethod cleavir-ir:clone-initargs append ((instruction named-call-mixin))
  (list :function-cell-cell (function-cell-cell instruction)))

(defclass named-call-instruction
    (cleavir-ir:named-call-instruction named-call-mixin)
  ())

(defclass catch-instruction
    (cleavir-ir:catch-instruction named-call-mixin)
  ())

(defclass bind-instruction
    (cleavir-ir:bind-instruction named-call-mixin)
  ())

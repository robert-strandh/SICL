(cl:in-package #:sicl-ir)

;;; This instruction class adds a function cell slot to the ordinary
;;; NAMED-CALL-INSTRUCTION for the purpose of the HIR evaluator.  When
;;; the code is tied to an environment, we go through all the
;;; NAMED-CALL-INSTRUCTIONs, change their classes to this one, and
;;; store the function cell corresponding to the name in the new slot.
(defclass named-call-instruction (cleavir-ir:named-call-instruction)
  ((%function-cell :initarg :function-cell :reader function-cell)))

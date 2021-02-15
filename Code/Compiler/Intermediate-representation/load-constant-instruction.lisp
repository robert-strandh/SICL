(cl:in-package #:sicl-ir)

;;; For the benefit of the HIR evaluator, we add a slot that will
;;; contain the vector of constants created from constants in the AST
;;; and the result of executing LOAD-TIME-VALUE forms.

(defclass load-constant-instruction (cleavir-ir:load-constant-instruction)
  ((%constants :initarg :constants :accessor constants)))

(defmethod cleavir-ir:clone-initargs append
    ((instruction load-constant-instruction))
  (list :constants nil))


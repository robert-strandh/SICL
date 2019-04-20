(cl:in-package #:cleavir-cst-to-ast)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; CONVERT-CONSTANT is called when a constant is found, either in the
;;; form of a literal or in the form of a constant variable.

(defun convert-constant (client constant-cst lexical-environment dynamic-environment-ast)
  (declare (ignore client lexical-environment))
  (let ((expression (cst:raw constant-cst)))
    (make-instance 'cleavir-ast:load-time-value-ast
      :form `',expression
      :read-only-p t
      :dynamic-environment-ast dynamic-environment-ast)))

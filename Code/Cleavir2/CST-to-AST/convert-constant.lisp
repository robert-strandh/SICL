(cl:in-package #:cleavir-cst-to-ast)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; CONVERT-CONSTANT is called when a constant is found, either in the
;;; form of a literal or in the form of a constant variable.

(defun convert-constant (client constant-cst lexical-environment)
  (declare (ignore client lexical-environment))
  (make-instance 'cleavir-ast:constant-ast
    :value (cst:raw constant-cst)))

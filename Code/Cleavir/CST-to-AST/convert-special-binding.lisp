(cl:in-package #:cleavir-cst-to-ast)

;;; FIXME.  This function should take a CST instead of a variable, and
;;; CONVERT should be called with a CST.
(defmethod convert-special-binding
    (variable value-ast next-ast env system)
  (convert `(cleavir-primop:call-with-variable-bound
             ',variable (cleavir-primop:ast ,value-ast)
             (lambda () (cleavir-primop:ast ,next-ast)))
           env system))

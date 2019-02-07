(cl:in-package #:cleavir-cst-to-ast)

(defun cst-to-ast (cst environment system
                       &optional (*dynamic-environment-ast*
                                  (cleavir-ast:make-lexical-ast
                                   '#:unused-dynamic-environment
                                   :policy (cleavir-env:environment-policy
                                            environment))))
  (let ((*subforms-are-top-level-p* t)
	(*compile-time-too* nil))
    (convert cst environment system)))

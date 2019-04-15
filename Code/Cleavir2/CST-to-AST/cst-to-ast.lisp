(cl:in-package #:cleavir-cst-to-ast)

(defun cst-to-ast (cst environment system
                       &optional (cleavir-ast:*dynamic-environment*
                                  (cleavir-ast:make-dynamic-environment-ast
                                   '#:unused-dynamic-environment
                                   :policy (cleavir-env:environment-policy
                                            environment))))
  (let ((*subforms-are-top-level-p* t)
	(*compile-time-too* nil))
    (convert cst environment system)))

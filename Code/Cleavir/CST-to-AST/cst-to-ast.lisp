(cl:in-package #:cleavir-cst-to-ast)

(defun cst-to-ast (cst environment system)
  (let ((*subforms-are-top-level-p* t)
	(*compile-time-too* nil))
    (convert cst environment system)))

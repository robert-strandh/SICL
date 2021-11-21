(cl:in-package #:cleavir-cst-to-ast)

(defmethod trivial-constant-p (client object)
  (declare (ignorable client object))
  nil)

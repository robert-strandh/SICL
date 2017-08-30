(cl:in-package #:cleavir-cst-to-ast-test)

(defmethod make-load-form ((object cleavir-ast:ast) &optional environment)
  (declare (ignore environment))
  (cleavir-ast-transformations:codegen-clone-ast object))

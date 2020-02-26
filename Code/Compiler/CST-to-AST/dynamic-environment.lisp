(cl:in-package #:sicl-cst-to-ast)

(defmethod cleavir-cst-to-ast:convert-special
    (client (symbol (eql 'sicl-primop:dynamic-environment)) cst environment)
  (declare (ignore client environment))
  (cleavir-cst-to-ast:check-simple-primop-syntax cst 0)
  (cleavir-ast:make-ast 'sicl-ast:dynamic-environment-ast))

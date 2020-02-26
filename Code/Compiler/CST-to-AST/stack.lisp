(cl:in-package #:sicl-cst-to-ast)

(defmethod cleavir-cst-to-ast:convert-special
    (client (symbol (eql 'sicl-primop:caller-stack-pointer)) cst environment)
  (declare (ignore client environment))
  (cleavir-cst-to-ast:check-simple-primop-syntax cst 0)
  (cleavir-ast:make-ast 'sicl-ast:caller-stack-pointer-ast))

(defmethod cleavir-cst-to-ast:convert-special
    (client (symbol (eql 'sicl-primop:caller-frame-pointer)) cst environment)
  (declare (ignore client environment))
  (cleavir-cst-to-ast:check-simple-primop-syntax cst 0)
  (cleavir-ast:make-ast 'sicl-ast:caller-frame-pointer-ast))

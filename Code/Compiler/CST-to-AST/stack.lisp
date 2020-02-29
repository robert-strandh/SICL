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

(defmethod cleavir-cst-to-ast:convert-special
    (client (symbol (eql 'sicl-primop:establish-stack-frame)) cst environment)
  (cst:db origin (esf-cst stack-pointer-cst frame-pointer-cst) cst
    (declare (ignore esf-cst))
    (cleavir-ast:make-ast 'sicl-ast:establish-stack-frame-ast
      :stack-pointer-ast
      (cleavir-cst-to-ast:convert client stack-pointer-cst environment)
      :frame-pointer-ast
      (cleavir-cst-to-ast:convert client frame-pointer-cst environment))))

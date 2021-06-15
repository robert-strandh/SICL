(cl:in-package #:sicl-cst-to-ast)

(defmethod cleavir-cst-to-ast:convert-special
    (client (symbol (eql 'sicl-primop:rack)) cst environment)
  (cleavir-cst-to-ast:check-simple-primop-syntax cst 1)
  (cst:db origin (rack-cst standard-object-cst) cst
    (declare (ignore rack-cst))
    (cleavir-ast:make-ast 'sicl-ast:rack-ast
      :standard-object-ast
      (cleavir-cst-to-ast:convert client standard-object-cst environment))))

(defmethod cleavir-cst-to-ast:convert-special
    (client (symbol (eql 'sicl-primop:set-rack)) cst environment)
  (cleavir-cst-to-ast:check-simple-primop-syntax cst 2)
  (cst:db origin (set-rack-cst standard-object-cst rack-cst) cst
    (declare (ignore set-rack-cst))
    (cleavir-ast:make-ast 'sicl-ast:set-rack-ast
      :standard-object-ast
      (cleavir-cst-to-ast:convert client standard-object-cst environment)
      :rack-ast
      (cleavir-cst-to-ast:convert client rack-cst environment))))

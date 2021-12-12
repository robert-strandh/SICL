(cl:in-package #:sicl-cst-to-ast)

(defmethod cleavir-cst-to-ast:convert-special
    (client (symbol (eql 'sicl-primop:dynamic-environment)) cst environment)
  (declare (ignore client environment))
  (cleavir-cst-to-ast:check-simple-primop-syntax cst 0)
  (cleavir-ast:make-ast 'sicl-ast:dynamic-environment-ast))

(defmethod cleavir-cst-to-ast:convert-special
    (client (symbol (eql 'sicl-primop:with-dynamic-environment)) cst environment)
  (cst:db origin (wde-cst dynamic-environment-cst . body-csts) cst
    (declare (ignore wde-cst))
    (cleavir-ast:make-ast 'sicl-ast:with-dynamic-environment-ast
      :dynamic-environment-ast
      (cleavir-cst-to-ast:convert client dynamic-environment-cst environment)
      :body-ast
      (cleavir-cst-to-ast:process-progn
       client
       (loop for cst in body-csts
             collect (cleavir-cst-to-ast:convert client cst environment))
       environment))))

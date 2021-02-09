(cl:in-package #:sicl-cst-to-ast)

(defmethod cleavir-cst-to-ast:convert-special-variable
    ((client sicl-client:sicl) cst info global-env)
  (make-instance 'cleavir-ast:named-call-ast
    :callee-name 'symbol-value
    :argument-asts
    (list
     (cleavir-ast:make-ast 'cleavir-ast:constant-ast
       :value (trucler:name info)))))

(defmethod cleavir-cst-to-ast:convert-setq-special-variable
    ((client sicl-client:sicl) var-cst form-ast info global-env)
  (make-instance 'cleavir-ast:named-call-ast
    :callee-name '(setf symbol-value)
    :argument-asts
    (list
     form-ast
     (cleavir-ast:make-ast 'cleavir-ast:constant-ast
       :value (trucler:name info)))))

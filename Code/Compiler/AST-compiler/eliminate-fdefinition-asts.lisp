(cl:in-package #:sicl-compiler)

(defun find-fdefinition-asts (ast)
  (let ((result '()))
    (cleavir-ast:map-ast-depth-first-preorder
     (lambda (node)
       (when (typep node 'cleavir-ast:fdefinition-ast)
         (push node result)))
     ast)
    result))

(defun eliminate-fdefinition-asts (ast)
  (let ((fdefinition-asts (find-fdefinition-asts ast)))
    (loop for ast in fdefinition-asts
          for origin = (cleavir-cst-to-ast:origin ast)
          for name-ast = (cleavir-ast:name-ast ast)
          for call-ast = (make-instance 'cleavir-ast:named-call-ast
                           :origin origin
                           :callee-name 'sicl-data-and-control-flow:function-cell
                           :argument-asts (list name-ast))
          for load-time-value-ast = (make-instance 'cleavir-ast:load-time-value-ast
                                      :origin origin
                                      :form-ast call-ast)
          do (change-class ast 'cleavir-ast:car-ast
                           :cons-ast load-time-value-ast))))

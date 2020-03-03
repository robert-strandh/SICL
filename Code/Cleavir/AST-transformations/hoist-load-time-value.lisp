(cl:in-package #:cleavir-ast-transformations)

(defun find-load-time-value-asts (ast)
  (let ((result '()))
    (cleavir-ast:map-ast-depth-first-preorder
     (lambda (node)
       (when (typep node 'cleavir-ast:load-time-value-ast)
	 (push node result)))
     ast)
    result))

(defun hoist-load-time-value (ast dynamic-environment-ast)
  (let* ((load-time-value-asts (find-load-time-value-asts ast))
	 (forms (mapcar #'cleavir-ast:form load-time-value-asts)))
    (loop for ast in load-time-value-asts
	  do (change-class ast 'cleavir-ast:lexical-ast :name (gensym)))
    (cleavir-ast:make-top-level-function-ast
     ast load-time-value-asts forms
     :policy (cleavir-ast:policy ast))))

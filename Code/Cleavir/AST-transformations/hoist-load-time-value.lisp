(cl:in-package #:cleavir-ast-transformations)

(defun find-load-time-value-asts (ast)
  (let ((table (make-hash-table :test #'eq)))
    (labels ((traverse (ast)
	       (unless (gethash ast table)
		 (setf (gethash ast table) t)
		 (if (typep ast 'cleavir-ast:load-time-value-ast)
		     (list ast)
		     (let ((children (cleavir-ast:children ast)))
		       (reduce #'append
			       (mapcar #'traverse children)
			       :from-end t))))))
      (traverse ast))))

(defun hoist-load-time-value (ast)
  (let* ((load-time-value-asts (find-load-time-value-asts ast))
	 (forms (mapcar #'cleavir-ast:form load-time-value-asts)))
    (loop for ast in load-time-value-asts
	  do (change-class ast 'cleavir-ast:lexical-ast :name (gensym)))
    (cleavir-ast:make-top-level-function-ast
     ast load-time-value-asts forms)))

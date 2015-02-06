(cl:in-package #:cleavir-ast-transformations)

(defun find-load-time-value-asts (ast)
  (if (typep ast 'cleavir-ast:load-time-value-ast)
      (list ast)
      (let ((children (cleavir-ast:children ast)))
	(reduce #'append (mapcar #'find-load-time-value-asts children)
		:from-end t))))

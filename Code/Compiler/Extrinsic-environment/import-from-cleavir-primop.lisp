(cl:in-package #:sicl-extrinsic-environment)

(defun import-from-cleavir-primop (environment)
  (setf (sicl-env:fdefinition 'cleavir-primop:call-with-variable-bound
			      environment)
	(fdefinition 'call-with-variable-bound)))

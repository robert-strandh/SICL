(cl:in-package #:sicl-extrinsic-environment)

(defun import-from-cleavir-primop (environment)
  (let ((fname 'cleavir-primop:call-with-variable-bound))
    (setf (sicl-env:fdefinition fname environment)
	  (fdefinition fname))))

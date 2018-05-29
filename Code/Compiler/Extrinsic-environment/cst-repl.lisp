(cl:in-package #:sicl-extrinsic-environment)

(defun cst-repl (env1 env2 system)
  (loop
    do (princ "SICL: ")
       (finish-output)
       (setf *dynamic-environment* '())
       (let ((form (eclector.reader:read)))
	 (when (equal form '(quit))
	   (loop-finish))
	 (let ((values (multiple-value-list
			(cleavir-env:cst-eval (cst:cst-from-expression form) env1 env2 system))))
	   (loop for value in values
		 do (print value))
	   (terpri)))))

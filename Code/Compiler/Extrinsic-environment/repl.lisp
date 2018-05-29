(cl:in-package #:sicl-extrinsic-environment)

(defun repl (env1 env2)
  (loop
    do (princ "SICL: ")
       (finish-output)
       (setf *dynamic-environment* '())
       (let ((form (eclector.reader:read)))
	 (when (equal form '(quit))
	   (loop-finish))
	 (let ((values (multiple-value-list
			(cleavir-env:eval form env1 env2))))
	   (loop for value in values
		 do (print value))
	   (terpri)))))

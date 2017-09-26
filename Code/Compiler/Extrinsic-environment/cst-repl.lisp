(cl:in-package #:sicl-extrinsic-environment)

(defun cst-repl (env1 env2)
  (loop
    do (princ "SICL: ")
       (finish-output)
       (setf *dynamic-environment* '())
       (let ((form (sicl-reader:read)))
	 (when (equal form '(quit))
	   (loop-finish))
	 (let ((values (multiple-value-list
			(cst-eval form env1 env2))))
	   (loop for value in values
		 do (print value))
	   (terpri)))))

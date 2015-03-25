(cl:in-package #:sicl-extrinsic-environment)

(defun repl (environment)
  (loop
    do (princ "SICL: ")
       (setf *dynamic-environment* '())
       (let ((form (sicl-reader:read)))
	 (when (equal form '(quit))
	   (loop-finish))
	 (let ((values (multiple-value-list
			(cleavir-env:eval form environment environment))))
	   (loop for value in values
		 do (print value))
	   (terpri)))))

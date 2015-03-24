(cl:in-package #:sicl-extrinsic-environment)

(defun repl (environment)
  (setf *dynamic-environment* '())
  (loop
    do (princ "SICL: ")
       (let ((form (sicl-reader:read)))
	 (when (equal form '(quit))
	   (loop-finish))
	 (let ((value (cleavir-env:eval form environment environment)))
	   (print value)
	   (terpri)))))

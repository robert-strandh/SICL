(cl:in-package #:sicl-extrinsic-environment)

(defun define-in-package (environment)
  (setf (sicl-env:macro-function 'in-package environment)
	(lambda (form environment)
	  (declare (ignore environment))
	  (setq *package* (find-package (cadr form)))
	  `(setq *package* (find-package ',(cadr form))))))

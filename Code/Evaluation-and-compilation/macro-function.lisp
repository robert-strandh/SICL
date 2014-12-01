(cl:in-package #:sicl-evaluation-and-compilation)

(defun macro-function (symbol &optional environment)
  (cleavir-env:macro-function symbol (or environment *global-environment*)))

(defun (setf macro-function) (new-function symbol &optional environment)
  (unless (null environment)
    (error 'environment-must-be-omitted-or-nil))
  (setf (sicl-env:macro-function symbol *global-environment*)
	new-function))

(cl:in-package #:sicl-evaluation-and-compilation)

(defun macro-function (symbol &optional environment)
  (cleavir-env:macro-function symbol (or environment *global-environment*)))

(cl:in-package #:cleavir-ir)

(defun all (&rest arguments)
  (notany #'null arguments))

(defun none (&rest arguments)
  (every #'null arguments))

(defun all-or-none (&rest arguments)
  (or (apply #'all arguments)
      (apply #'null arguments)))

(defun combine (combination &rest arguments)
  (if (apply #'none arguments)
      combination
      (copy-list arguments)))

(cl:in-package #:sicl-cons)

(defun consp (object)
  (if (cleavir-primop:consp object) t nil))

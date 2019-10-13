(cl:in-package #:sicl-data-and-control-flow)

(defun eq (x y)
  (if (cleavir-primop:eq x y) t nil))

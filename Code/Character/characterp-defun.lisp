(cl:in-package #:sicl-character)

(defun characterp (object)
  (if (cleavir-primop:characterp object) t nil))

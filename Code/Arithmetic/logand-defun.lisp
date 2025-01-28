(cl:in-package #:sicl-arithmetic)

(defun logand (&rest arguments)
  (case (length arguments)
    (0 -1)
    (1 (progn (assert (integerp (first arguments))) (first arguments)))
    (t (reduce #'binary-logand arguments))))

(cl:in-package #:sicl-arithmetic)

(defun logior (&rest arguments)
  (case (length arguments)
    (0 0)
    (1 (progn (assert (integerp (first arguments))) (first arguments)))
    (t (reduce #'binary-logior arguments))))

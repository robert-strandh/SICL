(cl:in-package #:sicl-arithmetic)

(defun logand (&rest arguments)
  (case (length arguments)
    (0 -1)
    (1 (progn (check-type (first arguments) 'integer) (first arguments)))
    (t (reduce #'binary-logand arguments))))

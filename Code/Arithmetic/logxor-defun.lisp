(cl:in-package #:sicl-arithmetic)

(defun logxor (&rest arguments)
  (case (length arguments)
    (0 0)
    (1 (progn (check-type (first arguments) 'integer) (first arguments)))
    (t (reduce #'binary-logxor arguments))))

(cl:in-package #:sicl-arithmetic)

(defun logior (&rest arguments)
  (case (length arguments)
    (0 0)
    (1 (progn (check-type (first arguments) 'integer) (first arguments)))
    (t (reduce #'binary-logior arguments))))

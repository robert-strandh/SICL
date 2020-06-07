(cl:in-package #:sicl-cons)

(defun consp (object)
  (typep object 'cons))

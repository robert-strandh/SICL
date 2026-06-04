(cl:in-package #:sicl-cons)

(defun consp (object)
  (sicl-primop:primop :consp object))

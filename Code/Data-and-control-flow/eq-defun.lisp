(cl:in-package #:sicl-data-and-control-flow)

(defun eq (x y)
  (if (sicl-primop:primop :eq x y) t nil))

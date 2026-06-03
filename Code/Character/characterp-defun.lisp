(cl:in-package #:sicl-character)

(defun characterp (object)
  (if (sicl-primop:primop :characterp object) t nil))

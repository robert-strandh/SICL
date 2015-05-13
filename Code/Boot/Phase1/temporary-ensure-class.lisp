(cl:in-package #:sicl-clos)

(defun ensure-class (name &rest arguments &key &allow-other-keys)
  (apply #'ensure-class-using-class-null name arguments))

(cl:in-package #:sicl-arithmetic)

(defclass single-float (float)
  ()
  (:metaclass built-in-class))

(defun single-float-p (object)
  (typep object 'single-float))

(cl:in-package #:sicl-symbol)

(defun (setf symbol-function) (new-function symbol)
  (check-type symbol symbol)
  (check-type new-function function)
  (setf (fdefinition symbol) new-function))

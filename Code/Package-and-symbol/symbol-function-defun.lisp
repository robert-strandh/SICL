(cl:in-package #:sicl-symbol)

(defun symbol-function (symbol)
  (check-type symbol symbol)
  (fdefinition symbol))

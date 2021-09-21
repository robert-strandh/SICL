(cl:in-package #:sicl-compiler)

(defun ensure-literal (literals literal)
  (when (null (position literal literals))
    (vector-push-extend literal literals)))

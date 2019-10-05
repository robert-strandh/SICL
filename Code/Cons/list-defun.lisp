(cl:in-package #:sicl-cons)

;;; This implementation assumes that there is no structure sharing
;;; between the &rest argument and the last argument to apply.

(defun list (&rest elements)
  elements)

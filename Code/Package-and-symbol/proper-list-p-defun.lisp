(cl:in-package #:sicl-package)

(defun proper-list-p (list)
  (integerp (ignore-errors (list-length list))))

(cl:in-package #:sicl-array)

(defun vector (&rest objects)
  (make-array (length objects) :initial-contents objects))

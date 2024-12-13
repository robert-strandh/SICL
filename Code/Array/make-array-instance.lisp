(cl:in-package #:sicl-array)

(defun regalia:make-array-instance
    (class-name
     &key dimensions
       size
       element-type)
  (make-instance class-name
    :dimensions dimensions
    :element-type element-type
    :additional-size size))

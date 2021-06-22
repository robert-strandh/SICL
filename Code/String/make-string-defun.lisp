(cl:in-package #:sicl-string)

(defun make-string (size &key (initial-element #\Space) (element-type 'character))
  (loop with result = (make-instance 'string
                        :dimensions (list size)
                        :additional-space (ceiling size 2))
        for index from 0 below size
        do (setf (aref result index) initial-element)
        finally (return result)))

(cl:in-package #:sicl-array)

(defun vector-push (new-element vector)
  (if (>= (fill-pointer vector)
          (array-dimension vector 0))
      nil
      (prog1 (fill-pointer vector)
        (setf (aref vector (fill-pointer vector))
              new-element)
        (incf (fill-pointer vector)))))

(cl:in-package #:sicl-sequence)

(defmethod map-into ((list list) function &rest sequences)
  (let ((function (function-designator-function function))
        (rest list))
    (flet ((fn (&rest arguments)
             (when (endp rest)
               (return-from map-into list))
             (setf (car rest)
                   (apply function arguments))
             (pop rest)))
      (declare (dynamic-extent #'fn))
      (apply #'map-for-effect #'fn sequences)
      list)))

(replicate-for-each-vector-class #1=#:vector-class
  (defmethod map-into ((vector #1#) function &rest sequences)
    (let ((function (function-designator-function function))
          (index 0))
      (declare (vector-length index))
      (block nil
        (flet ((fn (&rest arguments)
                 (unless (array-in-bounds-p vector index)
                   (return))
                 ;; We have to use AREF instead of ELT here, because only the
                 ;; former ignores the fill-pointer.
                 (setf (aref vector index)
                       (apply function arguments))
                 (incf index)))
          (declare (dynamic-extent #'fn))
          (apply #'map-for-effect #'fn sequences)))
      (when (array-has-fill-pointer-p vector)
        (setf (fill-pointer vector) index))
      vector)))

(seal-domain #'map-into '(list t))
(seal-domain #'map-into '(vector t))

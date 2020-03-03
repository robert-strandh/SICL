(cl:in-package #:sicl-sequence)

(defmethod elt ((list list) index)
  (check-type index array-index)
  (car (nth-cons list index)))

(defmethod (setf elt) (value (list list) index)
  (check-type index array-index)
  (setf (car (nth-cons list index)) value))

(defmethod elt ((vector vector) index)
  (cl:elt vector index))

(defmethod (setf elt) (value (vector vector) index)
  (setf (cl:elt vector index) value))

(seal-domain #'elt '(sequence t))
(seal-domain #'(setf elt) '(t sequence t))

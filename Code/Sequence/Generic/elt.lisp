(cl:in-package #:sicl-sequence)

(defmethod elt ((list list) index)
  (declare (method-properties inlineable))
  (check-type index array-index)
  (car (nth-cons list index)))

(seal-domain #'elt '(list t))

(defmethod elt ((vector vector) index)
  (declare (method-properties inlineable))
  (cl:elt vector index))

(seal-domain #'elt '(vector t))

(defmethod (setf elt) (value (list list) index)
  (declare (method-properties inlineable))
  (check-type index array-index)
  (setf (car (nth-cons list index)) value))

(seal-domain #'(setf elt) '(t list t))

(defmethod (setf elt) (value (vector vector) index)
  (declare (method-properties inlineable))
  (setf (cl:elt vector index) value))

(seal-domain #'(setf elt) '(t vector t))

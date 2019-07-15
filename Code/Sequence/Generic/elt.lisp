(cl:in-package #:sicl-sequence)

(defmethod elt ((datum t) index)
  (error 'must-be-sequence
         :datum datum))

(defmethod elt ((vector vector) index)
  (cl:elt vector index))

(defmethod elt ((list list) index)
  (cl:elt list index))

(defmethod (setf elt) (value (vector vector) index)
  (setf (cl:elt vector index) value))

(defmethod (setf elt) (value (list list) index)
  (setf (cl:elt list index) value))

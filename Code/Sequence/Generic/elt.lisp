(cl:in-package #:sicl-sequence)

(defmethod elt ((datum t) index)
  (error 'must-be-sequence
         :datum datum))

(defmethod elt ((list list) index)
  (check-type index array-index)
  (labels ((find-elt (rest counter)
             (if (atom rest)
                 (if (null rest)
                     (error 'invalid-sequence-index
                            :datum list
                            :expected-type `(integer 0 ,(- index counter)))
                     (error 'must-be-proper-list
                            :datum list))
                 (if (zerop counter)
                     (car rest)
                     (find-elt (cdr rest) (1- counter))))))
    (find-elt list index)))

(defmethod elt ((vector vector) index)
  (cl:elt vector index))

(defmethod (setf elt) (value (list list) index)
  (setf (cl:elt list index) value))

(defmethod (setf elt) (value (vector vector) index)
  (setf (cl:elt vector index) value))


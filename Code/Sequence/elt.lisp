(cl:in-package #:sicl-sequence)

(declaim (inline elt-aux))
(defun elt-aux (list index)
  (declare (list list) (list-index index))
  (loop for rest = list then (cdr rest)
        for position of-type list-length from 0
        when (endp rest) do
          (error 'invalid-sequence-index
                 :datum index
                 :in-sequence list
                 :expected-type `(integer 0 ,(1- position)))
        when (= position index) do (loop-finish)
        finally (return rest)))

(defmethod elt ((list list) index)
  (declare (method-properties inlineable))
  (check-type index list-index)
  (car (elt-aux list index)))

(seal-domain #'elt '(list t))

(defmethod elt ((vector vector) index)
  (declare (method-properties inlineable))
  (aref vector index))

(seal-domain #'elt '(vector t))

(defmethod (setf elt) (value (list list) index)
  (declare (method-properties inlineable))
  (check-type index list-index)
  (setf (car (elt-aux list index)) value))

(seal-domain #'(setf elt) '(t list t))

(defmethod (setf elt) (value (vector vector) index)
  (declare (method-properties inlineable))
  (setf (aref vector index) value))

(seal-domain #'(setf elt) '(t vector t))

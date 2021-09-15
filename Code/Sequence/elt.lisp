(cl:in-package #:sicl-sequence)

(declaim (notinline %invalid-sequence-index))
(defun %invalid-sequence-index (index sequence end)
  (error 'invalid-sequence-index
         :datum index
         :in-sequence sequence
         :expected-type `(integer 0 (,end))))

;;; List

;;; Returns the cons cell at INDEX, or signals an appropriate error.
(declaim (inline elt-aux))
(defun elt-aux (list index)
  (declare (list list) (list-index index))
  (loop for rest = list then (cdr rest)
        for position of-type list-length from 0
        when (endp rest) do
          (%invalid-sequence-index index list position)
        when (= position index) do (loop-finish)
          finally (return rest)))

(defmethod elt ((list list) index)
  (declare (method-properties inlineable))
  (check-type index list-index)
  (car (elt-aux list index)))

(seal-domain #'elt '(list t))

(defmethod (setf elt) (value (list list) index)
  (declare (method-properties inlineable))
  (check-type index list-index)
  (setf (car (elt-aux list index)) value))

(seal-domain #'(setf elt) '(t list t))

;;; Vector

(defmethod elt ((vector vector) index)
  (declare (method-properties inlineable))
  (let ((end (if (array-has-fill-pointer-p vector)
                 (fill-pointer vector)
                 (array-dimension vector 0))))
    (unless (< -1 index end)
      (%invalid-sequence-index index vector end)))
  (aref vector index))

(seal-domain #'elt '(vector t))

(defmethod (setf elt) (value (vector vector) index)
  (declare (method-properties inlineable))
  (let ((end (if (array-has-fill-pointer-p vector)
                 (fill-pointer vector)
                 (array-dimension vector 0))))
    (unless (< -1 index end)
      (%invalid-sequence-index index vector end)))
  (setf (aref vector index) value))

(seal-domain #'(setf elt) '(t vector t))

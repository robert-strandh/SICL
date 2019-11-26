(cl:in-package #:sicl-array)

(defmethod row-major-aref (object index)
  (error 'object-must-be-an-array
         :datum object
         :expected-type 'array))

(defmethod row-major-aref :before ((array array) index)
  (unless (and (numberp index)
               (>= index 0)
               (< index (array-total-size array)))
    ;; FIXME: signal a more specific condition.
    (error 'type-error
           :datum index
           :expected-type `(integer 0 (,(array-total-size array))))))

(defmethod (setf row-major-aref) :before (new-element (array array) index)
  (declare (ignore new-element))
  (unless (and (numberp index)
               (>= index 0)
               (< index (array-total-size array)))
    ;; FIXME: signal a more specific condition.
    (error 'type-error
           :datum index
           :expected-type `(integer 0 (,(array-total-size array))))))

(defmethod row-major-aref ((array array-t) index)
  (cleavir-primop:aref array
                       index
                       t
                       t
                       t))

(defmethod (setf row-major-aref) (new-element (array array-t) index)
  (progn (cleavir-primop:aset array
                              index
                              new-element
                              t
                              t
                              t)
         new-element))

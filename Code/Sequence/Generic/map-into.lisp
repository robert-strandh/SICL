(cl:in-package #:sicl-sequence)

(defmethod map-into ((list list) function &rest sequences)
  (flet ((terminate (n)
           (declare (ignore n))
           (return-from map-into list)))
    (let* ((n (length sequences))
           (function (function-designator-function function))
           (writer (make-sequence-writer list 0 nil nil #'terminate))
           (readers (make-array n)))
      (loop for index below n and sequence in sequences do
        (setf (aref readers index)
              (make-sequence-reader sequence 0 nil nil #'terminate)))
      (apply-to-sequence-iterators writer function readers))))

(seal-domain #'map-into '(list t))

(replicate-for-each-relevant-vectoroid #1=#:vectoroid
  (defmethod map-into ((vector #1#) function &rest sequences)
    (when (adjustable-array-p vector)
      (setf (fill-pointer vector)
            (array-total-size vector)))
    (flet ((terminate (n)
             (when (adjustable-array-p vector)
               (setf (fill-pointer vector) n))
             (return-from map-into vector)))
      (let* ((n (length sequences))
             (function (function-designator-function function))
             (writer (make-sequence-writer vector 0 nil nil #'terminate))
             (readers (make-array n)))
        (loop for index below n and sequence in sequences do
          (setf (aref readers index)
                (make-sequence-reader sequence 0 nil nil #'terminate)))
        (apply-to-sequence-iterators writer function readers)))))

(seal-domain #'map-into '(vector t))

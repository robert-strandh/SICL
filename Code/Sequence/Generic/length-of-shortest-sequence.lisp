(cl:in-package #:sicl-sequence)

;;; This function could have been written as
;;;
;;; (reduce #'min more-sequences :key #'length :initial-value (length sequence))
;;;
;;; The reason why we don't do so is that LENGTH is not an O(1) operation
;;; for lists.  So for the case where this function is called with at least
;;; one short sequence and at least one much longer list, we would perform
;;; a lot of unnecessary work.
;;;
;;; The solution is to iterate over all lists simultaneously, until either
;;; the shortest list is exhausted, or we have reached the length of the
;;; shortest non-list.
(defun length-of-shortest-sequence (sequence &rest more-sequences)
  (if (null more-sequences)
      (length sequence)
      (let ((list-readers '())
            (min most-positive-fixnum))
        (labels ((minimize (n)
                   (when (< n min) (setf min n)))
                 (terminate (n)
                   (minimize n)
                   (return-from length-of-shortest-sequence min))
                 (register-sequence (sequence)
                   (if (listp sequence)
                       (push (make-sequence-reader sequence 0 nil nil #'terminate) list-readers)
                       (minimize (length sequence)))))
          (register-sequence sequence)
          (mapc #'register-sequence more-sequences)
          (loop repeat min do (mapc #'funcall list-readers)
                finally (return min))))))

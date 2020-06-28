(cl:in-package #:sicl-sequence)

(defmethod replace ((list1 list) (list2 list) &key (start1 0) end1 (start2 0) end2)
  (declare (method-properties inlineable))
  ;; We have to be careful to correctly handle the cases where LIST1 and
  ;; LIST2 are identical.  Depending on the relative order of the source
  ;; and the target range, it may be necessary to copy the source elements.
  (flet ((simple-replace ()
           (with-cons-iterator (iter1 list1 start1 end1)
             (with-cons-iterator (iter2 list2 start2 end2)
               (loop
                 (multiple-value-bind (more1 cons1) (iter1)
                   (when (null more1) (return))
                   (multiple-value-bind (more2 cons2) (iter2)
                     (when (null more2) (return))
                     (setf (car cons1) (car cons2)))))
               list1)))
         (copying-replace ()
           (with-cons-iterator (iterator list1 start1 end1)
             (let ((values (subseq list2 start2 end2)))
               (loop
                 (multiple-value-bind (more cons) (iterator)
                   (if (or (not more)
                           (endp values))
                       (return)
                       (setf (car cons) (pop values)))))
               list1))))
    (if (eq list1 list2)
        (if (= start1 start2)
            list1
            (if (and (< start2 start1)
                     (or (null end2)
                         (< start1 end2)))
                (copying-replace)
                (simple-replace)))
        (simple-replace))))

(seal-domain #'replace '(list list))

(replicate-for-each-vector-class #1=#:vector-class
  (defmethod replace ((list list) (vector #1#) &key (start1 0) end1 (start2 0) end2)
    (declare (method-properties inlineable))
    (with-cons-iterator (iterator list start1 end1)
      (with-vector-start-and-end (start2 end2) (vector start2 end2)
        (loop for index from start2 below end2 do
          (multiple-value-bind (more cons) (iterator)
            (when (not more)
              (return))
            (setf (car cons)
                  (elt vector index))))
        list)))

  (defmethod replace ((vector #1#) (list list) &key (start1 0) end1 (start2 0) end2)
    (declare (method-properties inlineable))
    (with-vector-start-and-end (start1 end1) (vector start1 end1)
      (with-cons-iterator (iterator list start2 end2)
        (loop for index from start1 below end1 do
          (multiple-value-bind (more cons) (iterator)
            (when (not more) (return))
            (setf (elt vector index)
                  (car cons))))
        vector))))

(seal-domain #'replace '(list vector))

(seal-domain #'replace '(vector list))

(replicate-for-each-vector-class #1=#:vector-class
  (defmethod replace ((vector1 #1#) (vector2 #1#) &key (start1 0) end1 (start2 0) end2)
    (declare (method-properties inlineable))
    (with-vector-start-and-end (start1 end1) (vector1 start1 end1)
      (with-vector-start-and-end (start2 end2) (vector2 start2 end2)
        (let ((amount (min (- end1 start1) (- end2 start2))))
          (declare (vector-length amount))
          (flet ((replace-forward ()
                   (loop for offset fixnum below amount do
                     (setf (elt vector1 (+ start1 offset))
                           (elt vector2 (+ start2 offset))))
                   vector1)
                 (replace-backward ()
                   (loop for offset fixnum downfrom (1- amount) to 0 do
                     (setf (elt vector1 (+ start1 offset))
                           (elt vector2 (+ start2 offset))))
                   vector1))
            (if (eq vector1 vector2)
                (if (= start1 start2)
                    vector1
                    (if (< start2 start1 end2)
                        (replace-backward)
                        (replace-forward)))
                (replace-forward))))))))

(seal-domain #'replace '(vector vector))

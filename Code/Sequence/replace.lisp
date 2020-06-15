(cl:in-package #:sicl-sequence)

(defmethod replace ((list1 list) (list2 list) &key (start1 0) end1 (start2 0) end2)
  (declare (method-properties inlineable))
  (multiple-value-bind (start1 end1)
      (canonicalize-start-and-end list1 start1 end1)
    (multiple-value-bind (start2 end2)
        (canonicalize-start-and-end list2 start2 end2)
      ;; We have to be careful to correctly handle the cases where LIST1
      ;; and LIST2 are identical.  Depending on the relative order of the
      ;; source and the target range, it may be necessary to copy the
      ;; source elements.
      (multiple-value-bind (rest1 rest2)
          (if (eq list1 list2)
              (cond ((= start1 start2)
                     (return-from replace list1))
                    ((> end2 start1 start2)
                     (let ((tmp (nthcdr start2 list1)))
                       (values
                        (nthcdr (- start1 start2) tmp)
                        (subseq tmp 0 (- end2 start2)))))
                    ((< start1 start2)
                     (let ((tmp (nthcdr start1 list1)))
                       (values
                        tmp
                        (nthcdr (- start2 start1) tmp)))))
              (values
               (nthcdr start1 list1)
               (nthcdr start2 list2)))
        (do ((limit (min (- end1 start1) (- end2 start2)))
             (counter 0 (1+ counter))
             (cons1 rest1 (cdr cons1))
             (cons2 rest2 (cdr cons2)))
            ((= counter limit) list1)
          (declare (list-length counter limit))
          (setf (car cons1) (car cons2)))))))

(seal-domain #'replace '(list list))

(replicate-for-each-vector-class #1=#:vector-class
  (defmethod replace ((list list) (vector #1#) &key (start1 0) end1 (start2 0) end2)
    (declare (method-properties inlineable))
    (multiple-value-bind (start1 end1)
        (canonicalize-start-and-end list start1 end1)
      (multiple-value-bind (start2 end2)
          (canonicalize-start-and-end vector start2 end2)
        (do ((limit (min (- end1 start1) (- end2 start2)))
             (counter 0 (1+ counter))
             (cons (nthcdr start1 list) (cdr cons)))
            ((= counter limit) list)
          (declare (list-length counter limit))
          (setf (car cons) (elt vector (+ start2 counter)))))))

  (defmethod replace ((vector #1#) (list list) &key (start1 0) end1 (start2 0) end2)
    (declare (method-properties inlineable))
    (multiple-value-bind (start1 end1)
        (canonicalize-start-and-end vector start1 end1)
      (multiple-value-bind (start2 end2)
          (canonicalize-start-and-end list start2 end2)
        (do ((limit (min (- end1 start1) (- end2 start2)))
             (counter 0 (1+ counter))
             (cons (nthcdr start2 list) (cdr cons)))
            ((= counter limit) vector)
          (declare (type (and list-length vector-length) counter limit))
          (setf (elt vector (+ start1 counter)) (car cons)))))))

(seal-domain #'replace '(list vector))

(seal-domain #'replace '(vector list))

(replicate-for-each-vector-class #1=#:vector-class
  (defmethod replace ((vector1 #1#) (vector2 #1#) &key (start1 0) end1 (start2 0) end2)
    (declare (method-properties inlineable))
    (multiple-value-bind (start1 end1)
        (canonicalize-start-and-end vector1 start1 end1)
      (multiple-value-bind (start2 end2)
          (canonicalize-start-and-end vector2 start2 end2)
        (if (and (eq vector1 vector2)
                 (if (= start1 start2) (return-from replace vector1) t)
                 (> end2 start1 start2))
            ;; When we are reading and writing from the same array, and
            ;; when the destination precedes the source, we have to be
            ;; careful not to overwrite values that will be read later.  We
            ;; achieve this by copying from end to start.
            (if (= start1 start2)
                ;; If the source and target vectors and the designated
                ;; ranges are identical, we don't need to copy at all.
                vector1
                (do ((counter (min (- end1 start1) (- end2 start2)) (1- counter)))
                    ((zerop counter) vector1)
                  (declare (vector-length counter))
                  (setf (elt vector1 (+ start1 counter -1))
                        (elt vector1 (+ start2 counter -1)))))
            ;; If we are dealing with distinct vectors, or if the source
            ;; precedes the destination, we copy from start to end.
            (do ((limit (min (- end1 start1) (- end2 start2)))
                 (counter 0 (1+ counter)))
                ((= counter limit) vector1)
              (declare (vector-length counter limit))
              (setf (elt vector1 (+ start1 counter))
                    (elt vector2 (+ start2 counter)))))))))

(seal-domain #'replace '(vector vector))

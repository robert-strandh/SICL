(cl:in-package #:sicl-sequence)

(defmethod search ((pattern list) (text list)
                   &key from-end test test-not key (start1 0) (start2 0) end1 end2)
  (with-list-start-and-end (start1 end1) (pattern start1 end1)
    (with-list-start-and-end (start2 end2) (text start2 end2)
      (with-test-function (test test test-not)
        (with-key-function (key key)
          (let* ((m (- end2 start2))
                 (n (- end1 start1))
                 (pattern-rest (nthcdr start1 pattern)))
            (cond ((zerop n)
                   (if from-end end2 start2))
                  ((< m n) nil)
                  (t
                   (flet ((matchp (text-rest)
                            (loop repeat n
                                  for a in pattern-rest
                                  for b in text-rest
                                  always (test (key a) (key b)))))
                     (for-each-relevant-cons (cons position text start2 (- end2 (1- n)) from-end)
                       (when (matchp cons)
                         (return position))))))))))))

(replicate-for-each-vector-class #1=#:vector-class
  (defmethod search ((pattern #1#) (text list)
                     &key from-end test test-not key (start1 0) (start2 0) end1 end2)
    (with-vector-start-and-end (start1 end1) (pattern start1 end1)
      (with-list-start-and-end (start2 end2) (text start2 end2)
        (with-test-function (test test test-not)
          (with-key-function (key key)
            (let* ((m (- end2 start2))
                   (n (- end1 start1)))
              (cond ((zerop n)
                     (if from-end end2 start2))
                    ((< m n) nil)
                    (t
                     (flet ((matchp (text-rest)
                              (loop for offset below n
                                    for a = (elt pattern (+ start1 offset))
                                    for b in text-rest
                                    always (test (key a) (key b)))))
                       (for-each-relevant-cons (cons position text start2 (- end2 (1- n)) from-end)
                         (when (matchp cons)
                           (return position)))))))))))))

(replicate-for-each-vector-class #1=#:vector-class
  (defmethod search ((pattern list) (text #1#)
                     &key from-end test test-not key (start1 0) (start2 0) end1 end2)
    (with-list-start-and-end (start1 end1) (pattern start1 end1)
      (with-vector-start-and-end (start2 end2) (text start2 end2)
        (with-test-function (test test test-not)
          (with-key-function (key key)
            (let* ((n (- end1 start1))
                   (pattern-rest (nthcdr start1 pattern)))
              (flet ((matchp (position)
                       (loop for offset below n
                             for a in pattern-rest
                             for b = (elt text (+ position offset))
                             always (test (key a) (key b)))))
                (if (not from-end)
                    (loop for position from start2 to (- end2 n)
                          when (matchp position) return position)
                    (loop for position downfrom (- end2 n) to start2
                          when (matchp position) return position))))))))))

(replicate-for-each-vector-class #1=#:vector-class
  (defmethod search ((pattern #1#) (text #1#)
                     &key from-end test test-not key (start1 0) (start2 0) end1 end2)
    (with-vector-start-and-end (pattern-start pattern-end) (pattern start1 end1)
      (with-vector-start-and-end (text-start text-end) (text start2 end2)
        (with-test-function (test test test-not)
          (with-key-function (key key)
            (let ((n (- pattern-end pattern-start)))
              (flet ((matchp (position)
                       (loop for offset below n
                             for a = (elt pattern (+ pattern-start offset))
                             for b = (elt text (+ position offset))
                             always (test (key a) (key b)))))
                (if (not from-end)
                    (loop for position from text-start to (- text-end n)
                          when (matchp position) return position)
                    (loop for position downfrom (- text-end n) to text-start
                          when (matchp position) return position))))))))))

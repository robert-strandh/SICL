(cl:in-package #:sicl-sequence)

(defmethod replace ((datum t) sequence &key &allow-other-keys)
  (error 'must-be-sequence
         :datum datum))

(defmethod replace (sequence (datum t) &key &allow-other-keys)
  (error 'must-be-sequence
         :datum datum))

(defmethod replace ((list1 list) (list2 list) &key (start1 0) end1 (start2 0) end2)
  (multiple-value-bind (rest1 start1 end1)
      (canonicalize-list-interval list1 start1 end1)
    (multiple-value-bind (rest2 start2 end2)
        (canonicalize-list-interval list2 start2 end2)
      (if (and (null end1) (null end2))
          ;; Process any number of conses.
          (do ((rest1 rest1 (cdr rest1))
               (rest2 rest2 (cdr rest2)))
              ((or (atom rest1) (atom rest2))
               (when (and (atom rest1) (not (null rest1)))
                 (error 'must-be-proper-list
                        :datum list1))
               (when (and (atom rest2) (not (null rest2)))
                 (error 'must-be-proper-list
                        :datum list2)))
            (setf (car rest1) (car rest2)))
          ;; Process a fixed number of conses.
          (let ((n (cond ((null end1) (- end2 start2))
                         ((null end2) (- end1 start1))
                         (t (min (- end1 start1) (- end2 start2))))))
            (do ((rest1 rest1 (cdr rest1))
                 (rest2 rest2 (cdr rest2))
                 (offset 0 (1+ offset)))
                ((or (atom rest1)
                     (atom rest2)
                     (= offset n))
                 (unless (= offset n)
                   (cond ((and (atom rest1) (not (null rest1)))
                          (error 'must-be-proper-list
                                 :datum list1))
                         ((and (atom rest2) (not (null rest2)))
                          (error 'must-be-proper-list
                                 :datum list2))
                         ((null rest1)
                          (error 'invalid-end-index
                                 :in-sequence list1
                                 :expected-type `(integer ,start1 (,(+ start1 offset)))
                                 :datum (+ start1 offset)))
                         ((null rest2)
                          (error 'invalid-end-index
                                 :in-sequence list2
                                 :expected-type `(integer ,start2 (,(+ start2 offset)))
                                 :datum (+ start2 offset))))))
              (setf (car rest1) (car rest2)))))))
  list1)

;;; TODO replace on lists.

(replicate-for-all-compatible-vectoroids #1=#:vectoroid-1 #2=#:vectoroid-2
  (defmethod replace ((vectoroid-1 #1#) (vectoroid-2 #2#) &key start1 end1 start2 end2)
    (let ((length-1 (length vectoroid-1))
          (length-2 (length vectoroid-2)))
      (multiple-value-bind (start1 end1)
          (canonicalize-start-and-end vectoroid-1 length-1 start1 end1)
        (multiple-value-bind (start2 end2)
            (canonicalize-start-and-end vectoroid-1 length-2 start2 end2)
          (let ((n (min (- end1 start1) (- end2 start2))))
            (loop for offset below n do
              (setf (elt vectoroid-1 (+ start1 offset))
                    (elt vectoroid-2 (+ start2 offset))))))))
    vectoroid-1))

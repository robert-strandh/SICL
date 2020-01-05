(cl:in-package #:sicl-sequence)

(defmethod replace ((datum t) sequence &key &allow-other-keys)
  (error 'must-be-sequence
         :datum datum))

(defmethod replace (sequence (datum t) &key &allow-other-keys)
  (error 'must-be-sequence
         :datum datum))

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

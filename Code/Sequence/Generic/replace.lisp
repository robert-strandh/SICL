(cl:in-package #:sicl-sequence)

(defmethod replace ((list1 list) (list2 list) &key (start1 0) end1 (start2 0) end2)
  (multiple-value-bind (start1 end1)
      (canonicalize-start-and-end list1 start1 end1)
    (multiple-value-bind (start2 end2)
        (canonicalize-start-and-end list2 start2 end2)
      (loop repeat (min (- end1 start1) (- end2 start2))
            for cons1 on (nthcdr start1 list1)
            for cons2 on (nthcdr start2 list2)
            do (setf (car cons1) (car cons2)))
      list1)))

(seal-domain #'replace '(list list))

(replicate-for-each-vector-class #1=#:vector-class
  (defmethod replace ((list list) (vector #1#) &key (start1 0) end1 (start2 0) end2)
    (multiple-value-bind (start1 end1)
        (canonicalize-start-and-end list start1 end1)
      (multiple-value-bind (start2 end2)
          (canonicalize-start-and-end vector start2 end2)
        (loop repeat (min (- end1 start1) (- end2 start2))
              for index from start2
              for cons on (nthcdr start1 list)
              do (setf (car cons) (elt vector index)))
        list)))

  (defmethod replace ((vector #1#) (list list) &key (start1 0) end1 (start2 0) end2)
    (multiple-value-bind (start1 end1)
        (canonicalize-start-and-end vector start1 end1)
      (multiple-value-bind (start2 end2)
          (canonicalize-start-and-end list start2 end2)
        (loop repeat (min (- end1 start1) (- end2 start2))
              for index from start1
              for cons on (nthcdr start2 list)
              do (setf (elt vector index) (car cons)))
        vector))))

(seal-domain #'replace '(list vector))

(seal-domain #'replace '(vector list))

(replicate-for-all-compatible-vector-classes #1=#:vector-class-1 #2=#:vector-class-2
  (defmethod replace ((vector1 #1#) (vector2 #2#) &key (start1 0) end1 (start2 0) end2)
    (multiple-value-bind (start1 end1)
        (canonicalize-start-and-end vector1 start1 end1)
      (multiple-value-bind (start2 end2)
          (canonicalize-start-and-end vector2 start2 end2)
        (loop repeat (min (- end1 start1) (- end2 start2))
              for index1 from start1
              for index2 from start2
              do (setf (elt vector1 index1) (elt vector2 index2)))
        vector1))))

(seal-domain #'replace '(vector vector))

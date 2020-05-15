(cl:in-package #:sicl-sequence)

(defmethod subseq ((list list) start &optional end)
  (multiple-value-bind (start end)
      (canonicalize-start-and-end list start end)
    (loop repeat (- end start)
          for cons on (nthcdr start list)
          collect (car cons))))

(replicate-for-each-vector-class #1=#:vector-class
  (defmethod subseq ((vector #1#) start &optional end)
    (multiple-value-bind (start end)
        (canonicalize-start-and-end vector start end)
      (replace
       (make-sequence-like vector (- end start))
       vector :start2 start :end2 end))))

(defsetf subseq (sequence start &optional end) (value)
  `(progn (replace ,sequence ,value :start1 ,start :end1 ,end) ,value))


(cl:in-package #:sicl-sequence)

(defmethod subseq ((list list) start &optional end)
  (with-list-start-and-end (start end) (list start end)
    (loop repeat (- end start)
          for cons on (nthcdr start list)
          collect (car cons))))

(replicate-for-each-vector-class #1=#:vector-class
  (defmethod subseq ((vector #1#) start &optional end)
    (with-vector-start-and-end (start end) (vector start end)
      (replace
       (make-sequence-like vector (- end start))
       vector :start2 start :end2 end))))

;; The spec gives this definition of (setf subseq) as an example of using
;; the long form of defsetf.
(defsetf subseq (sequence-1 start &optional end) (sequence-2)
  `(progn (replace ,sequence-1 ,sequence-2 :start1 ,start :end1 ,end)
          ,sequence-2))


(cl:in-package #:sicl-sequence)

(defmethod subseq ((list list) start &optional end)
  (sicl-utilities:with-collectors ((result collect))
    (with-cons-iterator (iterator list start end)
      (loop
        (multiple-value-bind (more cons) (iterator)
          (if (not more)
              (return-from subseq (result))
              (collect (car cons))))))))

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

(seal-domain #'subseq '(list t))
(seal-domain #'subseq '(vector t))

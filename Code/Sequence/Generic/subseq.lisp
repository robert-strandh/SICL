(cl:in-package #:sicl-sequence)

(defmethod subseq ((list list) start &optional end)
  (sicl-utilities:with-collectors ((result collect))
    (let ((read (make-sequence-reader
                 list start end nil
                 (lambda (n)
                   (declare (ignore n))
                   (return-from subseq (result))))))
      (loop (collect (funcall read))))))

(replicate-for-each-vector-class #1=#:vector-class
  (defmethod subseq ((vector #1#) start &optional end)
    (multiple-value-bind (start end)
        (canonicalize-start-and-end vector start end)
      (replace
       (make-sequence-like vector (- end start))
       vector :start2 start :end2 end))))

(defsetf subseq (sequence start &optional end) (value)
  `(progn (replace ,sequence ,value :start1 ,start :end1 ,end) ,value))


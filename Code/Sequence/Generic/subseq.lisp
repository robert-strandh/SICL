(cl:in-package #:sicl-sequence)

(defmethod subseq ((datum t) start &optional end)
  (declare (ignore start end))
  (error 'must-be-sequence
         :datum datum))

(defmethod subseq ((list list) start &optional end)
  (sicl-utilities:with-collectors ((result collect))
    (let ((read (make-sequence-reader
                 list start end nil
                 (lambda () (return-from subseq (result))))))
      (loop (collect (funcall read))))))

(replicate-for-each-relevant-vectoroid #1=#:vectoroid
  (defmethod subseq ((vector #1#) start &optional end)
    (multiple-value-bind (start end)
        (canonicalize-start-and-end vector (length vector) start end)
      (replace
       (make-sequence-like vector (- end start))
       vector :start2 start :end2 end))))

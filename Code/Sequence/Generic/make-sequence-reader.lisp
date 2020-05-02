(cl:in-package #:sicl-sequence)

(defmethod make-sequence-reader ((list list) start end from-end terminate)
  (declare (method-properties inlineable))
  (declare (function terminate))
  (if (not from-end)
      ;; Forward iteration.
      (multiple-value-bind (start limit)
          (canonicalize-start-and-end list (1- array-total-size-limit) start end)
        (let ((rest (skip-to-start list start))
              (index start))
          (declare (array-index index))
          (lambda ()
            (cond ((= index limit)
                   (funcall terminate (- index start)))
                  ((endp rest)
                   (unless (null end)
                     (error 'invalid-end-index
                            :datum index
                            :in-sequence list
                            :expected-type `(integer ,start ,(1- index))))
                   (funcall terminate (- index start)))
                  (t
                   (let ((v (first rest))
                         (i index))
                     (setf rest (cdr rest))
                     (incf index)
                     (values v i)))))))
      ;; Backward iteration.
      (let* ((rest (nreverse (subseq list start end)))
             (index (length rest)))
        (declare (array-length index))
        (lambda ()
          (if (= index start)
              (funcall terminate (- end index))
              (let ((v (first rest))
                       (i (decf index)))
                   (setf rest (cdr rest))
                   (values v i)))))))

(seal-domain #'make-sequence-reader '(list t t t t))

(replicate-for-each-relevant-vectoroid #1=#:vectoroid
  (defmethod make-sequence-reader ((vector #1#) start end from-end terminate)
    (declare (method-properties inlineable))
    (declare (function terminate))
    (multiple-value-bind (start end)
        (canonicalize-start-and-end vector (length vector) start end)
      (declare (optimize (speed 3) (safety 0)))
      (if (not from-end)
          ;; Forward iteration.
          (let ((index start))
            (declare (array-length index))
            (lambda ()
              (cond ((= index end)
                     (funcall terminate (- index start)))
                    (t
                     (let ((v (elt vector index))
                           (i index))
                       (incf index)
                       (values v i))))))
          ;; Backward iteration.
          (let ((index end))
            (declare (array-length index))
            (lambda ()
              (cond ((= index start)
                     (funcall terminate (- end index)))
                    (t
                     (let ((v (elt vector index))
                           (i (decf index)))
                       (values v i))))))))))

(seal-domain #'make-sequence-reader '(vector t t t t))

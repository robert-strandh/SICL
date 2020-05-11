(cl:in-package #:sicl-sequence)

(defmethod make-sequence-writer ((list list) start end from-end terminate)
  (declare (method-properties inlineable))
  (declare (function terminate))
  (if (not from-end)
      ;; Forward iteration.
      (multiple-value-bind (start end length)
          (canonicalize-start-and-end list start end)
        (let ((rest (skip-to-start list start))
              (index start))
          (declare (array-index index))
          (lambda (new-value)
            (cond ((= index end)
                   (funcall terminate (- index start)))
                  ((endp rest)
                   (unless (null end)
                     (error 'invalid-end-index
                            :datum index
                            :in-sequence list
                            :expected-type `(integer ,start ,(1- length))))
                   (funcall terminate (- index start)))
                  (t
                   (let ((v (shiftf (first rest) new-value))
                         (i index))
                     (pop rest)
                     (incf index)
                     (values v i)))))))
      ;; Backward iteration.
      (let* ((rest (nreverse (subseq list start end)))
             (end (+ start (length rest)))
             (index end))
        (declare (array-length end index))
        (lambda (new-value)
          (if (= index start)
              (funcall terminate (- end start))
              (let ((v (shiftf (first rest) new-value))
                    (i (decf index)))
                (pop rest)
                (values v i)))))))

(seal-domain #'make-sequence-writer '(list t t t t))

(replicate-for-each-vector-class #1=#:vector-class
  (defmethod make-sequence-writer ((vector #1#) start end from-end terminate)
    (declare (method-properties inlineable))
    (declare (function terminate))
    (declare (type #1# vector))
    (multiple-value-bind (start end)
        (canonicalize-start-and-end vector start end)
      (declare (optimize (speed 3) (safety 0)))
      (if (not from-end)
          ;; Forward iteration.
          (let ((index start))
            (declare (array-length index))
            (lambda (new-value)
              (cond ((= index end)
                     (funcall terminate (- index start)))
                    (t
                     (let ((v (shiftf (elt vector index) new-value))
                           (i index))
                       (incf index)
                       (values v i))))))
          ;; Backward iteration.
          (let ((index end))
            (declare (array-length index))
            (lambda (new-value)
              (cond ((= index start)
                     (funcall terminate (- end index)))
                    (t
                     (values (shiftf (elt vector (decf index)) new-value) index)))))))))

(seal-domain #'make-sequence-writer '(vector t t t t))

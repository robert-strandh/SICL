(cl:in-package #:sicl-sequence)

(defmethod make-sequence-writer ((list list) start end from-end terminate)
  (declare (function terminate))
  (if (not from-end)
      ;; Forward iteration.
      (multiple-value-bind (start limit)
          (canonicalize-start-and-end list (1- array-total-size-limit) start end)
        (let ((rest (skip-to-start list start))
              (index start))
          (declare (array-index index))
          (lambda (new-value)
            (cond ((= index limit)
                   (funcall terminate (- index start)))
                  ((consp rest)
                   (let ((v (shiftf (first rest) new-value))
                         (i index))
                     (setf rest (cdr rest))
                     (incf index)
                     (values v i)))
                  ((not (null rest))
                   (error 'must-be-proper-list
                          :datum list))
                  ((null end)
                   (funcall terminate (- index start)))
                  (t
                   (error 'invalid-end-index
                          :datum index
                          :in-sequence list
                          :expected-type `(integer ,start ,(1- index))))))))
      ;; Backward iteration.
      (let* ((rest (cl:nreverse (cl:subseq list start end)))
             (index (length rest)))
        (declare (array-length index))
        (lambda (new-value)
          (cond ((= index start)
                 (funcall terminate (- end index)))
                ((consp rest)
                 (let ((v (shiftf (first rest) new-value))
                       (i (decf index)))
                   (setf rest (cdr rest))
                   (values v i)))
                ((not (null rest))
                 (error 'must-be-proper-list
                        :datum list))
                (t
                 (error 'invalid-end-index
                        :datum index
                        :in-sequence list
                        :expected-type `(integer ,start ,(1- index)))))))))

(replicate-for-each-relevant-vectoroid #1=#:vectoroid
  (defmethod make-sequence-writer ((vector #1#) start end from-end terminate)
    (declare (function terminate))
    (multiple-value-bind (start end)
        (canonicalize-start-and-end vector (length vector) start end)
      (declare (optimize (speed 3) (safety 0)))
      (if (not from-end)
          ;; Forward iteration.
          (let ((index start))
            (declare (array-length index))
            (lambda (new-value)
              (cond ((= index end)
                     (funcall terminate (- index start)))
                    (t
                     (let ((v (shiftf (aref vector index) new-value))
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
                     (let ((v (shiftf (aref vector index) new-value))
                           (i (decf index)))
                       (values v i))))))))))

(seal-domain #'make-sequence-writer '(sequence t t t t))

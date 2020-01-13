(cl:in-package #:sicl-sequence)

(declaim (inline make-list-writer))
(defun make-list-writer (list start end from-end terminate)
  (declare (list list))
  (if (not from-end)
      ;; Forward iteration.
      (multiple-value-bind (start limit)
          (canonicalize-start-and-end list (1- array-total-size-limit) start end)
        (let ((rest (skip-to-start list start))
              (index start))
          (declare (array-index index))
          (lambda (new-value)
            (cond ((= index limit)
                   (funcall terminate))
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
                   (funcall terminate))
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
                 (funcall terminate))
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

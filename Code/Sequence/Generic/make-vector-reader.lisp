(cl:in-package #:sicl-sequence)

(declaim (inline make-vector-reader))
(defun make-vector-reader (vector start end from-end terminate)
  (declare (vector vector))
  (multiple-value-bind (start end)
      (canonicalize-start-and-end vector (length vector) start end)
    (if (not from-end)
        ;; Forward iteration.
        (let ((index start))
          (declare (array-length index))
          (lambda ()
            (cond ((= index end)
                   (funcall terminate))
                  (t
                   (let ((v (aref vector index))
                         (i index))
                     (incf index)
                     (values v i))))))
        ;; Backward iteration.
        (let ((index end))
          (declare (array-length index))
          (lambda ()
            (cond ((= index start)
                   (funcall terminate))
                  (t
                   (let ((v (aref vector index))
                         (i (decf index)))
                     (values v i)))))))))

(cl:in-package #:sicl-sequence)

(defmethod count-if-not (predicate (list list) &key from-end (start 0) end key)
  (declare (method-properties inlineable))
  (let ((count 0))
    (declare (list-length count))
    (with-predicate (predicate predicate)
      (with-key-function (key key)
        (for-each-relevant-cons (cons index list start end from-end)
          (unless (predicate (key (car cons)))
            (incf count)))))
    count))

(seal-domain #'count-if-not '(t list))

(replicate-for-each-vector-class #1=#:vector-class
  (defmethod count-if-not (predicate (vector #1#) &key from-end (start 0) end key)
    (declare (method-properties inlineable))
    (let ((count 0))
      (declare (vector-length count))
      (with-predicate (predicate predicate)
        (with-key-function (key key)
          (for-each-relevant-element (element index vector start end from-end)
            (unless (predicate (key element))
              (incf count)))))
      count)))

(seal-domain #'count-if-not '(t vector))

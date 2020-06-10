(cl:in-package #:sicl-sequence)

(defmethod position-if (predicate (list list) &key from-end (start 0) end key)
  (declare (method-properties inlineable))
  (with-predicate (predicate predicate)
    (with-key-function (key key)
      (for-each-relevant-cons (cons index list start end from-end)
        (let ((element (car cons)))
          (when (predicate (key element))
            (return-from position-if index)))))))

(seal-domain #'position-if '(t list))

(replicate-for-each-vector-class #1=#:vector-class
  (defmethod position-if (predicate (vector #1#) &key from-end (start 0) end key)
    (declare (method-properties inlineable))
    (with-predicate (predicate predicate)
      (with-key-function (key key)
        (for-each-relevant-element (element index vector start end from-end)
          (when (predicate (key element))
            (return-from position-if index)))))))

(seal-domain #'position-if '(t vector))
